# catwalk-red.R - reduction of redundant variables by correlation analysis
# this script must be called from within catwalk.R or afterwards (uses pre-computed variables)

# varname contains the name of the dataset to reduce
stopifnot(is.character(varname))
stopifnot(class(get(varname)) == "data.frame")

cat("applying variable reduction to", varname, "...\n")

if (! exists("catwalk_red.plot.file")) catwalk_red.plot.file = "catwalk-red.pdf"
if (is.character(catwalk_red.plot.file)) pdf(file = catwalk_red.plot.file, title = catwalk_red.plot.file)

# aggregation of front-hind paws averages in addition to left-right within front/hind pairs
if (! exists("do.reduce.FH")) do.reduce.FH = FALSE
# remove variables that do not correlate between left/right paw (lateralization)
if (! exists("do.remove.unpaired")) do.remove.unpaired = TRUE

# correlation threshold - minimum value for which two variables are judged correlated
cor.threshold = 0.5

# working variable
catwalk.reduced = get(varname)

# start with predefined reduction rules
# remove useless initial attributes which encode environment conditions not mice measurements
drop.attr = c(
    #"Experiment", # important - contains date information !
    "Group",
    "Group_Type",
    "Group_Description",
    #"Animal",
    "Time_Point",
    "Time_Point_Description",
    "Trial",
    "Trial_Description",
    "Run",
    "Run_Description",
    "Run_Duration_(s)",
    "Run_Average_Speed_(cm/s)", # do oceny
    "Run_Maximum_Variation_(%)", # do oceny
    "Camera_Gain_(dB)",
    "Green_Intensity_Threshold",
    "Ceiling_Light_(V)",
    "Walkway_Light_(V)",
    "X-Unit_(mm/pixel)",
    "Y-Unit_(mm/pixel)",
    "WalkWay_Length_(cm)",
    "WalkWay_Width_(cm)"
)
# create index of irrelevant attributes
i.drop = which(colnames(catwalk.reduced) %in% drop.attr)
cat("removing irrelevant attributes :", colnames(catwalk.reduced)[i.drop], sep = "\n - ")
catwalk.reduced = subset(catwalk.reduced, select = -i.drop)

# additionally, remove attributes by patterns selected by AR
drop.attr.biased = c(
    # following attributes are influenced by the experimental procedure
    "Run_Average_Speed_.cm.s.",
    "Run_Maximum_Variation_...",
    ".._BodySpeedVariation_..._Mean",
    ".._BodySpeedVariation_..._Mean",
    "OtherStatistics_Duration_.s.",
    "OtherStatistics_Average_Speed_.cm.s.",
    "OtherStatistics_Maximum_Variation_..."
)
i.biased = unlist(sapply(drop.attr.biased, grep, colnames(catwalk.reduced))) # causes warnings !
cat("removing attributes influenced by experimenter :", colnames(catwalk.reduced)[i.biased], sep = "\n - ")
catwalk.reduced = subset(catwalk.reduced, select = -i.biased)

# remove columns only containing NA fields
#i.na = which(apply(is.na(catwalk.reduced), 2, all))
#cat("removing attributes with all values missing :", colnames(catwalk.reduced)[i.na], sep = "\n - ")
#catwalk.reduced = subset(catwalk.reduced, select = -i.na)

# remove columns with zero variance
#i.zero.var = which(sapply(catwalk.reduced, function(x) ifelse(is.numeric(x), var(x, na.rm = TRUE) == 0, FALSE)))
#cat("removing numeric attributes with zero variance :", colnames(catwalk.reduced)[i.zero.var], sep = "\n - ")
#catwalk.reduced = subset(catwalk.reduced, select = -i.zero.var)

# correlation of paw-related measurements
# what is the minimum correlation value between variables from all 4 paws ?
prefix = c("LF_", "RF_", "RH_", "LH_")
suffix.set = substring(colnames(catwalk.reduced)[startsWith(colnames(catwalk.reduced), prefix[1])], 4)
min.cor = sapply(suffix.set,
    function(s) min(cor(subset(catwalk.reduced, select = paste0(prefix, s)), use = "complete.obs")))
par(mar = c(15, 4, 2, 2) + 0.1)
plot(as.factor(labels(min.cor)), min.cor, las = 3,
    ylim = c(0, 1),
    main = "min correlations for all paws")
par(mar = c(5, 4, 2, 2) + 0.1)

prefix.f = c("LF_", "RF_")
cor.list.lf_rf = with(catwalk.reduced, sapply(suffix.set,
    function(s) cor(get(paste0(prefix.f[1], s)), get(paste0(prefix.f[2], s)), use = "complete.obs")
))

prefix.h = c("LH_", "RH_")
# same set of parameters for front and hind paws - use prior set for consistent order
cor.list.lh_rh = with(catwalk.reduced, sapply(suffix.set,
    function(s) cor(get(paste0(prefix.h[1], s)), get(paste0(prefix.h[2], s)), use = "complete.obs")
))

# plot correlations
par(mar = c(15, 4, 2, 2) + 0.1)
plot(as.factor(labels(cor.list.lf_rf)), cor.list.lf_rf, las = 3,
    ylim = c(0, 1),
    main = "correlations for left-right front paws")
abline(cor.threshold, 0, col = "red")
plot(as.factor(labels(cor.list.lh_rh)), cor.list.lh_rh, las = 3,
    ylim = c(0, 1),
    main = "correlations for left-right hind paws")
abline(cor.threshold, 0, col = "red")
par(mar = c(5, 4, 2, 2) + 0.1)
hist(cor.list.lf_rf, breaks = seq(0, 1, 0.1))
hist(cor.list.lh_rh, breaks = seq(0, 1, 0.1))

plot(cor.list.lh_rh, cor.list.lf_rf, xlim = c(0, 1), ylim = c(0, 1),
    main = "left-right correlations for front and hind paws")
rect(cor.threshold, cor.threshold, 2, 2, col = rgb(0, 1, 0, alpha = 0.25), border = NA)
abline(cor.threshold, 0, col = "red")
abline(v = cor.threshold, col = "red")

f.suffix.set = labels(cor.list.lf_rf)[cor.list.lf_rf > cor.threshold]
h.suffix.set = labels(cor.list.lh_rh)[cor.list.lh_rh > cor.threshold]
catwalk.f.lr = data.frame(rows = seq(nrow(catwalk.reduced)))
catwalk.h.lr = data.frame(rows = seq(nrow(catwalk.reduced)))
for (p.name in f.suffix.set)
{
    catwalk.f.lr[, p.name] = apply(catwalk.reduced[, paste0(prefix.f, p.name)], 1, sum)
}
for (p.name in h.suffix.set)
{
    catwalk.h.lr[, p.name] = apply(catwalk.reduced[, paste0(prefix.h, p.name)], 1, sum)
}
catwalk.f.lr = subset(catwalk.f.lr, select = f.suffix.set)
colnames(catwalk.f.lr) = paste0("F_", f.suffix.set)
catwalk.h.lr = subset(catwalk.h.lr, select = h.suffix.set)
colnames(catwalk.h.lr) = paste0("H_", h.suffix.set)

catwalk.lr = cbind(catwalk.f.lr, catwalk.h.lr)

fh.suffix.set = c()
catwalk.fh.lr = subset(catwalk.lr, select = c())
if (do.reduce.FH)
{
    # redefine suffix.set - select only variables for averaged within both front and hind paws
    fh.suffix.set = intersect(f.suffix.set, h.suffix.set)
    cor.list.f_h = with(catwalk.lr, sapply(fh.suffix.set,
        function(s) cor(get(paste0("F_", s)), get(paste0("H_", s)), use = "complete.obs")
    ))
    
    par(mar = c(15, 4, 2, 2) + 0.1)
    plot(as.factor(labels(cor.list.f_h)), sort(cor.list.f_h), las = 3,
        ylim = c(0, 1),
        main = "correlations for front-hind paws averages")
    abline(cor.threshold, 0, col = "red")
    par(mar = c(5, 4, 2, 2) + 0.1)
    hist(cor.list.f_h, breaks = seq(0, 1, 0.1))
    
    # apply correlation threshold again to select variables to be averaged across front/hind paws
    fh.suffix.set = labels(cor.list.f_h)[cor.list.f_h > cor.threshold]
    cor.catwalk.lr = cor(catwalk.lr, use = "complete.obs")
    Plot_Matrix(cor.catwalk.lr)
    
    catwalk.fh.lr = data.frame(rows = seq(nrow(catwalk.reduced)))
    prefix.fh = c("F_", "H_")
    for (p.name in fh.suffix.set)
    {
        catwalk.fh.lr[, p.name] = apply(catwalk.lr[, paste0(prefix.fh, p.name)], 1, sum)
    }
    catwalk.fh.lr = subset(catwalk.fh.lr, select = fh.suffix.set)
    
    cor.catwalk.fh.lr = cor(catwalk.fh.lr, use = "complete.obs")
    Plot_Matrix(cor.catwalk.fh.lr)
}

# reduce the data set by removing redundant variables
# list of redundant variables :
# f.suffix.set : correlated front paws
# names in catwalk.reduced : <?>F_<*>, <?> - L/R, <*> - suffix
# averages in catwalk.f.lr as F_<*>
# h.suffix.set : correlated hind paws
# names in catwalk.reduced : <?>H_<*>, <?> - L/R, <*> - suffix
# averages in catwalk.h.lr as H_<*>
# fh.suffix.set : correlated front/hind paws averages
# names in catwalk.lr : <?>_<*>
# averages in catwalk.fh.lr
# NOTE : fh.suffix.set is empty if do.reduce.FH = FALSE (now the default setting)

remove.set = c(as.vector(sapply(prefix.f, paste0, f.suffix.set)), as.vector(sapply(prefix.h, paste0, h.suffix.set)))
cat("removing paw-related variables with strong correlations :", remove.set, sep = "\n - ")
catwalk.reduced = catwalk.reduced[, ! colnames(catwalk.reduced) %in% remove.set]

# subset of aggregations only within front or hind paws, not across all paws
f.h.set = c(paste0("F_", f.suffix.set[! f.suffix.set %in% fh.suffix.set]),
    paste0("H_", h.suffix.set[! h.suffix.set %in% fh.suffix.set]))
cat("substituting averaged variables :", c(colnames(catwalk.fh.lr), f.h.set), sep = "\n + ")
catwalk.reduced = cbind(catwalk.reduced, catwalk.fh.lr, catwalk.lr[, f.h.set] )

# circular statistics
i.cs = grep("CStat", colnames(catwalk.reduced))
# remove
cat("removing circular statistics :", colnames(catwalk.reduced)[i.cs], sep = "\n - ")
catwalk.reduced = subset(catwalk.reduced, select = -i.cs)

# these variables must stay : (Anna 20.11.2020)
#Initial[X] = Terminal
#Single Stance = Swing [X]
#PhaseDispersions[X] = Couplings
required.suffix.set = c(
# removed as agreed on 1.07.2021
#    "TerminalDualStance_(s)_Mean",
#    "SingleStance_(s)_Mean"
)
# final cut - get rid of all individual paws
if (do.remove.unpaired)
{
    suffix.set.reduced = suffix.set[! suffix.set %in% required.suffix.set]
    i.paws = which(colnames(catwalk.reduced) %in% as.vector(sapply(prefix, paste0, suffix.set.reduced)))
    cat("removing remaining paw-related variables :", colnames(catwalk.reduced)[i.paws], sep = "\n - ")
    #catwalk.reduced = subset(catwalk.reduced, select = -i.paws)
    catwalk.reduced = catwalk.reduced[, -i.paws]
}

# should we remove Couplings_* ?
if (FALSE) # ML 23.02.2022 - fix to get reference results
{
    i.couplings = grep("Couplings", colnames(catwalk.reduced))
    cat("removing Couplings_* :", colnames(catwalk.reduced)[i.couplings], sep = "\n - ")
    catwalk.reduced = catwalk.reduced[, -i.couplings]
}

i.ph.disp = grep("PhaseDispersions", colnames(catwalk.reduced))
cat("removing PhaseDispersions_* :", colnames(catwalk.reduced)[i.ph.disp], sep = "\n - ")
catwalk.reduced = catwalk.reduced[, -i.ph.disp]

# clustering remaining variables by correlation
cat("clustering remaining variables by correlation\n")
i.num.reduced = which(as.vector(sapply(catwalk.reduced, is.numeric)))

cor.matrix.reduced = cor(catwalk.reduced[, i.num.reduced], use = "complete.obs")
col.dist.reduced = as.dist(m = Cor_Dist(cor.matrix.reduced))

hc.reduced = hclust(col.dist.reduced, method = "complete")
cut.hc = cutree(hc.reduced, h = sqrt(1 - cor.threshold **2))
tab.hc = table(as.factor(cut.hc))
lab.hc.clusters = labels(tab.hc)[[1]][tab.hc > 1]

plot(hc.reduced, labels = FALSE)
abline(sqrt(1 - cor.threshold **2), 0, col = "red")
cor.matrix.reordered = cor.matrix.reduced[hc.reduced$order, hc.reduced$order]
Plot_Matrix(cor.matrix.reordered)
#cor.matrix.thresh = cor.matrix.reordered[abs(cor.matrix.reordered) < 0.5]
#Plot_Matrix(cor.matrix.thresh)
dist.matrix.reordered = Cor_Dist(cor.matrix.reordered) > sqrt(1 - cor.threshold ** 2)
Plot_Matrix(dist.matrix.reordered)

clusters.list = lapply(lab.hc.clusters, function(lab) labels(cut.hc[cut.hc == lab]))

# report resulting grouping in a text file
sink("catwalk-var.txt")

cat("Variables correlated within front paw pair (f.suffix.set) :\n")
print(sort(f.suffix.set))
cat("Variables correlated within hind paw pair (h.suffix.set) :\n")
print(sort(h.suffix.set))
if (do.reduce.FH)
{
    cat("Variables correlated within front-hind averages (fh.suffix.set) :\n")
    print(sort(fh.suffix.set))
}
cat("Grouping by residual correlation :\n")
cat(length(clusters.list), "groups > 1,", sum(sapply(clusters.list, length)), "grouped variables\n")
print(clusters.list)

sink()

if (is.character(catwalk_red.plot.file)) dev.off()

cat("variable reduction complete -> catwalk.reduced\n")

if(do.view) system(paste("xdg-open", catwalk_red.plot.file))

