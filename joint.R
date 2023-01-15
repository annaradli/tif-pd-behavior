# joint.R - joint analysis of selected features from all data sets

source("miceutils.R")

if (! exists("verbose")) verbose = FALSE

experiment.start = "03.02.2019" # day zero, Sunday
tamoxifen.start = "04.02.2019" # = day 1
tamoxifen.end = "08.02.2019" # last day of tamoxifen
l.dopa.start = "6.05.2019" # for males +1 day re 6.05.2019 (implanted on Tuesday)
l.dopa.end = "20.05.2019"
date.format = "%d.%m.%Y"

if (! exists("do.plot")) do.plot = TRUE # note : individual dataset scripts are invoked with do.plot = FALSE
if (! exists("do.view")) do.view = FALSE

uniform.bins = TRUE # set to FALSE for unequal bins spanning complete calendar weeks (Monday-Sunday)

# define time periods/bins for joint analysis
if(uniform.bins)
{
    Boundary = as.POSIXct("3.03.2019", format = date.format) + as.difftime(16, units = "days") * seq(0, 4)
} else
{
    Boundary = as.POSIXct("3.03.2019", format = date.format) + as.difftime(1, units = "weeks") * c(0, 2, 4, 6, 9)
}

# --- Catwalk ---

if (! exists("cor.threshold")) cor.threshold = 0.5
if (! exists("catwalk.select"))
{
    catwalk.select = local({do.model = do.plot = FALSE ; source("catwalk.R", local = TRUE) ; catwalk.select})
}
stopifnot(class(catwalk.select) == "data.frame")
stopifnot(c("Date", "Animal") %in% colnames(catwalk.select))
cat("dataset ready : catwalk.select\n")

# --- OSS ---

if (! exists("oss.clean"))
{
    oss.clean = local({do.model = do.plot = FALSE ; source("oss.R", local = TRUE) ; oss.clean})
}
stopifnot(class(oss.clean) == "data.frame")
stopifnot(c("Date", "Animal") %in% colnames(oss.clean))
cat("dataset ready : oss.clean\n")

# --- weight ---

if (! exists("weight"))
{
    weight = local({do.model = do.plot = FALSE ; source("weight.R", local = TRUE) ; weight})
}
stopifnot(class(weight) == "data.frame")
stopifnot(c("Animal", "Date") %in% colnames(weight))
cat("dataset ready : weight\n")

# --- olfactory ---

if (! exists("olfactory.clean"))
{
    olfactory.clean = local({do.model = do.plot = FALSE ; source("olfactory.R", local = TRUE) ; olfactory.clean})
}
stopifnot(class(olfactory.clean) == "data.frame")
stopifnot(c("Animal", "Date") %in% colnames(olfactory.clean))
olfactory.delta = within(olfactory.clean, {
    Take.delta = Take - Dig
    Bite.delta = Bite - Take
})
cat("dataset ready : olfactory.delta\n")

# --- saccharin ---

if (! exists("saccharin.clean"))
{
    saccharin.clean = local({do.model = do.plot = FALSE ; source("saccharin.R", local = TRUE) ; saccharin.clean})
}
stopifnot(class(saccharin.clean) == "data.frame")
stopifnot(c("Animal", "Start") %in% colnames(saccharin.clean))
cat("dataset ready : saccharin.clean\n")

# --- intelli ---

if (! exists("intelli.clean"))
{
    intelli.clean = local({do.model = do.plot = FALSE ; source("intelli.R", local = TRUE) ; intelli.clean})
}
stopifnot(class(intelli.clean) == "data.frame")
stopifnot(c("Animal", "StartDateTime") %in% colnames(intelli.clean))
cat("dataset ready : intelli.clean\n")

# --- select male subsets and adjust Animal ID factors ---

oss.m = subset(oss.clean, Sex == "male")
oss.m = within(oss.m, {
    Animal = droplevels(factor(Animal))
    Animal = factor(Animal, levels = sort(levels(Animal)))
    rm(Sex, Genotype, Treatment, Group)
})
catwalk.m = subset(catwalk.select, Sex == "male")
catwalk.m = within(catwalk.m, {
    Animal = droplevels(factor(Animal))
    Animal = factor(Animal, levels = sort(levels(Animal)))
    rm(Sex, Genotype, Treatment, Days)
})
weight.m = subset(weight, Sex == "male")
weight.m = within(weight.m, {
    Animal = factor(Animal, levels = sort(unique(Animal)))
    rm(Sex, Genotype, Treatment, Days)
})

# --- select female subsets and adjust Animal ID factors ---

catwalk.f = subset(catwalk.select, Sex == "female")
catwalk.f = within(catwalk.f, {
    Animal = droplevels(factor(Animal))
    Animal = factor(Animal, levels = sort(levels(Animal)))
    rm(Sex, Genotype, Treatment, Days)
})
weight.f = subset(weight, Sex == "female")
weight.f = within(weight.f, {
    #Animal = droplevels(factor(Animal))
    Animal = factor(Animal, levels = sort(unique(Animal)))
    rm(Sex, Genotype, Treatment, Days)
})

Aggregate_Bins = function(data, col.name, bin.nodes, factor.name = "Bin", cofactors = NULL)
{
    if (verbose) cat(format(match.call()), "\n")

    # cut variable denoted by col.name into bins encoded by integers (outside values get NA)
    bins = cut(data[, col.name], breaks = bin.nodes, labels = FALSE)
    # transform bin numbers to factor and add to the dataset
    data[, factor.name] = ordered(paste0(factor.name, bins),
        levels = paste0(factor.name, seq(length(bin.nodes) - 1)))
    # drop original quantized variable
    data = within(data, rm(list = col.name))
    # perform data aggregation by levels of Bin(Period):Animal by using function "mean"
    cofactors = append(cofactors, factor.name)
    formula.string = paste(". ~", paste(cofactors, collapse = ":"))
    if (verbose) cat("formula.string : ", format(formula.string), "\n")
    data.aggr = aggregate(formula(formula.string), data, mean, na.rm = TRUE, na.action = na.pass)
    data.aggr.tab = table(data[, cofactors], dnn = cofactors)
    i.missing = which(data.aggr.tab == 0, arr.ind = TRUE)
    missing.explicit = data.frame(i.missing)
# ? as.matrix :
#     When coercing a vector, it produces a one-column matrix, and
#     promotes the names (if any) of the vector to the rownames of the
#     matrix.
    missing.explicit = do.call(cbind, lapply(colnames(missing.explicit),
        function(name) dimnames(data.aggr.tab)[[name]][missing.explicit[, name]]))
    missing.explicit = as.data.frame(missing.explicit)
    # temporary hack : restore column names
    colnames(missing.explicit) = cofactors
    missing.explicit[, factor.name] = ordered(missing.explicit[, factor.name])
    empty = matrix(nrow = nrow(i.missing),
        ncol = ncol(data.aggr) - length(cofactors),
        dimnames = list(c(), tail(colnames(data.aggr), -length(cofactors))))
    missing.explicit = cbind(missing.explicit, empty)
    # fill in missing values by binding missing.explicit to the aggregated data
    data.aggr = rbind(data.aggr, missing.explicit)
    # set the rows in a deterministic order (sorting by cofactors) to facilitate merging tables later
    permut = do.call(order, lapply(data.aggr[, cofactors], function(x) x))
    data.aggr = data.aggr[permut, ]
    return (data.aggr)
}

# === MALES ===

cat("aggregating male datasets ...\n")

catwalk.m.aggr = Aggregate_Bins(catwalk.m, "Date", Boundary, factor.name = "Period", cofactors = "Animal")
oss.m.aggr = Aggregate_Bins(oss.m, "Date", as.POSIXct(Boundary), factor.name = "Period", cofactors = "Animal")
saccharin.aggr = Aggregate_Bins(saccharin.clean, "Start", Boundary, factor.name = "Period", cofactors = "Animal")
weight.m.aggr = Aggregate_Bins(weight.m, "Date", Boundary, factor.name = "Period", cofactors = "Animal")
# olfactory test data removed from joint analysis
#olfactory.aggr = Aggregate_Bins(olfactory.clean, "Date", Boundary, factor.name = "Period", cofactors = "Animal")

common.cols = c("Animal", "Period")
joint.data.m.merge = Reduce(function(df.a, df.b) merge(df.a, df.b, all = TRUE, by = common.cols),
    list(weight.m.aggr, saccharin.aggr, oss.m.aggr, catwalk.m.aggr))

joint.data.m = data.frame(
    subset(joint.data.m.merge, select = c(Animal, Period)),
    Genotype = as.factor(Animal_Genotype(joint.data.m.merge$Animal)),
    subset(joint.data.m.merge, select = -c(Animal, Period)),
    check.names = FALSE
)

# === FEMALES ===

cat("aggregating female datasets ...\n")

catwalk.f.aggr = Aggregate_Bins(catwalk.f, "Date", Boundary, factor.name = "Period", cofactors = "Animal")
#intelli.aggr = Aggregate_Bins(intelli.stats, "Date", Boundary, factor.name = "Period", cofactors = "Animal")
weight.f.aggr = Aggregate_Bins(weight.f, "Date", Boundary, factor.name = "Period", cofactors = "Animal")

# special procedure for intelli data
periods = paste0("Period", seq(length(Boundary) - 1))
intelli.clean$Period = cut(intelli.clean$StartDateTime, breaks = Boundary, labels = FALSE)
intelli.clean$Period = ordered(paste0("Period", intelli.clean$Period), levels = periods)
visits.long = with(intelli.clean, data.frame(
    table(Period, Animal, RP, Duration = ifelse(VisitDuration >= 2, "Long", "Short"))))
colnames(visits.long)[colnames(visits.long) == "Freq"] = "Visits"
visits.stats = reshape(as.data.frame(visits.long), direction = "wide",
    v.names = "Visits", timevar = "Duration", idvar = 1:3)
visits.stats = reshape(visits.stats, direction = "wide",
    v.names = c("Visits.Long", "Visits.Short"), timevar = "RP", idvar = 1:2)
licks.stats.long = aggregate(LicksNumber ~ Period + Animal + RP, data = intelli.clean, sum)
licks.stats.wide = reshape(licks.stats.long, direction = "wide", v.names = "LicksNumber", timevar = "RP", idvar = 1:2)
intelli.stats = merge(licks.stats.wide, visits.stats, by = c("Period", "Animal"))
intelli.stats[is.na(intelli.stats)] = 0
colnames(intelli.stats) = gsub(",", ".", colnames(intelli.stats))
# add summary statistics for ease of manipulation
intelli.stats = within(intelli.stats, {
    Visits.Long = Visits.Long.0 + Visits.Long.0.3 + Visits.Long.0.9
    Visits.Short = Visits.Short.0 + Visits.Short.0.3 + Visits.Short.0.9
    Choices.Long = Visits.Long - Visits.Long.0
    Choices.Short = Visits.Short - Visits.Short.0
    LicksNumber = LicksNumber.0 + LicksNumber.0.3 + LicksNumber.0.9
})
# expand to include all Period-Animal combinations
intelli.stats = merge(intelli.stats, with(intelli.stats,
    expand.grid(Period = unique(Period), Animal = unique(Animal))), all.y = TRUE, by = c("Animal", "Period"))
# replace missing values with zero
intelli.stats[is.na(intelli.stats)] = 0
stopifnot(! is.na(intelli.stats)) # check
# final aggregated statistics
intelli.aggr = with(intelli.stats, data.frame(
    Animal,
    Period,
    #Visits = Visits.Long + Visits.Short,
    Visits = (Visits.Long + Visits.Short) / 16,
    #Choices = Choices.Long + Choices.Short / 16,
    Choices = Choices.Long / 16,
    #Visits.90 = Visits.Long.0.9 + Visits.Short.0.9,
    `Pref.90/30` = (Visits.Long.0.9 + Visits.Short.0.9) / (Visits.Long.0.3 + Visits.Short.0.3 + Visits.Long.0.9 + Visits.Short.0.9),
    #Licks.total = LicksNumber.0 + LicksNumber.0.3 + LicksNumber.0.9,
    #Licks.water = LicksNumber.0,
    #Licks.sacch = LicksNumber.0.3 + LicksNumber.0.9,
    Pref.sacch = (LicksNumber.0.3 + LicksNumber.0.9) / LicksNumber,
    check.names = FALSE
    #Pref.P_0.9.licks = LicksNumber.0.9 / (LicksNumber.0.3 + LicksNumber.0.9),
    #Pref.P_0.9 = (Visits.Long.0.9 + Visits.Short.0.9) / (Choices.Long + Choices.Short),
    #Pref.P_0.9.2s = Visits.Long.0.9 / Choices.Long,
    #Visits.2s = Visits.Long,
    #Choices.2s = Choices.Long,
    #Pref.P_nz = (Choices.Long + Choices.Short) / (Visits.Long + Visits.Short),
    #Pref.P_nz.2s = Choices.Long / Visits.Long,
))

# remove values computed in Period1 - insufficient data available
is.na(intelli.aggr[intelli.aggr$Period == "Period1", setdiff(colnames(intelli.aggr), c("Animal", "Period"))]) = TRUE

# reassure all tables are sorted in the same way
#stopifnot(weight.f.aggr$Animal == intelli.aggr$Animal)
#stopifnot(intelli.aggr$Animal == catwalk.f.aggr$Animal)

#stopifnot(weight.f.aggr$Period == intelli.aggr$Period)
#stopifnot(intelli.aggr$Period == catwalk.f.aggr$Period)

common.cols = c("Animal", "Period")
joint.data.f.merge = Reduce(function(df.a, df.b) merge(df.a, df.b, all = TRUE, by = common.cols),
    list(weight.f.aggr, intelli.aggr, catwalk.f.aggr))

joint.data.f = data.frame(
    subset(joint.data.f.merge, select = c(Animal, Period)),
    Genotype = as.factor(Animal_Genotype(joint.data.f.merge$Animal)),
    subset(joint.data.f.merge, select = -c(Animal, Period)),
    check.names = FALSE
)

# final touch before joining tables - remove redundant columns
#intelli.aggr = within(intelli.aggr,
#{
#    rm(Animal, Period)
#})
#catwalk.f.aggr = within(catwalk.f.aggr,
#{
#    rm(Sex, Genotype, Treatment)
#    rm(Animal, Period)
#})
#weight.f.aggr = within(weight.f.aggr,
#{
#    Animal = factor(Animal, levels = sort(levels(Animal)))
#    rm(Sex, Genotype, Treatment, Days)
#    Genotype = Animal_Genotype(Animal)
#})

# joint.data.f.ref from merge() - now joint.data.f
#joint.data.f = cbind(weight.f.aggr, intelli.aggr, catwalk.f.aggr)
#stopifnot(all(subset(joint.data.f, select = -Genotype) == subset(joint.data.f.ref, select = -Genotype), na.rm = TRUE))
#stopifnot(sum(is.na(joint.data.f)) == sum(is.na(joint.data.f.ref)))

cat("aggregation done.\n")

sink("joint.txt")

i = which(as.vector(sapply(joint.data.m, is.numeric)))
cor.matrix.m = cor(joint.data.m[, i], use = "complete.obs")
var.dist.m = as.dist(m = Cor_Dist(cor.matrix.m))
hc.complete.m = hclust(var.dist.m, method = "complete")
cor.matrix.m.complete = cor.matrix.m[hc.complete.m$order, hc.complete.m$order]
dist.matrix.m.complete = Cor_Dist(cor.matrix.m.complete)

# recalculate correlation threshold (n = number of samples, m = number of correlations)
# 1/ (absolute) critical value of the T statistic for n - 2 degrees of freedom 
# Critical set is (-t_C, +t_C)
# Bonferroni correction for p : p = 0,025 / m
t_C.m = qt(0.025 / length(var.dist.m), df = nrow(joint.data.m) - 2, lower.tail = FALSE)
# 2/ r_C^{2} = \frac{t_C^{2}}{t_C^{2} + n - 2}
cor.threshold.m = sqrt(t_C.m ^ 2 / (t_C.m ^ 2 + nrow(joint.data.m) - 2))

# find strongest correlates
for (attr in setdiff(colnames(cor.matrix.m), colnames(catwalk.m.aggr)))
{
    cat("strongest correlations for", attr, ":\n")
    print(cor.matrix.m[attr, order(abs(cor.matrix.m[attr, ]), decreasing = TRUE)[2:7]])
}
i = which(abs(cor.matrix.m) >= cor.threshold.m, arr.ind = TRUE)
i = i[i[, 1] < i[, 2], ]
signif.cor.m = data.frame(
    R = cor.matrix.m[i],
    Attr1 = rownames(cor.matrix.m)[i[, "row"]],
    Attr2 = colnames(cor.matrix.m)[i[, "col"]])
write.csv2(signif.cor.m, "signif_cor_males.csv", row.names = FALSE)

# cut to obtain variable groupings
cut.hc = cutree(hc.complete.m, h = sqrt(1 - cor.threshold **2))
tab.hc = table(as.factor(cut.hc))
lab.hc.clusters = labels(tab.hc)[[1]][tab.hc > 1]
clusters.list = lapply(lab.hc.clusters, function(lab) labels(cut.hc[cut.hc == lab]))
cat(length(clusters.list), "groups > 1,", sum(sapply(clusters.list, length)), "grouped variables\n")
print(clusters.list)

# === FEMALES ===

i = which(as.vector(sapply(joint.data.f, is.numeric)))
cor.matrix.f = cor(joint.data.f[, i], use = "complete.obs")
var.dist.f = as.dist(m = Cor_Dist(cor.matrix.f))
hc.complete.f = hclust(var.dist.f, method = "complete")
cor.matrix.f.complete = cor.matrix.f[hc.complete.f$order, hc.complete.f$order]
dist.matrix.f.complete = Cor_Dist(cor.matrix.f.complete)

# recalculate correlation threshold (n = number of samples, m = number of correlations)
# 1/ (absolute) critical value of the T statistic for n - 2 degrees of freedom 
# Critical set is (-t_C, +t_C)
# Bonferroni correction for p : p = 0,025 / m
t_C.f = qt(0.025 / length(var.dist.f), df = nrow(joint.data.f) - 2, lower.tail = FALSE)
# 2/ r_C^{2} = \frac{t_C^{2}}{t_C^{2} + n - 2}
cor.threshold.f = sqrt(t_C.f ^ 2 / (t_C.f ^ 2 + nrow(joint.data.f) - 2))

# find strongest correlates
for (attr in setdiff(colnames(cor.matrix.f), colnames(catwalk.f.aggr)))
{
    cat("strongest FEMALE correlations for", attr, ":\n")
    print(cor.matrix.f[attr, order(abs(cor.matrix.f[attr, ]), decreasing = TRUE)[2:7]])
}
# store all strong correlations in a dedicated table
i = which(abs(cor.matrix.f) >= cor.threshold.f, arr.ind = TRUE)
i = i[i[, 1] < i[, 2], ]
signif.cor.f = data.frame(
    R = cor.matrix.f[i],
    Attr1 = rownames(cor.matrix.f)[i[, "row"]],
    Attr2 = colnames(cor.matrix.f)[i[, "col"]])
write.csv2(signif.cor.f, "signif_cor_females.csv", row.names = FALSE)

sink()

# extra code - export results in format useable in TikZ plotting
colnames(joint.data.m)

tab.name.list = list("weight.f.aggr", "intelli.aggr", "catwalk.f.aggr")
var.tab.f = do.call(rbind, lapply(tab.name.list, function(tab)
    data.frame(Var.no = NA, Set.no = tab, Name = colnames(get(tab)))))
var.tab.f = subset(var.tab.f, ! Name %in% c("Animal", "Genotype", "Period"))
rownames(var.tab.f) = var.tab.f$Name
var.tab.f = within(var.tab.f, {
    Var.no = seq(along.with = Name)
    Set.no = as.integer(Set.no)
    Name = paste0("{", Name, "}")
    Name = gsub("_", " ", Name, fixed = TRUE)
    Name = gsub("?", "2", Name, fixed = TRUE)
})

write.table(var.tab.f, "var.tab.f.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "/", eol = ",\n")

cor.tab.f = with(signif.cor.f, data.frame(
    A = var.tab.f[as.character(Attr1), "Var.no"],
    B = var.tab.f[as.character(Attr2), "Var.no"],
    R = R
))

write.table(cor.tab.f, "cor.tab.f.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "/", eol = ",\n")

tab.name.list = list("weight.m.aggr", "saccharin.aggr", "oss.m.aggr", "catwalk.m.aggr")
var.tab.m = do.call(rbind, lapply(tab.name.list, function(tab)
    data.frame(Var.no = NA, Set.no = tab, Name = colnames(get(tab)))))
var.tab.m = subset(var.tab.m, ! Name %in% c("Animal", "Genotype", "Period"))
rownames(var.tab.m) = var.tab.m$Name
var.tab.m = within(var.tab.m, {
    Var.no = seq(along.with = Name)
    Set.no = as.integer(Set.no)
    Name = paste0("{", Name, "}")
    Name = gsub("_", " ", Name, fixed = TRUE)
    Name = gsub("?", "2", Name, fixed = TRUE)
})

write.table(var.tab.m, "var.tab.m.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "/", eol = ",\n")

cor.tab.m = with(signif.cor.m, data.frame(
    A = var.tab.m[as.character(Attr1), "Var.no"],
    B = var.tab.m[as.character(Attr2), "Var.no"],
    R = R
))

write.table(cor.tab.m, "cor.tab.m.txt", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "/", eol = ",\n")

# --- plots ---

if (do.plot)
{
    cat("drawing plots ...\n")
    pdf(file = "joint.pdf")
    source("joint-plots.R")
    dev.off()
    cat("plots done.\n")

    cat("running post hoc analyses...\n")

    pdf(file = "joint-aov.males.pdf", title = "AOV post hoc tests - males")
    sink("joint-aov.males.txt")
    varname = "joint.data.m"
    source("joint-aov.R", encoding = "utf-8")
    sink()
    dev.off()

    pdf(file = "joint-aov.females.pdf", title = "AOV post hoc tests - males")
    sink("joint-aov.females.txt")
    varname = "joint.data.f"
    source("joint-aov.R", encoding = "utf-8")
    sink()
    dev.off()

    cat("post hoc done.\n")

    if(do.view) system("xdg-open joint.pdf")
}
