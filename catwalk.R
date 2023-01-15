# catwalk.R - Catwalk experiment
# workflow :
# catwalk -> catwalk.filtered -[catwalk-red.R]-> catwalk.reduced -> catwalk.clean -[catwalk-glm.R]-> catwalk.select

source("miceutils.R", local = TRUE)
#library(reshape2) # melt
#library(lattice) # levelplot

if (! exists("do.view")) do.view = FALSE
if (! exists("verbose")) verbose = getOption("verbose")
if (! exists("do.plot")) do.plot = TRUE
if (! exists("do.model")) do.model = TRUE

stopifnot(is.logical(do.view))
stopifnot(is.logical(verbose))
stopifnot(is.logical(do.plot))
stopifnot(is.logical(do.model))

legend.position = "topright"
palette.f = Palette
opacity = 0.5

experiment.start = "03.02.2019" # day zero, Sunday
tamoxifen.start = "04.02.2019" # = day 1
tamoxifen.end = "08.02.2019" # last day of tamoxifen
l.dopa.start = "6.05.2019" # for males +1 day re 6.05.2019 (implanted on Tuesday)
l.dopa.end = "20.05.2019"

tamoxifen.start.date = as.POSIXct(tamoxifen.start, format = "%d.%m.%Y", tz = "Europe/Warsaw")
tamoxifen.end.date = as.POSIXct(tamoxifen.end, format = "%d.%m.%Y", tz = "Europe/Warsaw")
l.dopa.start.date = as.POSIXct(l.dopa.start, format = "%d.%m.%Y", tz = "Europe/Warsaw")
l.dopa.end.date = as.POSIXct(l.dopa.end, format = "%d.%m.%Y", tz = "Europe/Warsaw")
zero.date = as.POSIXct("8.02.2019", format = "%d.%m.%Y", tz = "Europe/Warsaw")

catwalk.m = read.csv2("run_statistics_all_males.csv", as.is = TRUE, fileEncoding = "CP1250", check.names = FALSE)
catwalk.f = read.csv2("run_statistics_all_females.csv", as.is = TRUE, fileEncoding = "CP1250", check.names = FALSE)

# fix Animal identifiers
catwalk.m$Animal = as.factor(paste0(catwalk.m$Animal, "M"))
catwalk.f$Animal = as.factor(paste0(catwalk.f$Animal, "F"))

# sanity checks
stopifnot(catwalk.m$Group == Animal_Genotype(catwalk.m$Animal))
# stopifnot(catwalk.f$Group == Animal_Genotype(catwalk.f$Animal))
# fails for animal 380F ! -> see fix in miceutils.R

# combine males and females
catwalk = rbind(catwalk.m, catwalk.f)

# add Sex/Genotype/Treatment attributes using reference mapping -> miceutils.R
catwalk$Sex = as.factor(Animal_Sex(catwalk$Animal))
catwalk$Genotype = as.factor(Animal_Genotype(catwalk$Animal))
catwalk$Treatment = as.factor(Animal_Treatment(catwalk$Animal))

# manually create mapping from "Experiment" values to date
experiments = c(
    "20-05-2019 ANIA TIF-DAT MALES",
    "22-05-2019 ANIA TIF-DAT FEMALES",
    "25-04-2019 ANIA TIF_DAT FEMALES",
    "26-04-2019 ANIA TIF-DAT MALES",
    "4-06-2019 ANIA TIF-DAT FEMALES",
    "5-06-2019 ANIA TIF MALES",
    "8-04-2019 ANIA TIF-IA FEMALES",
    "8-05-2019 TIF-DAT FEMALES",
    "9-04-2019 ANIA TIF-IA MALES",
    "9-05-2019 ANIA TIF-DAT MALES",
    "Ania 2019-03-11 TIFDAT females",
    "Ania 2019-03-12 TIFDAT Males",
    "Ania 25-03-2019 Females",
    "Ania 26-03-2019 Males"
)
# check for completeness
stopifnot(all(catwalk$Experiment %in% experiments))

date.formats = c(
    "%d-%m-%Y ANIA TIF-DAT MALES",
    "%d-%m-%Y ANIA TIF-DAT FEMALES",
    "%d-%m-%Y ANIA TIF_DAT FEMALES",
    "%d-%m-%Y ANIA TIF MALES",
    "%d-%m-%Y ANIA TIF-IA FEMALES",
    "%d-%m-%Y ANIA TIF-IA MALES",
    "%d-%m-%Y TIF-DAT FEMALES",
    "Ania %Y-%m-%d TIFDAT females",
    "Ania %Y-%m-%d TIFDAT Males",
    "Ania %d-%m-%Y Females",
    "Ania %d-%m-%Y Males"
)
# extract dates from Experiment - try various rules to fit all forms in dataset :
catwalk$Date = as.POSIXct(NA)
for (f in date.formats)
{
    i = is.na(catwalk$Date)
    catwalk$Date[i] = as.POSIXct(catwalk$Experiment, format = f)[i]
}
# check if all dates were correctly matched
stopifnot(! any(is.na(catwalk$Date)))

# number of days since last day of tamoxifen
catwalk$Days = round(difftime(catwalk$Date, zero.date, units = "days"))

# index of (not yet) numeric attributes
i.num = (as.vector(sapply(catwalk, class)) == "character")
# transform measurements to numeric form - fix decimal separator problem in R
catwalk[, i.num] = apply(catwalk[, i.num], 2, function(x) sub(",", ".", x))
# change all remaining character attributes to numeric
catwalk[, i.num] = suppressWarnings(apply(catwalk[, i.num], 2, as.numeric)) # would issue warnings on NA's

# how many NA fields in columns ?
#na = apply(is.na(catwalk), 2, mean)
# some columns contain only fields with NA values
#sum(na == 1) # 4

# remove columns only containing NA fields
i.na = which(apply(is.na(catwalk), 2, all))
cat("removing attributes with all values missing :", colnames(catwalk)[i.na], sep = "\n - ")
catwalk = subset(catwalk, select = -i.na)

# remove columns with zero variance
i.zero.var = which(sapply(catwalk, function(x) ifelse(is.numeric(x), var(x, na.rm = TRUE) == 0, FALSE)))
cat("removing numeric attributes with zero variance :", colnames(catwalk)[i.zero.var], sep = "\n - ")
catwalk = subset(catwalk, select = -i.zero.var)

# reorder columns -- non-numeric attributes come first
i.num = which(as.vector(sapply(catwalk, is.numeric)))
catwalk = subset(catwalk, select = c(Animal, Sex, Genotype, Treatment, Date, Days, i.num))
# recreate index of numeric values
i.num = which(as.vector(sapply(catwalk, is.numeric)))

# missing values
missing = apply(is.na(catwalk), 1, sum)
cor.missing = apply(catwalk[, i.num], 2, cor, missing, use = "complete.obs")

# which variables are most correlated with presence of missing values ?
i.cor.missing = i.num[which(abs(cor.missing) > 0.5)]
cat("missing at random (MAR) - variables with strong (>0,5) correlation with row missingness\n")
print(cor.missing[colnames(catwalk)[i.cor.missing]])
#   StepSequence_NumberOfPatterns StepSequence_RegularityIndex_(%) 
#                      -0,6179555                       -0,7498028 
#   OtherStatistics_NumberOfSteps 
#                      -0,5602958 

# apply defined filters - remove rows containing the most of missing fields
cat("filtering the dataset...\n")
catwalk.filtered = catwalk
#catwalk.filtered = subset(catwalk.filtered, OtherStatistics_NumberOfSteps >= 10)
catwalk.filtered = subset(catwalk.filtered, StepSequence_NumberOfPatterns > 0)
catwalk.filtered = subset(catwalk.filtered, Date <= l.dopa.start.date)
cat("filtering complete,", nrow(catwalk) - nrow(catwalk.filtered), "records removed.\n")

# to see percentage of missing fields per attribute before and after filtering :
#cbind(apply(is.na(catwalk), 2, mean), apply(is.na(catwalk.filtered), 2, mean))

# correlation analysis and reduction of variables
varname = "catwalk.filtered"
source("catwalk-red.R", local = TRUE)
# result is in catwalk.reduced
i.num.reduced = which(as.vector(sapply(catwalk.reduced, is.numeric)))

# dataset cleanup
cat("cleaning up the data set...\n")

varname = "catwalk.reduced"
# apply hierarchic clustering to group correlated variables
source("catwalk-cor.R", local = TRUE)
i.num.varname = which(as.vector(sapply(get(varname), is.numeric)))
cor.matrix = cor(get(varname)[, i.num.varname], use = "complete.obs")

col.dist = as.dist(m = Cor_Dist(cor.matrix))
hc.single = hclust(col.dist, method = "single")
hc.average = hclust(col.dist, method = "average")
hc.complete = hclust(col.dist, method = "complete")
hc.centroid = hclust(col.dist, method = "centroid")
hc.ward = hclust(col.dist, method = "ward")
hc = hc.complete # selected clustering type
# select and reorder attributes
catwalk.clean = subset(get(varname), select = c(Animal, Sex, Genotype, Treatment, Date, Days, i.num[hc$order]))

# imputation - replace missing values with column means (needed for model predictions)
i.num.clean = which(as.vector(sapply(catwalk.clean, is.numeric)))
#catwalk.clean[, i.num.clean] = lapply(catwalk.clean[, i.num.clean], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
# equivalent, perhaps mode readable :
catwalk.clean[, i.num.clean] = sapply(catwalk.clean[, i.num.clean],
    function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

cat("cleanup complete.\n")

if (! exists("do.select")) do.select = TRUE
if (do.select)
{
    cat("selecting variables relevant to Genotype ...\n")
    catwalk.developed = subset(catwalk.clean, Date >= as.POSIXct("1.04.2019", format = "%d.%m.%Y", tz = "Europe/Warsaw"))
    sink("catwalk-glm.developed.txt")
    varname = "catwalk.developed"
    base.formula = Genotype ~ Sex
    source("catwalk-glm.R", local = TRUE)
    sink()

    sink("catwalk-glm.clean.int.txt")
    varname = "catwalk.clean"
    base.formula = Genotype ~ Sex * Days
    source("catwalk-glm.R", local = TRUE)
    sink()

    # the last call to catwalk-glm.R prevails - the calls above are for reference only
    # (see generated .txt files)
    sink("catwalk-glm.clean.txt")
    varname = "catwalk.clean"
    base.formula = Genotype ~ Sex + Days
    source("catwalk-glm.R", local = TRUE)
    sink()

    cat("variable selection complete -> catwalk.select\n")
    cat("selected attributes :", vars.select, sep = "\n * ")
}

if(do.model)
{
    cat("fitting models ...\n")

    sink("catwalk-models.txt")
    source("catwalk-models.R", local = TRUE)
    sink()

    cat("models done.\n")
} else cat("skipping models.\n")

if (do.plot)
{
    cat("drawing plots ...\n")
    source("catwalk-plots.R", local = TRUE)
    cat("plots done.\n")

    if(do.view) system("xdg-open catwalk.pdf")
} else cat("skipping plots.\n")

