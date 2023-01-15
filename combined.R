# combined analysis of all data sets, optionally including AR's script for reference
# ML 23.12.2021

# set to TRUE to use AR reference code and verify identity of processed data tables
do.verify.ref = TRUE

start.date = as.POSIXct("2019-02-09")

# OSS
source("oss.R")
# general filtering
# all.long <- rbind(dane.long.2, OSS.long, waga.long, sacharyna.long, intelli.long, sunbites.long)
# all.long$Weeks <- as.numeric(difftime(all.long$Date,
#   as.POSIXct(strptime("4/2/2019 9:00","%d/%m/%Y %H:%M")), units = c("weeks")))
# OSS.all <- filter(all.long, Weeks <= 13, Parameter %in% c("OSS.Correct.Nosepokes",
#   "OSS.Incorrect.Nosepokes", "OSS.Rewards"))
# correct <- filter(OSS.all, Parameter == "OSS.Correct.Nosepokes")
# incorrect <- filter(OSS.all, Parameter == "OSS.Incorrect.Nosepokes")
# > as.POSIXct(strptime("4/2/2019 9:00","%d/%m/%Y %H:%M")) == as.POSIXct("2019-2-4 9:00")
# [1] TRUE
oss.filtered = subset(oss, difftime(Date, as.POSIXct("2019-2-4 9:00"), units = "weeks") < 13)
oss.filtered$Days = as.numeric(difftime(oss.filtered$Date, as.Date("2019-2-9")))
# intelli

# dane.long.2 ~~ dane.long # Catwalk złożony z kawałków

# intelli.choice.number_mutacja_filtered <- intelli.choice.number_mutacja <- all.long.mutacja <- all.long
# all.long <- rbind(dane.long.2, OSS.long, waga.long, sacharyna.long, intelli.long, sunbites.long)
# all.long.mutacja <- filter(all.long, Weeks < 13, Animal != "405F")
# intelli.choice.number_mutacja <- filter(all.long.mutacja, Parameter == "intelli.choice.number")
# intelli.choice.number_mutacja_filtered <- filter(intelli.choice.number_mutacja, Days != "44.5463622453698")

if (do.verify.ref)
{
    library(tidyr)
    # recreate catwalk.reduced.csv from catwalk.reduced
    catwalk.env = new.env()
    local({do.plot = do.model = FALSE}, envir = catwalk.env)
    source("catwalk.R", local = catwalk.env)
    write.csv(catwalk.env$catwalk.reduced, "catwalk.reduced.csv")
    # recreate Olfactory_results_new_29-04-2021.csv from source data
    olfactory = read.csv2("Olfactory_results.csv", check.names = FALSE, as.is = TRUE)
    olfactory = subset(olfactory, select = -Notes)
    olfactory = olfactory[-68, ]
    is.na(olfactory)[olfactory == ""] = TRUE
    i.na = with(olfactory, is.na(`Time to take/dig out the cracker`))
    #olfactory$`Time to take/dig out the cracker`[i.na] = olfactory$`Time to bite`[i.na]
    t = with(olfactory, ifelse(i.na, `Time to bite`, `Time to take/dig out the cracker`))
    olfactory = cbind(subset(olfactory, select = -Date), `Time to retrieve the cracker` = t, Date = olfactory$Date)
    write.csv(olfactory, "Olfactory_results_new_29-04-2021.csv", row.names = FALSE, quote = FALSE)
    # recreate : OSS_sum_updated_14-01-22_final_full.csv
    # from original : file OSS_sum_updated_9-10_final_full.csv
    oss = read.csv2("OSS_sum_updated_9-10_final_full.csv", check.names = FALSE, as.is = TRUE)
    i.na = sweep(is.na(oss), 2, sapply(oss, is.character), "&")
    oss[i.na] = "NA"
    oss[9, 9] = "0" # "0 (mouse under the floor)"
    write.csv(oss, "OSS_sum_updated_14-01-22_final_full.csv", row.names = FALSE, quote = FALSE, na = "")
    # recreate : females_weight_updated_7june_updated_corrected.csv
    # from original : females_weight_updated_7june_updated.csv
    weight.f = read.csv2("females_weight_updated_7june_updated.csv",
        check.names = FALSE, as.is = TRUE, fileEncoding = "CP1250")
    i.na = is.na(weight.f)
    i.na[, "IntelliCage No."] = FALSE
    weight.f[i.na] = "NA"
    weight.f[10, "Weight [g] 9.05.2019"] = 26.6 # fix error - was 23.6
    write.csv(weight.f, "females_weight_updated_7june_updated_corrected.csv",
        row.names = FALSE, quote = FALSE, na = "")
    # recreate : animals_info.csv
    # from original : podział_na_peletki.csv
    animals = read.csv2("podział_na_peletki.csv", check.names = FALSE, as.is = TRUE, row.names = "Animal ID")
    animals["380F", "Genotype"] = "con"
    intelli_mice = read.csv2("Intelli_mice.csv", check.names = FALSE, as.is = TRUE, row.names = "Mouse.ID")
    intelli_mice$Transponder = as.character(intelli_mice$Transponder)
    animals_info = data.frame(
        Animal = rownames(animals),
        Sex = c("female", "male")[factor(substr(rownames(animals), 4, 5), levels = c("F", "M"))],
        Genotype = c("control", "mutant")[factor(animals$Genotype, levels = c("con", "mut"))],
        Treatment = animals$`Pellet type`,
        Intellicage = NA,
        Dead.prematurely = FALSE,
        Tag = intelli_mice[rownames(animals), "Transponder"]
    )
    animals_info[animals_info$Animal %in% c("391M", "413F", "405F", "374F"), "Dead.prematurely"] = TRUE
    animals_info[animals_info$Sex == "female", "Intellicage"] = 5
    animals_info[animals_info$Animal %in% paste0(c(411:414, 378, 380, 385:388), "F"), "Intellicage"] = 2
    write.csv(animals_info, "animals_info.csv", row.names = FALSE, quote = FALSE)
    # temporary hack to force the script to go forward
if (FALSE)
{
    load("dane_intelli2.RData")
    wsad.intelli <- dane %>%
    group_by(Phase, Tag, Info, EndDateTime) %>%
    summarize(intelli.visit.number = n(),
        intelli.choice.number = length(which((RP != 0) & VisitDuration > 2.0)),
        intelli.sachharin.preference = sum(LicksNumber[which(RP != 0)], na.rm = TRUE)/sum(LicksNumber, na.rm = TRUE),
        intelli.higher.p.preference = length(which((RP == 0.9) & VisitDuration > 2.0))/length(which((RP != 0) & VisitDuration > 2.0)))
    myszy <- read.csv2(file = "Intelli_mice.csv")
    extract.genotype <- transmute(group_by(wsad.intelli, Phase, Tag, Info, EndDateTime),
        Mouse.ID = myszy$Mouse.ID[which(myszy$Transponder == Tag)],
        #Genotype = dane.long.2$Genotype[first(which(as.character(dane.long.2$Animal) == as.character(Mouse.ID)))]
        Genotype = Animal_Genotype(Mouse.ID),
    )
    wsad.intelli <- data.frame(wsad.intelli, extract.genotype)
    wsad.intelli <- filter(wsad.intelli, Info == "reversals")
}

    # override rstatix::anova_test() function which hangs under Linux
    #library (rstatix)
    anova_test = function(...) { NULL }
    # override emmeans::emmeans() function which fails when evaluating in a local env
    #library (emmeans)
    emmeans = function(...) { NULL }

    # set up config vars
    ref.encoding = "CP1250"
    ref.path = "."
    # list of files in AR/
    # catwalk-nowe-ANOVA.R
    # intelli-nowe_dni.R
    # intelli-nowe_v2.R
    # olfactory-nowe.R
    # OSS-nowe.R
    # preferencja-sacharyny-nowe.R
    # wagi-nowe.R

if(FALSE)
{
    #ref.script = "wykresy_do_publikacji_v10_nonmotor_compliant_w_catwalk_anova_new_plots.R"
    #ref.script = "AR/catwalk-nowe-ANOVA.R"
    ref.script = "AR/OSS-nowe.R"
    source("win-compat.R") # działa
    print(ls(ref.env))
    ref.script = "AR/olfactory-nowe.R"
    #source("win-compat.R") # problem : object 'dane.long.2' not found
    ref.script = "AR/preferencja-sacharyny-nowe.R"
    #source("win-compat.R") # problem : object 'dane.long.2' not found
    ref.script = "AR/wagi-nowe.R"
    #source("win-compat.R") # problem : object 'dane.long.2' not found
    ref.script = "AR/intelli-nowe_v2.R"
    source("win-compat.R") # poszło
    print(ls(ref.env))
    ref.script = "AR/intelli-nowe_dni.R"
    source("win-compat.R") # poszło
    print(ls(ref.env))
}
    # nowe
    sink("AR2/catwalk-AR+ML.txt")
    pdf(file = "AR2/catwalk-AR+ML.pdf")
    ref.script = "AR2/catwalk-AR+ML.R"
    source("win-compat.R") # działa
    print(ls(ref.env))
    catwalk.env = ref.env
    dev.off()
    sink()

    sink("AR2/intelli-AR+ML.txt")
    pdf(file = "AR2/intelli-AR+ML.pdf")
    ref.script = "AR2/intelli-AR+ML.R"
    source("win-compat.R") # nie działało - brak h_printarea_males z catwalk-AR+ML.R - poprawione
    print(ls(ref.env))
    intelli.env = ref.env
    dev.off()
    sink()

    sink("AR2/intelli-days-AR+ML.txt")
    pdf(file = "AR2/intelli-days-AR+ML.pdf")
    ref.script = "AR2/intelli-days-AR+ML.R"
    source("win-compat.R") # działa (ale długo)
    print(ls(ref.env))
    intelli.days.env = ref.env
    dev.off()
    sink()

    sink("AR2/saccharin-AR+ML.txt")
    pdf(file = "AR2/saccharin-AR+ML.pdf")
    ref.script = "AR2/saccharin-AR+ML.R"
    source("win-compat.R") # czy działa ?
    print(ls(ref.env))
    saccharin.env = ref.env
    dev.off()
    sink()

    sink("AR2/weight-AR+ML.txt")
    pdf(file = "AR2/weight-AR+ML.pdf")
    ref.script = "AR2/weight-AR+ML.R"
    source("win-compat.R") # czy działa ?
    print(ls(ref.env))
    weight.env = ref.env
    dev.off()
    sink()

    sink("AR2/oss-AR+ML.txt")
    pdf(file = "AR2/oss-AR+ML.pdf")
    ref.script = "AR2/oss-AR+ML.R"
    source("win-compat.R") # czy działa ?
    print(ls(ref.env))
    oss.env = ref.env
    dev.off()
    sink()
}

# joint analysis incl. correlation analysis
#source("joint.R")

