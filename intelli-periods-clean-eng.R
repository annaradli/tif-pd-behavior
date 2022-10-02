library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(rstatix)


#loadfonts(device = "win")

#data import
load("dane_intelli2.RData")
intelli.data <- dane

#add info on Genotype, Animal ID, etc.
animals_info <- read.csv("animals_info.csv")
intelli.data.clean <- full_join(intelli.data, animals_info, by = "Tag") 
# intelli.data.clean <- filter(intelli.data.clean, Animal != "413F") #do not filter out this animal, although it died during the tests

#filter out periods when at least one cage wasn't working properly (wrong program on, cage turned off, technical issues, etc.)
#WARNING! This is for the whole experiment, not just before L-DOPA!
intelli.data.clean <- filter(intelli.data.clean,
       StartDateTime < as.POSIXct(strptime("17/3/2019 1:00","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("18/3/2019 9:30","%d/%m/%Y %H:%M")), #wrong program on
       StartDateTime < as.POSIXct(strptime("25/3/2019 10:31","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("25/3/2019 14:07","%d/%m/%Y %H:%M")),#wrong program on and cages turned off
       StartDateTime < as.POSIXct(strptime("28/3/2019 0:15","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("28/3/2019 11:04","%d/%m/%Y %H:%M")), #program error
       StartDateTime < as.POSIXct(strptime("3/4/2019 18:00","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("4/4/2019 18:00","%d/%m/%Y %H:%M")), #cage malfunction
       StartDateTime < as.POSIXct(strptime("6/4/2019 14:11","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("6/4/2019 14:28","%d/%m/%Y %H:%M")), #saccharin change
       StartDateTime < as.POSIXct(strptime("8/4/2019 10:45","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("8/4/2019 12:51","%d/%m/%Y %H:%M")), #cage no. 2 turned off
       StartDateTime < as.POSIXct(strptime("9/4/2019 9:12","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("9/4/2019 11:47","%d/%m/%Y %H:%M")), #cage no. 5 turned off
       StartDateTime < as.POSIXct(strptime("18/4/2019 00:00","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("21/4/2019 12:00","%d/%m/%Y %H:%M")), #cages malfunction
       StartDateTime < as.POSIXct(strptime("24/4/2019 00:00","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("25/4/2019 19:30","%d/%m/%Y %H:%M")), #cages turned off not simutaneously (catwalk), the comuter froze
       StartDateTime < as.POSIXct(strptime("6/5/2019 9:50","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("6/5/2019 16:21","%d/%m/%Y %H:%M")), #operations on female mice
       StartDateTime < as.POSIXct(strptime("7/5/2019 09:09","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("7/5/2019 09:28","%d/%m/%Y %H:%M")), #corner 3 was changed
       StartDateTime < as.POSIXct(strptime("7/5/2019 13:44","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("7/5/2019 14:00","%d/%m/%Y %H:%M")), # corners 2&4 changed, cage 5 
       StartDateTime < as.POSIXct(strptime("8/5/2019 11:35","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("8/5/2019 17:34","%d/%m/%Y %H:%M")), #cages turned off not simutaneously (catwalk)
       StartDateTime < as.POSIXct(strptime("9/5/2019 14:40","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("9/5/2019 17:30","%d/%m/%Y %H:%M")), # mouse found dead, turned off cage no.2
       StartDateTime < as.POSIXct(strptime("19/5/2019 14:00","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("20/5/2019 9:30","%d/%m/%Y %H:%M")), #cage no. 5 malfunction
       StartDateTime < as.POSIXct(strptime("9/6/2019 1:00","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("10/6/2019 12:05","%d/%m/%Y %H:%M")), #27-03 - corner#3, cage#5 - broken
       StartDateTime < as.POSIXct(strptime("4/6/2019 10:42","%d/%m/%Y %H:%M")) |
         StartDateTime > as.POSIXct(strptime("4/6/2019 14:47","%d/%m/%Y %H:%M")), #cages turned off not simutaneously (catwalk)
       StartDateTime < as.POSIXct(strptime("11/6/2019 1:00","%d/%m/%Y %H:%M")) 
)


#define time periods boundaries - copied from ML's script 'joint.R' - version from 29-07-2021
#but I changed the as.date to as.POSIXct
date.format = "%d.%m.%Y"
uniform.bins = TRUE
period.seq = data.frame(
)

if(uniform.bins)
{
  period.seq = data.frame(
    Boundary = as.POSIXct("3.03.2019", format = date.format) + as.difftime(16, units = "days") * seq(0, 4)
  )
}


#define time ranges and 'period'
intelli.data.clean$range <- cut(as_datetime(intelli.data.clean$StartDateTime), breaks = period.seq$Boundary, include.lowest = TRUE) #consider only days prior to L-DOPA
intelli.data.clean <- filter(intelli.data.clean, range != "2019-03-03 00:00:00") #consider only 3 periods
intelli.data.clean <- intelli.data.clean %>%
  mutate(period = case_when(
    range == "2019-03-19 00:00:00" ~ 2,
    range == "2019-04-04 01:00:00" ~ 3,
    range == "2019-04-20 01:00:00" ~ 4
  ))

#how many days are within each period - not 16 in each
intelli.data.clean %>% 
mutate(true_day = lubridate::as_date(ymd_hms(StartDateTime))) %>%
 group_by(period) %>% summarise(saccharin.dni = length(unique(true_day)))

#because there are varying number of days per period - introduce new variable - true_day - as opposed to "Day" which has wrong timezone
intelli.data.clean <- intelli.data.clean %>% group_by(period) %>% mutate(true_day = lubridate::as_date(ymd_hms(StartDateTime)))
intelli.data.clean %>% group_by(period) %>% summarise(unique_days = unique(true_day))

intelli.data.clean$StartDateTime <- as.POSIXct(intelli.data.clean$StartDateTime, "%Y-%m-%d", tz = "CET")
intelli.data.clean$Days <- round(as.numeric(difftime(intelli.data.clean$true_day, as.POSIXct(strptime("8/2/2019","%d/%m/%Y"), tz = "CET"), units = "days")))

intelli.data.clean$period <- as.factor(intelli.data.clean$period)

#see how many observations are NA in a selected column
length(which(is.na(intelli.data.clean$LicksContactTime)))

#change NAs to 0s
intelli.data.clean <- intelli.data.clean %>% replace(is.na(.), 0)
as_tibble(intelli.data.clean)


#how many visits are there in total in each period per animal?
#how many animals in each period?
intelli.data.clean %>% group_by(Animal, period) %>% summarize(visits = n()) %>% ggplot(aes(x = period, y = visits)) + geom_boxplot(size = 2) + theme_classic()
intelli.data.clean %>% group_by(period) %>% summarize(no.animals = length(unique(Animal)))

intelli.parameters <- intelli.data.clean %>%
  group_by(Animal, period, Genotype) %>%
 summarize(saccharin.30 = length(which(RP==0.3)),
            saccharin.90 = length(which(RP==0.9)),
            fraction.90 = saccharin.90/(saccharin.30+saccharin.90),
            licks.water = sum(LicksNumber[which(RP == 0)], na.rm = TRUE),
            licks.saccharin = sum(LicksNumber[which(RP > 0)], na.rm = TRUE),
            preference.saccharin = licks.saccharin/(licks.water + licks.saccharin),
            visits.per.24h = n()/16,
            choices.per.24h =  length(which(RP >0 & VisitDuration > 2))/16)

intelli.parameters$period <- as.factor(intelli.parameters$period)

intelli.parameters.long <- gather(intelli.parameters, Parameter, Measurement, 4:11)

#saccharin preference
pref.sach <- filter(intelli.parameters.long, Parameter == "preference.saccharin")

analysis.pref.sach <- summarize(group_by(pref.sach, period, Genotype, Parameter),
          Mean = mean(Measurement, na.rm = TRUE),
          SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()))



#higher P corner preference
pref.corner <- filter(intelli.parameters.long, Parameter == "fraction.90")

analysis.pref.corner <- summarize(group_by(pref.corner, period, Genotype, Parameter),
                               Mean = mean(Measurement, na.rm = TRUE),
                               SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()))


#visits in all corners per 24h (16)
visits <- filter(intelli.parameters.long, Parameter == "visits.per.24h")

analysis.visits <- summarize(group_by(visits, period, Genotype, Parameter),
                               Mean = mean(Measurement, na.rm = TRUE),
                               SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()))



#choices = visits in the saccharine corners for more than 2s per 24h (16)
choices <- filter(intelli.parameters.long, Parameter == "choices.per.24h")

analysis.choices <- summarize(group_by(choices, period, Genotype, Parameter),
                            Mean = mean(Measurement, na.rm = TRUE),
                            SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()))



#ANOVA and t-tests:
pref.sach$period <- as.factor(pref.sach$period)
aov_pref.sach <- aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.sach)
summary(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.sach))
#pref.sach %>% group_by(Genotype) %>% summarise(mean = mean(Measurement), SEM = sd(Measurement)/sqrt(n()))

pref.corner$period <- as.factor(pref.corner$period)
aov_pref.corner <- aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.corner)
summary(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.corner))
#pref.corner %>% group_by(Genotype) %>% summarise(mean = mean(Measurement), SEM = sd(Measurement)/sqrt(n()))

visits$period <- as.factor(visits$period)
aov_visits <- aov(Measurement ~ Genotype * period + Error(Animal/period), data = visits)
summary(aov(Measurement ~ Genotype * period + Error(Animal/period), data = visits))

choices$period <- as.factor(choices$period)
aov_choices <- aov(Measurement ~ Genotype * period + Error(Animal/period), data = choices)
summary(aov(Measurement ~ Genotype * period + Error(Animal/period), data = choices))



#t-tests
pref.sach_t_test <- pref.sach %>%
  group_by(period) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")


pref.corner_t_test <- pref.corner %>%
  group_by(period) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")


visits_t_test <- visits %>%
  group_by(period) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")


choices_t_test <- choices %>%
  group_by(period) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")


