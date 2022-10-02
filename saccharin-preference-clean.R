library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(rstatix)

animals_info <- read.csv("animals_info.csv")

saccharin.input.data <- read.csv2(file = "saccharin_preference_clean.csv")

saccharin.input.data$Start <- as.POSIXct(as.Date(saccharin.input.data$Start, "%d.%m.%Y"))
saccharin.input.data$Finish <- as.POSIXct(as.Date(saccharin.input.data$Finish, "%d.%m.%Y"))

saccharin.input.data <- mutate(group_by(saccharin.input.data, Start, Mouse.ID),
                         pref.sach.volume.water = Water.finish - Water.start,
                         pref.sach.volume.saccharin = Saccharin.finish - Saccharin.start,
                         pref.sach.preference = pref.sach.volume.saccharin/(pref.sach.volume.saccharin + pref.sach.volume.water),
                         pref.sach.volume = pref.sach.volume.saccharin + pref.sach.volume.water)


#remove 4 observations with the consumed volume >20 ml
saccharin.input.data <- saccharin.input.data %>%
  dplyr::filter(pref.sach.volume < 20)


extract.genotype <- transmute(group_by(saccharin.input.data, Start, Mouse.ID),
                              Genotype = animals_info$Genotype[first(which(as.character(animals_info$Animal) == as.character(Mouse.ID)))])

saccharin.input.data <- data.frame(saccharin.input.data, extract.genotype)

saccharin.wide <- data.frame(Date = saccharin.input.data$Start, 
                             Animal = saccharin.input.data$Mouse.ID, 
                             Genotype = saccharin.input.data$Genotype, 
                             Sex = "male", 
                             pref.sach.preference = saccharin.input.data$pref.sach.preference,
                             pref.sach.volume = saccharin.input.data$pref.sach.volume)

saccharin.long <- gather(saccharin.wide, key = Parameter, value = Measurement, 
                         -Date, -Animal, -Genotype, -Sex)

saccharin.long <- mutate(group_by(saccharin.long, Date, Animal, Parameter))



unique(saccharin.long$Date) # dates in CET and CEST
#there are hours in our Date variable - remove it
saccharin.long$Date <- format(as.POSIXct(saccharin.long$Date, format = "%d/%m/%Y %H:%M:%S"), format = "%Y-%m-%d")
saccharin.long$Date <- as.POSIXct(saccharin.long$Date, "%Y-%m-%d", tz = "CET") #correct CET and CEST timezones for Dates
#add variable Days
saccharin.long$Days <- round(as.numeric(difftime(saccharin.long$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) #correct

saccharin.long$Days <- as.factor(saccharin.long$Days)

#select days prior to the operation
saccharin.long <- saccharin.long %>% filter(Date < as.POSIXct(strptime("7/5/2019", "%d/%m/%Y")))
unique(saccharin.long$Date)

#check for NAs
saccharin.long %>% filter(is.na(Measurement))
#no NAs for the selected time period

#check whether all animals have viable results for all time points
missing_data <- saccharin.long %>% group_by(Animal, Parameter) %>% summarise(n_days = length(unique(Days))) %>% filter(n_days < 4)
unique(missing_data$Animal) #"389M" "392M" "407M"
#exclude them from the analysis
saccharin.long <- saccharin.long %>% filter(!Animal %in% c("389M", "392M", "407M"))

#select only saccharin preference
sachpref.males <- saccharin.long %>% filter(Parameter == "pref.sach.preference")

#ANOVA and plots
sachpref.males$Days <- as.factor(sachpref.males$Days)
aov_sachpref.males <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = sachpref.males)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = sachpref.males))

analysis.sachpref.males <- summarize(group_by(sachpref.males, Days, Genotype),
                             Mean = mean(Measurement, na.rm = TRUE),
                             SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                             N = n())


#t-tests
sachpref.males_t_test <- sachpref.males %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")





