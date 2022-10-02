library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(rstatix)


animals_info <- read.csv("animals_info.csv")

import.weight.males <- data.frame(read.table(file = "males_weight_updated_7june.csv", header = TRUE, sep = ";", dec = ","), sex = "male")

import.weight.males2  <- data.frame()

for (i in (5 : (ncol(import.weight.males)-1))){
  
  Date = as.POSIXct(as.Date(ifelse(str_detect(colnames(import.weight.males)[i], "Initial"), 
                                   as.character(as.Date("01.02.2019", "%d.%m.%Y")),
                                   as.character(as.Date(colnames(import.weight.males)[i], "Weight..g..%d.%m.%Y"))), "%Y-%m-%d"))
  Animal = as.character(import.weight.males$Animal.ID)
  Sex = as.character(import.weight.males$sex)
  Weight = as.numeric(str_replace_all(as.character(import.weight.males[ ,i]), ",", "."))
  extract.genotype <- transmute(group_by(import.weight.males, Animal.ID),
                                Genotype = animals_info$Genotype[first(which(as.character(animals_info$Animal) == as.character(Animal.ID)))])
  temp <- data.frame(Date, Animal, Genotype = extract.genotype$Genotype, Sex, Weight)
  import.weight.males2  <- rbind(import.weight.males2,temp)
}

import.weight.females <- data.frame(read.table(file = "females_weight_updated_7june_updated_corrected.csv", header = TRUE, sep = ",", dec = ","), sex = "female")
import.weight.females <- filter(import.weight.females, Mouse.ID != "381F") #mouse excluded from the Intelli experiment
import.weight.females2  <- data.frame()

for (i in (2 : (ncol(import.weight.females)-3))){
  
  Date = as.POSIXct(as.Date(ifelse(str_detect(colnames(import.weight.females)[i], "Initial"), 
                                   as.character(as.Date("01.02.2019", "%d.%m.%Y")),
                                   as.character(as.Date(colnames(import.weight.females)[i], "Weight..g..%d.%m.%Y"))), "%Y-%m-%d"))
  Animal = as.character(import.weight.females$Mouse.ID)
  Sex = as.character(import.weight.females$sex)
  Weight = as.numeric(str_replace_all(as.character(import.weight.females[ ,i]), ",", "."))
  extract.genotype <- transmute(group_by(import.weight.females, Mouse.ID),
                                Genotype = animals_info$Genotype[first(which(as.character(animals_info$Animal) == as.character(Mouse.ID)))])
  temp <- data.frame(Date, Animal, Genotype = extract.genotype$Genotype, Sex, Weight)
  import.weight.females2  <- rbind(import.weight.females2,temp)
}


import.weight <- rbind(import.weight.males2, import.weight.females2)

weight.long <- gather(import.weight, key = Parameter, value = Measurement, 
                    -Date, -Animal, -Genotype, -Sex)

weight.long <- mutate(group_by(weight.long, Date, Animal, Parameter))


#the following fragment not necessary, unless we want to check which animal is prematurely dead
# #remove some columns so that they do not duplicate after merging two dfs
# weight.long <- weight.long %>% select(-Genotype, -Sex)
# 
# #merge with animals info
# animals_info <- read.csv("animals_info.csv")
# weight.long <- full_join(weight.long, animals_info, by = "Animal") 

#what are the timezones for the variable Date?
unique(weight.long$Date) # dates in CET and CEST
#there are hours in our Date variable - remove it
weight.long$Date <- format(as.POSIXct(weight.long$Date, format = "%d/%m/%Y %H:%M:%S"), format = "%Y-%m-%d")
weight.long$Date <- as.POSIXct(weight.long$Date, "%Y-%m-%d", tz = "CET") #correct CET and CEST timezones for Dates
#add variable Days - compliant with catwalk-nowe-ANOVA.R
weight.long$Days <- round(as.numeric(difftime(weight.long$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) #correct

weight.long$Days <- as.factor(weight.long$Days)

#create separate objects for each sex
weight.males <- weight.long %>% filter(Sex == "male")
weight.females <- weight.long %>% filter(Sex == "female")

#check dates of weight measuring and select only data prior to the operations
unique(weight.males$Date)
weight.males <- weight.males %>% filter(Date < as.POSIXct(strptime("7/5/2019", "%d/%m/%Y")))

unique(weight.females$Date)
weight.females <- weight.females %>% filter(Date < as.POSIXct(strptime("6/5/2019", "%d/%m/%Y")))

#check for NAs
weight.males %>% filter(is.na(Measurement))
weight.females %>% filter(is.na(Measurement))
#no NAs for the time points - but for later - yes, 4 prematurely dead animals

#ANOVA  - MALES
weight.males$Days <- as.factor(weight.males$Days)
aov_weight.males <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.males)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.males))

analysis.weight.males <- summarize(group_by(weight.males, Days, Genotype),
                               Mean = mean(Measurement, na.rm = TRUE),
                               SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                               N = n())


#ANOVA - FEMALES
weight.females$Days <- as.factor(weight.females$Days)
aov_weight.females <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.females)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.females))

analysis.weight.females <- summarize(group_by(weight.females, Days, Genotype),
                                Mean = mean(Measurement, na.rm = TRUE),
                                SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                                N = n())



#t-tests
weight.males_t_test <- weight.males %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")

weight.females_t_test <- weight.females %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")





