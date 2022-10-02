library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(rstatix)


#based on catwalk.reduced object from ML's script catwalk.R
catwalk_data <-read.csv("catwalk.reduced.csv")

colnames(catwalk_data)

#change Days 
unique(catwalk_data$Date) # 4 dates are in CET, 4 dates are in CET
catwalk_data$Date <- as.POSIXct(catwalk_data$Date, "%Y-%m-%d", tz = "CET") #correct CET and CEST timezones for Dates
catwalk_data$Days <- round(as.numeric(difftime(catwalk_data$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) 
#'round' because due to timezone changes some values were not integers

glm.parameters.all = c("Animal",
                       "Sex",
                       "Genotype",
                       "Days",
                       "BOS_HindPaws_Mean_.cm.", 
                       "StepSequence_AA_...", 
                       "StepSequence_CB_...",
                       "H_MaxContactMeanIntensity_Mean",
                       "H_PrintArea_.cm.._Mean",
                       "H_PrintWidth_.cm._Mean",
                       "H_SwingSpeed_.cm.s._Mean",
                       "H_StepCycle_.s._Mean",
                       "Couplings_RF..LF_Mean", 
                       "Couplings_RF..RH_Mean",
                       "BOS_FrontPaws_Mean_.cm.",
                       "Couplings_RH..LF_Mean",
                       "StepSequence_RB_...",
                       "OtherStatistics_Cadence",
                       "PrintPositions_RightPaws_Mean_.cm.")

catwalk_data <- catwalk_data %>%
  select(one_of(glm.parameters.all))

catwalk_data$Days <- as.factor(catwalk_data$Days)

#change the format to long
catwalk_data_long <- catwalk_data %>%
  gather(Parameter, Value, 5:19) 

#find all NAs
catwalk_nas <- catwalk_data_long %>% filter(is.na(Value)) #79 NA observations
unique(catwalk_nas$Animal) #21 animals with at least 1 NA

#find all animals that lack data at least for 1 time-point

catwalk_data_long %>% filter(Sex == "male") %>% group_by(Animal) %>% summarise(unique_days = unique(Days)) %>%
  group_by(Animal) %>% summarise(n_timepoints = length(unique(unique_days))) %>% filter(n_timepoints < 4) #391M has only 3 unique timepoints data

catwalk_data_long %>% filter(Sex == "female") %>% group_by(Animal) %>% summarise(unique_days = unique(Days)) %>%
  group_by(Animal) %>% summarise(n_timepoints = length(unique(unique_days))) %>% filter(n_timepoints < 4) #385F and 413F have only 3 unique timepoints data

#find all cases in which an animal has observations for a parameter for less than 4 sessions, having excluded NAs
missing_data <- catwalk_data_long %>% drop_na(Value) %>% group_by(Animal, Parameter) %>% summarise(n_days = length(unique(Days))) %>% filter(n_days < 4)
unique(missing_data$Animal) #"382M" "385F" "391M" "413F" "415M" 
#"382M" missing Couplings_RH..LF, H_StepCycle, H_SwingSpeed, Print_Positions_RightPaws | "415M" missing H_StepCycle and H_SwingSpeed
#these mice will be excluded from these particular analyses
unique(missing_data$Parameter)

#filtering the data to exclude the 3 mice that had 3 CatWalk sessions only, and exclude all NAs
catwalk_data_long <- catwalk_data_long %>%
  filter(Animal != "385F") %>%
  filter(Animal != "413F") %>%
  filter(Animal != "391M") %>%
  drop_na()

#####ANOVA
####save each parameter for each sex as a separate object, add means and perform ANOVA

#code for checking completeness of a df object: H_MaxContact_males %>% group_by(Animal) %>% summarise(n_days = length(unique(Days)))

H_MaxContact_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "H_MaxContactMeanIntensity_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = H_MaxContact_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days

  
H_MaxContact_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "H_MaxContactMeanIntensity_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = H_MaxContact_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none


BOS_hind_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "BOS_HindPaws_Mean_.cm.") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = BOS_hind_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days and Genotype

BOS_hind_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "BOS_HindPaws_Mean_.cm.") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = BOS_hind_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Days


BOS_front_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "BOS_FrontPaws_Mean_.cm.") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = BOS_front_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days 

BOS_front_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "BOS_FrontPaws_Mean_.cm.") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = BOS_front_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none


stepseq_AA_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "StepSequence_AA_...") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = stepseq_AA_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days

stepseq_AA_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "StepSequence_AA_...") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = stepseq_AA_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Days


stepseq_CB_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "StepSequence_CB_...") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = stepseq_CB_males, Mean ~ Genotype*Days + Error(Animal/Days))) #none

stepseq_CB_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "StepSequence_CB_...") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = stepseq_CB_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none


stepseq_RB_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "StepSequence_RB_...") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = stepseq_RB_males, Mean ~ Genotype*Days + Error(Animal/Days))) #none

stepseq_RB_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "StepSequence_RB_...") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = stepseq_RB_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none


h_printarea_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "H_PrintArea_.cm.._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_printarea_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Genotype and Days

h_printarea_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "H_PrintArea_.cm.._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_printarea_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none


h_printwidth_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "H_PrintWidth_.cm._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_printwidth_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Genotype and Days

h_printwidth_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "H_PrintWidth_.cm._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_printwidth_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Genotype*Days


h_swingspeed_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Animal != "382M") %>%
  filter(Animal != "415M") %>%
  filter(Parameter == "H_SwingSpeed_.cm.s._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_swingspeed_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days

#extra filtering of animals for females as well - 4 extra filtered out, 6 in total

h_swingspeed_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "H_SwingSpeed_.cm.s._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_swingspeed_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Genotype

#for step cycle- the same filtering as for swing speed
h_stepcycle_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Animal != "382M") %>%
  filter(Animal != "415M") %>%
  filter(Parameter == "H_StepCycle_.s._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_stepcycle_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days

h_stepcycle_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "H_StepCycle_.s._Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = h_stepcycle_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Days


couplings_RFLF_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "Couplings_RF..LF_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = couplings_RFLF_males, Mean ~ Genotype*Days + Error(Animal/Days))) #none


couplings_RFLF_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "Couplings_RF..LF_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = couplings_RFLF_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none

couplings_rfrh_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "Couplings_RF..RH_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = couplings_rfrh_males, Mean ~ Genotype*Days + Error(Animal/Days))) #none


couplings_rfrh_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "Couplings_RF..RH_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = couplings_rfrh_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Days

couplings_rhlf_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Animal != "382M") %>%
  filter(Parameter == "Couplings_RH..LF_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = couplings_rhlf_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Genotype and Interaction


couplings_rhlf_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "Couplings_RH..LF_Mean") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = couplings_rhlf_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none


cadence_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Parameter == "OtherStatistics_Cadence") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = cadence_males, Mean ~ Genotype*Days + Error(Animal/Days))) #Days

cadence_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "OtherStatistics_Cadence") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = cadence_females, Mean ~ Genotype*Days + Error(Animal/Days))) #Days


right_paws_males <- catwalk_data_long %>%
  filter(Sex == "male") %>%
  filter(Animal != "382M") %>%
  filter(Parameter == "PrintPositions_RightPaws_Mean_.cm.") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = right_paws_males, Mean ~ Genotype*Days + Error(Animal/Days))) #none


right_paws_females <- catwalk_data_long %>%
  filter(Sex == "female") %>%
  filter(Parameter == "PrintPositions_RightPaws_Mean_.cm.") %>%
  group_by(Animal, Genotype, Days, Parameter, Sex) %>%
  summarise(Mean = mean(Value),
            SEM = sd(Value, na.rm = TRUE)/sqrt(n()))
summary(aov(data = right_paws_females, Mean ~ Genotype*Days + Error(Animal/Days))) #none



