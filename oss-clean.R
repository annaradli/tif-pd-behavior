library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(rstatix)
library(lemon)
library(writexl)
library(extrafont)


import.OSS <- data.frame(read.table(file = "OSS_sum_updated_14-01-22_final_full.csv", header = TRUE, sep = ",", dec = "."), sex = "male")

import.OSS.2  <- data.frame()

for (i in seq(8, ncol(import.OSS)-1, 3)){
  
  Date = as.POSIXct(as.Date(colnames(import.OSS)[i], "X%d.%m.%Y"))
  Animal = as.character(import.OSS$Animal.ID[1:nrow(import.OSS)-1])
  Genotype = as.character(import.OSS$Genotype[1:nrow(import.OSS)-1])
  Sex = as.character(import.OSS$sex[1:nrow(import.OSS)-1])
  Rewarded.Side = import.OSS$Rewarded.side[1:nrow(import.OSS)-1]
  OSS.Correct.Nosepokes = ifelse(Rewarded.Side == "L", 
                                 as.numeric(as.character(import.OSS[1:nrow(import.OSS)-1, i])), 
                                 as.numeric(as.character(import.OSS[1:nrow(import.OSS)-1, i + 2])))
  OSS.Incorrect.Nosepokes = ifelse(Rewarded.Side == "L", 
                                   as.numeric(as.character(import.OSS[1:nrow(import.OSS)-1, i + 2])), 
                                   as.numeric(as.character(import.OSS[1:nrow(import.OSS)-1, i])))
  OSS.Rewards = as.numeric(as.character(import.OSS[1:nrow(import.OSS)-1, i+1]))
  
  temp <- data.frame(Date, Animal, Genotype, Sex, OSS.Correct.Nosepokes, OSS.Incorrect.Nosepokes, OSS.Rewards)
  import.OSS.2  <- rbind(import.OSS.2,temp)
}


OSS.long <- gather(import.OSS.2 , key = Parameter, value = Measurement, 
                   -Date, -Animal, -Genotype, -Sex)


OSS.long <- mutate(group_by(OSS.long, Date, Animal, Parameter))

unique(OSS.long$Date) # dates in CET and CEST
#there are hours in our Date variable - remove it
OSS.long$Date <- format(as.POSIXct(OSS.long$Date, format = "%d/%m/%Y %H:%M:%S"), format = "%Y-%m-%d")
OSS.long$Date <- as.POSIXct(OSS.long$Date, "%Y-%m-%d", tz = "CET") #correct CET and CEST timezones for Dates
#add variable Days - compliant with wagi-nowe.R
OSS.long$Days <- round(as.numeric(difftime(OSS.long$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) #correct

OSS.long$Days <- as.factor(OSS.long$Days)

#select days prior to the operation
OSS.long <- OSS.long %>% filter(Date < as.POSIXct(strptime("7/5/2019", "%d/%m/%Y")))
unique(OSS.long$Date)

#check for NAs
OSS.long %>% filter(is.na(Measurement))
#no NAs for the selected time period

#create separate objects for active and inactive nosepokes
correct <- OSS.long %>% filter(Parameter == "OSS.Correct.Nosepokes")
incorrect <- OSS.long %>% filter(Parameter == "OSS.Incorrect.Nosepokes")

correct$Days <- as.factor(correct$Days)
incorrect$Days <- as.factor(incorrect$Days)
#ANOVA and plots

aov_correct <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = correct)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = correct))
correct %>% group_by(Genotype) %>% summarise(mean = mean(Measurement), SEM = sd(Measurement)/sqrt(n()))

analiza.correct <- summarize(group_by(correct, Days, Genotype),
                                Mean = mean(Measurement, na.rm = TRUE),
                                SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                                N = n())



aov_incorrect <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = incorrect)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = incorrect))
incorrect %>% group_by(Genotype) %>% summarise(mean = mean(Measurement), SEM = sd(Measurement)/sqrt(n()))

analiza.incorrect <- summarize(group_by(incorrect, Days, Genotype),
                             Mean = mean(Measurement, na.rm = TRUE),
                             SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                             N = n())


#t-tests
correct_t_test <- correct %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")

incorrect_t_test <- incorrect %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")

#mean correct and incorrect
correct %>% group_by(Genotype) %>% summarize(mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()))

incorrect %>% group_by(Genotype) %>% summarize(mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()))

###pairwise comparisons as different means of post hoc
# 
# correct_t_test_pairwise <- correct %>%
#   group_by(Genotype) %>%
#   pairwise_t_test(Measurement ~ Days, paired = TRUE, p.adjust.method = "bonferroni") 
# 
# incorrect_t_test_pairwise <- incorrect %>%
#   group_by(Genotype) %>%
#   pairwise_t_test(Measurement ~ Days, paired = TRUE, p.adjust.method = "bonferroni") 

###PLOTS

correct$Days <- as.numeric(as.character(correct$Days))
incorrect$Days <- as.numeric(as.character(incorrect$Days))

fig_4A <- correct %>% group_by(Genotype, Days) %>% summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 4) +
  geom_line(linetype = "solid", size = 1) +
  geom_pointrange(aes(ymin = Mean - SEM, ymax = Mean + SEM),  size = 1) +
  scale_colour_manual(values = c("#95c2db", "#1674cc"), labels = c("controls", "mutants"))+
  labs(x = "\n Days after last tamoxifen administration",
       y = "Mean \n",
       title = "a.",
       subtitle = "Number of active operant responses") +
  theme_classic() +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 200), xlim = c(20, 90), gap = 0.05) +
  scale_x_continuous(breaks = seq(20, 90, by = 10)) +
  theme(aspect.ratio = 50/55,
        legend.position = "none",
        text = element_text(family = "Calibri"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        title = element_text(size = 22, face = "bold")) +
  geom_vline(xintercept = 32, linetype = 2, size = 0.7) +
  geom_text(angle = 90, aes(x = 27, y = 180, label = "training"), color = "black")

fig_4B <- incorrect %>% group_by(Genotype, Days) %>% summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 4) +
  geom_line(linetype = "solid", size = 1) +
  geom_pointrange(aes(ymin = Mean - SEM, ymax = Mean + SEM),  size = 1) +
  scale_colour_manual(values = c("#95c2db", "#1674cc"), labels = c("controls", "mutants"))+
  labs(x = "\n Days after last tamoxifen administration",
       y = "Mean \n",
       title = "b.",
       subtitle = "Number of inactive operant responses") +
  theme_classic() +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 50), xlim = c(20, 90), gap = 0.05) +
  scale_x_continuous(breaks = seq(20, 90, by = 10)) +
  theme(aspect.ratio = 50/55,
        legend.position = "none",
        text = element_text(family = "Calibri"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        title = element_text(size = 22, face = "bold")) +
  geom_vline(xintercept = 32, linetype = 2, size = 0.7) +
  geom_text(angle = 90, aes(x = 27, y = 40, label = "training"), color = "black")
