library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(rstatix)
library(lemon)
library(writexl)
library(extrafont)

animals_info <- read.csv("animals_info.csv")


olfactory.input.data <- read.csv2(file = "Olfactory_results_new_29-04-2021.csv", sep = ",")

extract.genotype <- transmute(group_by(olfactory.input.data, Date, Mouse.ID),
                              Genotype = animals_info$Genotype[first(which(as.character(animals_info$Animal) == as.character(Mouse.ID)))])

olfactory.input.data <- data.frame(group_by(olfactory.input.data, Date, Mouse.ID), extract.genotype)

olfactory.wide <- data.frame(Date = as.POSIXct(as.Date(as.character(olfactory.input.data$Date), "%d-%m-%Y")), 
                            Animal = olfactory.input.data$Mouse.ID, 
                            Genotype = olfactory.input.data$Genotype.1, 
                            Sex = "male", 
                            Time.to.start.digging = as.numeric(as.difftime(as.character(olfactory.input.data$Time.to.start), "%H:%M:%S", units = "secs")),
                            Time.to.bite = as.numeric(as.difftime(as.character(olfactory.input.data$Time.to.bite), "%H:%M:%S", units = "secs")),
                            Time.to.dig.out.the.cracker = as.numeric(as.difftime(as.character(olfactory.input.data$Time.to.take.dig.out.the.cracker), "%H:%M:%S", units = "secs")),
                            Time.to.retrieve.the.cracker = as.numeric(as.difftime(as.character(olfactory.input.data$Time.to.retrieve.the.cracker), "%H:%M:%S", units = "secs")))

olfactory.long <- gather(olfactory.wide, key = Parameter, value = Measurement, 
                        -Date, -Animal, -Genotype, -Sex)


olfactory.long <- mutate(group_by(olfactory.long, Date, Animal, Parameter))



unique(olfactory.long$Date) # dates in CET and CEST
#there are hours in our Date variable - remove it
olfactory.long$Date <- format(as.POSIXct(olfactory.long$Date, format = "%d/%m/%Y %H:%M:%S"), format = "%Y-%m-%d")
olfactory.long$Date <- as.POSIXct(olfactory.long$Date, "%Y-%m-%d", tz = "CET") #correct CET and CEST timezones for Dates
#add variable Days
olfactory.long$Days <- round(as.numeric(difftime(olfactory.long$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) #correct

olfactory.long$Days <- as.factor(olfactory.long$Days)

#select days prior to the operation
olfactory.long <- olfactory.long %>% filter(Date < as.POSIXct(strptime("7/5/2019", "%d/%m/%Y")))
unique(olfactory.long$Date)

#select only 2 latter sessions and only relevant parameters
olfactory.long.test <- olfactory.long %>% filter(Days %in% c("59", "80")) %>% filter(Parameter %in% c("Time.to.start.digging", "Time.to.retrieve.the.cracker"))
olfactory.long.training <- olfactory.long %>% filter(Days == "41") %>% filter(Parameter %in% c("Time.to.start.digging", "Time.to.retrieve.the.cracker"))



#check for NAs - test 
olfactory.long.test %>% filter(is.na(Measurement)) #no NAs
#do all the animals have complete set of observations?
olfactory.long.test %>% group_by(Animal, Parameter) %>% summarise(n_days = length(unique(Days))) %>% filter(n_days < 2)
#yes

#check for NAs - training
olfactory.long.training %>% filter(is.na(Measurement)) #one missing - 382, time to start digging
#remove
olfactory.long.training <- olfactory.long.training %>% drop_na()


#create separate objects for each parameter, for test and training
start.digging.test <- olfactory.long.test %>% filter(Parameter == "Time.to.start.digging")
retrieve.test <- olfactory.long.test %>% filter(Parameter == "Time.to.retrieve.the.cracker")

start.digging.training <- olfactory.long.training %>% filter(Parameter == "Time.to.start.digging")
retrieve.training <- olfactory.long.training %>% filter(Parameter == "Time.to.retrieve.the.cracker")


####ANOVA and t-tests - TEST
start.digging.test$Days <- as.factor(start.digging.test$Days)
retrieve.test$Days <- as.factor(retrieve.test$Days)
#TIME TO START DIGGING TEST
aov_start.digging.test <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = start.digging.test)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = start.digging.test))
start.digging.test %>% group_by(Genotype) %>% summarise(mean = mean(Measurement), SEM = sd(Measurement)/sqrt(n()))


analysis.start.digging <- summarize(group_by(start.digging.test , Days, Genotype),
                                    Mean = mean(Measurement, na.rm = TRUE),
                                    SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                                    N = n())


start.digging.test_t_test <- start.digging.test %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")

# ###pairwise comparisons as different means of post hoc
# 
# start.digging.test_t_test_pairwise <- start.digging.test %>%
#   group_by(Genotype) %>%
#   pairwise_t_test(Measurement ~ Days, paired = TRUE, p.adjust.method = "bonferroni") 

#TIME TO RETRIEVE TEST
aov_retrieve.test <- aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = retrieve.test)
summary(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = retrieve.test))
retrieve.test %>% group_by(Genotype) %>% summarise(mean = mean(Measurement), SEM = sd(Measurement)/sqrt(n()))

analysis.retrieve <- summarize(group_by(retrieve.test , Days, Genotype),
                                   Mean = mean(Measurement, na.rm = TRUE),
                                   SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                                   N = n())

#t-tests
retrieve.test_t_test <- retrieve.test %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")

# ###pairwise comparisons as different means of post hoc
# 
# retrieve.test_t_test_pairwise <- retrieve.test %>%
#   group_by(Genotype) %>%
#   pairwise_t_test(Measurement ~ Days, paired = TRUE, p.adjust.method = "bonferroni") 


####PLOTS

retrieve.test$Days <- round(as.numeric(difftime(retrieve.test$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) #correct
start.digging.test$Days <- round(as.numeric(difftime(start.digging.test$Date, as.POSIXct(strptime("8/2/2019","%d/%m/%Y")), units = c("days")))) #correct

fig_4D <- retrieve.test %>% group_by(Genotype, Days) %>% summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 4) +
  geom_line(linetype = "solid", size = 1) +
  geom_pointrange(aes(ymin = Mean - SEM, ymax = Mean + SEM), size = 1) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  labs(x = "\n Time to retrieve the cracker",
       y = "Mean [s] \n",
       title = "d.",
       subtitle = "Buried food test") +
  theme_classic() +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 50), xlim = c(50,90), gap = 0.05) +
  scale_x_continuous(breaks = seq(50, 90, by = 10)) +
  theme(aspect.ratio = 5/4,
        legend.position = "none",
        text = element_text(family = "Calibri"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        title = element_text(size = 22, face = "bold"))

fig_4E <- start.digging.test %>% group_by(Genotype, Days) %>% summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 4) +
  geom_line(linetype = "solid", size = 1) +
  geom_pointrange(aes(ymin = Mean - SEM, ymax = Mean + SEM), size = 1) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  labs(x = "\n Time to start digging \nat the right corner",
       y = "Mean [s] \n",
       title = "e.",
       subtitle = "Buried food test") +
  theme_classic() +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 30), xlim = c(50,90), gap = 0.05) +
  scale_x_continuous(breaks = seq(50, 90, by = 10)) +
  theme(aspect.ratio = 5/4,
        legend.position = "none",
        text = element_text(family = "Calibri"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        title = element_text(size = 22, face = "bold"))




#####
#####
####t-tests and plots - TRAINING

#TIME TO START DIGGING TEST

analysis.start.digging.training <- summarize(group_by(start.digging.training , Days, Genotype),
                                   Mean = mean(Measurement, na.rm = TRUE),
                                   SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                                   N = n())
fig_S2A <- ggplot(analysis.start.digging.training , stat = "identity", aes(x = Genotype, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = Mean - SEM, ymax = Mean + SEM),  size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  # coord_cartesian(ylim = c(0, 32), expand = TRUE) +
  theme_classic() +
  labs(x = "\n Time to start digging \nat the right corner",
       y = "Mean [s] \n",
       title = "a.",
       subtitle = "Buried food test") +
  theme_classic() +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 80), gap = 0.05) +
  theme(aspect.ratio = 5/4,
        legend.position = "none",
        text = element_text(family = "Calibri"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        title = element_text(size = 22, face = "bold"))


#t-test
start.digging.training_t_test <- start.digging.training %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")

#TIME TO RETRIEVE TRAINING
analysis.retrieve.training <- summarize(group_by(retrieve.training , Days, Genotype),
                              Mean = mean(Measurement, na.rm = TRUE),
                              SEM = sd(Measurement, na.rm = TRUE)/sqrt(n()),
                              N = n())

fig_S2B <- ggplot(analysis.retrieve.training , stat = "identity", aes(x = Genotype, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = Mean - SEM, ymax = Mean + SEM),  size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  # coord_cartesian(ylim = c(0, 32), expand = TRUE) +
  theme_classic() +
  labs(x = "\n Time to retrieve the cracker",
       y = "Mean [s \n",
       title = "b.",
       subtitle = "Buried food test") +
  theme_classic() +
  coord_capped_cart(bottom='none', left='none', ylim = c(0, 100), gap = 0.05) +
  theme(aspect.ratio = 5/4,
        legend.position = "none",
        text = element_text(family = "Calibri"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        title = element_text(size = 22, face = "bold"))

#t-test
retrieve.training_t_test <- retrieve.training %>%
  group_by(Days) %>%
  t_test(Measurement ~ Genotype) %>%
  adjust_pvalue(method = "bonferroni")
