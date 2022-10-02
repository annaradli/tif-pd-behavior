#run after all the dependent scripts - for each test there is 1 or more (in the case of CatWalk) separate .R script
library(tidyverse)
library(knitr)
library(car)
library(broom)



#catwalk
catwalk_to_anova_table <- bind_rows(list(H_MaxContact_males,
                                         H_MaxContact_females,
                                         BOS_hind_males,
                                         BOS_hind_females,
                                         BOS_front_males,
                                         BOS_front_females,
                                         stepseq_AA_males,
                                         stepseq_AA_females,
                                         stepseq_CB_males,
                                         stepseq_CB_females,
                                         stepseq_RB_males,
                                         stepseq_RB_females,
                                         h_printarea_males,
                                         h_printarea_females,
                                         h_printwidth_males,
                                         h_printwidth_females,
                                         h_swingspeed_males,
                                         h_swingspeed_females,
                                         h_stepcycle_males,
                                         h_stepcycle_females,
                                         couplings_RFLF_males,
                                         couplings_RFLF_females,
                                         couplings_rfrh_males ,
                                         couplings_rfrh_females,
                                         couplings_rhlf_males,
                                         couplings_rhlf_females,
                                         cadence_males,
                                         cadence_females,
                                         right_paws_males,
                                         right_paws_females))

catwalk_together_anova <- catwalk_to_anova_table %>%
  nest(data = c(Animal, Genotype, Days, Mean, SEM)) %>%
  mutate(model = map(data, ~aov(Mean ~ Genotype*Days + Error(Animal/Days), .)),
         tidy = map(model, broom::tidy)) %>%
  select(Parameter, Sex, tidy) %>%
  unnest(tidy) %>% print(n = 150)



#weight - separately for females and males
weight.females$Days <- as.factor(weight.females$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.females))


weight.males$Days <- as.factor(weight.males$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.males))


#non-motor males
correct$Days <- as.factor(correct$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = correct))


incorrect$Days <- as.factor(incorrect$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = incorrect))

sachpref.males$Days <- as.factor(sachpref.males$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = sachpref.males))


retrieve.test$Days <- as.factor(retrieve.test$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = retrieve.test))


start.digging.test$Days <- as.factor(start.digging.test$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = start.digging.test))

#non-motor females - by periods
visits$period <- as.factor(visits$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = visits))


choices$period <- as.factor(choices$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = choices))


pref.sach$period <- as.factor(pref.sach$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.sach))


pref.corner$period <- as.factor(pref.corner$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.corner))
