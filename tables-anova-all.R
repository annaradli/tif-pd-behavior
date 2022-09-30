#after all the dependent scripts
library(tidyverse)
library(knitr)
library(car)
library(broom)
library(flextable)
library(writexl)
library(xlsx)
library(openxlsx)


H_MaxContact_males
H_MaxContact_females
BOS_hind_males
BOS_hind_females
BOS_front_males
BOS_front_females
stepseq_AA_males
stepseq_AA_females
stepseq_CB_males
stepseq_CB_females
stepseq_RB_males
stepseq_RB_females
h_printarea_males
h_printarea_females
h_printwidth_males
h_printwidth_females
h_swingspeed_males
h_swingspeed_females
h_stepcycle_males
h_stepcycle_females
couplings_RFLF_males
couplings_RFLF_females
couplings_rfrh_males 
couplings_rfrh_females
couplings_rhlf_males
couplings_rhlf_females
cadence_males
cadence_females
right_paws_males
right_paws_females 

weight.males
weight.females
correct
incorrect
sachpref.males
retrieve.test
start.digging.test

#period, not days
visits
choices
pref.sach
pref.corner

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
write_xlsx(catwalk_together_anova, "Table_S5_catwalk.xlsx") 


#weight - separately for females and males
weight.females$Days <- as.factor(weight.females$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.females))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = weight.females)), "Table_S5_weight_females.xlsx")

weight.males$Days <- as.factor(weight.males$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = weight.males))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = weight.males)), "Table_S5_weight_males.xlsx")

#non-motor males
correct$Days <- as.factor(correct$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = correct))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = correct)), "Table_S5_correct_oss.xlsx")

incorrect$Days <- as.factor(incorrect$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = incorrect))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = incorrect)), "Table_S5_incorrect_oss.xlsx")

sachpref.males$Days <- as.factor(sachpref.males$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = sachpref.males))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = sachpref.males)), "Table_S5_sachpref.males.xlsx")

retrieve.test$Days <- as.factor(retrieve.test$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = retrieve.test))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = retrieve.test)), "Table_S5_retrieve.test.xlsx")

start.digging.test$Days <- as.factor(start.digging.test$Days)
tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days), data = start.digging.test))
write_xlsx(tidy(aov(Measurement ~ Genotype * Days + Error(Animal/Days),data = start.digging.test)), "Table_S5_start.digging.test.xlsx")

#non-motor females - by periods
visits$period <- as.factor(visits$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = visits))
write_xlsx(tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = visits)), "Table_S5_visits.xlsx")

choices$period <- as.factor(choices$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = choices))
write_xlsx(tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = choices)), "Table_S5_choices.xlsx")

pref.sach$period <- as.factor(pref.sach$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.sach))
write_xlsx(tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.sach)), "Table_S5_pref.sach.xlsx")

pref.corner$period <- as.factor(pref.corner$period)
tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.corner))
write_xlsx(tidy(aov(Measurement ~ Genotype * period + Error(Animal/period), data = pref.corner)), "Table_S5_pref.corner.xlsx")