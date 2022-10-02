#run after all the dependent scripts - for each test there is 1 or more (in the case of CatWalk) separate .R script
library(tidyverse)
library(lemon)
library(extrafont)
library(lubridate)


#weight
source("weight-clean.R")
weight.males$Days <- as.numeric(as.character(weight.males$Days))
weight.males %>% group_by(Genotype, Days) %>% 
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(15, 30),  xlim = c(-10, 90), expand = TRUE) +
  geom_text(aes(x = 58, y = 28.7, label = "*"), size = 10, colour = "black") +
  ylab("Weight [g]") +
  theme(aspect.ratio = 1.1,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("weight-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)


weight.females$Days <- as.numeric(as.character(weight.females$Days))
weight.females %>% group_by(Genotype, Days) %>% 
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(15, 30), xlim = c(-10, 90), expand = TRUE) +
  ylab("Weight [g]") +
  theme(aspect.ratio = 1.1,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("weight-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

#olfactory
source("olfactory-clean.R")
retrieve.test$Days <- as.numeric(as.character(retrieve.test$Days))
retrieve.test %>% group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 40),  xlim = c(30, 90), expand = TRUE) +
  ylab("Time to retrieve the cracker [s]") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("olfactory-retrieve-082022.pdf", width = 7, height = 7, device = cairo_pdf)

start.digging.test$Days <- as.numeric(as.character(start.digging.test$Days))
start.digging.test %>% group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 40),  xlim = c(30, 90), expand = TRUE) +
  ylab("Time to start digging [s]") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("olfactory-startdigging-082022.pdf", width = 7, height = 7, device = cairo_pdf)

#training olfactory
ggplot(analysis.start.digging.training, stat = "identity", aes(x = Genotype, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.1, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  coord_capped_cart(bottom = "none", left = "both",  gap = 0.03,
                     ylim = c(0, 100), expand = TRUE) +
  ylab( "Start digging training [s]") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("olfactory-startdigging-training-082022.pdf", width = 7, height = 7, device = cairo_pdf)

ggplot(analysis.retrieve.training , stat = "identity", aes(x = Genotype, y = Mean, group = Genotype, color = Genotype)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.1, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  coord_capped_cart(bottom = "none", left = "both" , gap = 0.03,
                    ylim = c(0, 100), expand = TRUE) +
  ylab( "Retrieve training [s]") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("olfactory-retrieve-training-082022.pdf", width = 7, height = 7, device = cairo_pdf)


#as in wykresy_do_publikacji_v7_nonmotor_compliant_w_catwalk_anova_new_plots.R but not facet wrap and export 5x7
# weight.males %>% group_by(Genotype, Days) %>% 
#   summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
#   ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
#   geom_point(size = 9) +
#   geom_line(linetype = "solid", size = 1) +
#   geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 2, size = 0.8) +
#   scale_colour_manual(values = c( "#95c2db", "#1674cc")) +
#   coord_cartesian(ylim = c(15, 30), expand = TRUE) +
#   theme_classic() 
# #ggsave("weight-wg-skryptu-10-21.pdf", width = 10, height = 7)

#oss
source("oss-clean.R")
correct$Days <- as.numeric(as.character(correct$Days))
correct %>% group_by(Genotype, Days) %>% 
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 5) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 1, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80, 90) ) +
  scale_y_continuous(breaks = c(0, 45, 90, 135, 180)) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 180),  xlim = c(20, 90), expand = TRUE) +
  ylab("Active nosepokes") +
  theme(aspect.ratio = 0.5,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("oss-correct-082022.pdf", width = 7, height = 7, device = cairo_pdf)

incorrect$Days <- as.numeric(as.character(incorrect$Days))
incorrect %>% group_by(Genotype, Days) %>% 
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 5) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 1, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80, 90) ) +
  scale_y_continuous(breaks = c(0, 10, 20)) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 20),  xlim = c(20, 90), expand = TRUE) +
  ylab("Active nosepokes") +
  theme(aspect.ratio = 0.5,
    text = element_text(size = 22, color = "black", family = "Arial"),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(color = "black", size = 26),
    axis.title.y = element_text(vjust = +3))
#ggsave("oss-incorrect-082022.pdf", width = 7, height = 7, device = cairo_pdf)

#saccharin preference
source("saccharin-preference-clean.R")
sachpref.males$Days <- as.numeric(as.character(sachpref.males$Days))
sachpref.males%>% group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 1),  xlim = c(30, 90), expand = TRUE) +
  ylab("Saccharin preference") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("sachpref-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

#intelli periods
source("intelli-periods-clean.R")

visits$period <- as.numeric(as.character(visits$period))
visits %>% group_by(Genotype, period) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = period, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.1, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 2, 3, 4 )) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 120),  xlim = c(1.8 , 4.2), expand = TRUE) +
  geom_text(aes(x = 2, y = 122, label = "*"), size = 10, colour = "black") +
  geom_text(aes(x = 3, y = 121, label = "*"), size = 10, colour = "black") +
  ylab("Visits per 24h") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("intelli-visits-082022.pdf", width = 7, height = 7, device = cairo_pdf)

choices$period <- as.numeric(as.character(choices$period))
choices %>% group_by(Genotype, period) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = period, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.1, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 2, 3, 4 )) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 80),  xlim = c(1.8 , 4.2), expand = TRUE) +
  ylab("choices per 24h") +
  geom_text(aes(x = 2, y = 71, label = "*"), size = 10, colour = "black") +
  geom_text(aes(x = 3, y = 69, label = "*"), size = 10, colour = "black") +
  geom_text(aes(x = 4, y = 62, label = "*"), size = 10, colour = "black") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("intelli-choices-082022.pdf", width = 7, height = 7, device = cairo_pdf)

pref.sach$period <- as.numeric(as.character(pref.sach$period))
pref.sach %>% group_by(Genotype, period) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = period, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.1, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 2, 3, 4 )) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 1),  xlim = c(1.8 , 4.2), expand = TRUE) +
  ylab("Saccharin preference") +
  geom_text(aes(x = 4, y = 0.99, label = "*"), size = 10, colour = "black") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("intelli-pref.sach-082022.pdf", width = 7, height = 7, device = cairo_pdf)

pref.corner$period <- as.numeric(as.character(pref.corner$period))
pref.corner %>% group_by(Genotype, period) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = period, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.1, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 2, 3, 4 )) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 1),  xlim = c(1.8 , 4.2), expand = TRUE) +
  ylab("Saccharin preference") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("intelli-pref.corner-082022.pdf", width = 7, height = 7, device = cairo_pdf)



###catwalk
source("catwalk-clean.R")
BOS_hind_males$Days <- as.numeric(as.character(BOS_hind_males$Days)) 
BOS_hind_males  %>% rename(Measurement = Mean) %>% select(-SEM) %>% # so that the Mean and SEM plot correctly
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(2, 4),  xlim = c(30, 90), expand = TRUE) +
  ylab("BOS hind [cm] males") +
  geom_text(aes(x = 60, y = 3.5, label = "*"), size = 10, colour = "black") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("bos-hind-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

BOS_hind_females$Days <- as.numeric(as.character(BOS_hind_females$Days))
BOS_hind_females  %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(2, 4),  xlim = c(30, 90), expand = TRUE) +
  ylab("BOS hind [cm] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("bos-hind-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

BOS_front_males$Days <- as.numeric(as.character(BOS_front_males$Days))
BOS_front_males  %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(1, 2),  xlim = c(30, 90), expand = TRUE) +
  ylab("BOS front [cm] males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("bos-front-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

BOS_front_females$Days <- as.numeric(as.character(BOS_front_females$Days))
BOS_front_females  %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(1, 2),  xlim = c(30, 90), expand = TRUE) +
  ylab("BOS front [cm] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("bos-front-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

H_MaxContact_males$Days <- as.numeric(as.character(H_MaxContact_males$Days))
H_MaxContact_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(150, 200),   xlim = c(30, 90), expand = TRUE) +
  ylab("H Max Contact Mean Intensity males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_maxcontact-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

H_MaxContact_females$Days <- as.numeric(as.character(H_MaxContact_females$Days))
H_MaxContact_females  %>% rename(Measurement = Mean) %>% select(-SEM) %>%
 group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(150, 200),   xlim = c(30, 90), expand = TRUE) +
  ylab("H Max Contact Mean Intensity females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_maxcontact-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

stepseq_AA_males$Days <- as.numeric(as.character(stepseq_AA_males$Days))
stepseq_AA_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 60),   xlim = c(30, 90), expand = TRUE) +
  ylab("Step Sequence AA [%] males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("stepseq-AA-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)


stepseq_AA_females$Days <- as.numeric(as.character(stepseq_AA_females$Days))
stepseq_AA_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 60),   xlim = c(30, 90), expand = TRUE) +
  ylab("Step Sequence AA [%] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("stepseq-AA-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_printarea_males$Days <- as.numeric(as.character(h_printarea_males$Days))
h_printarea_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 0.6),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws print area [cm^2] males") +
  geom_text(aes(x = 77, y = 0.5, label = "*"), size = 10, colour = "black") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_printarea-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_printarea_females$Days <- as.numeric(as.character(h_printarea_females$Days))
h_printarea_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 0.6),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws print area [cm^2] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_printarea-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_printwidth_males$Days <- as.numeric(as.character(h_printwidth_males$Days))
h_printwidth_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 2),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws print width [cm] males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_printwidth-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_printwidth_females$Days <- as.numeric(as.character(h_printwidth_females$Days))
h_printwidth_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 2),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws print width [cm] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_printwidth-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_swingspeed_males$Days <- as.numeric(as.character(h_swingspeed_males$Days))
h_swingspeed_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 150),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws swing speed [cm/s] males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_swingspeed-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_swingspeed_females$Days <- as.numeric(as.character(h_swingspeed_females$Days))
h_swingspeed_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 150),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws swing speed [cm/s] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_swingspeed-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_stepcycle_males$Days <- as.numeric(as.character(h_stepcycle_males$Days))
h_stepcycle_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 1.5),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws step cycle [s] males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_stepcycle-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

h_stepcycle_females$Days <- as.numeric(as.character(h_stepcycle_females$Days))
h_stepcycle_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 1.5),   xlim = c(30, 90), expand = TRUE) +
  ylab("Hind paws step cycle [s] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("h_stepcycle-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

couplings_rfrh_males$Days <- as.numeric(as.character(couplings_rfrh_males$Days))
couplings_rfrh_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(20, 100),   xlim = c(30, 90), expand = TRUE) +
  ylab("Couplings RF -> RH [%] males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("couplings_rfrh-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

couplings_rfrh_females$Days <- as.numeric(as.character(couplings_rfrh_females$Days))
couplings_rfrh_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(20, 100),   xlim = c(30, 90), expand = TRUE) +
  ylab("Couplings RF -> RH [%] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("couplings_rfrh-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

couplings_rhlf_males$Days <- as.numeric(as.character(couplings_rhlf_males$Days))
couplings_rhlf_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(20, 100),   xlim = c(30, 90), expand = TRUE) +
  ylab("Couplings RH -> LF [%] males") +
  geom_text(aes(x = 32, y = 95, label = "*"), size = 10, colour = "black") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("couplings_rhlf-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

couplings_rhlf_females$Days <- as.numeric(as.character(couplings_rhlf_females$Days))
couplings_rhlf_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(20, 100),   xlim = c(30, 90), expand = TRUE) +
  ylab("Couplings RH -> LF [%] females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("couplings_rhlf-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)

cadence_males$Days <- as.numeric(as.character(cadence_males$Days))
cadence_males %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#95c2db", "#1674cc")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 15),   xlim = c(30, 90), expand = TRUE) +
  ylab("Cadence males") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("cadence-males-082022.pdf", width = 7, height = 7, device = cairo_pdf)

cadence_females$Days <- as.numeric(as.character(cadence_females$Days))
cadence_females %>% rename(Measurement = Mean) %>% select(-SEM) %>%
  group_by(Genotype, Days) %>%
  summarize(Mean = mean(Measurement), SEM = sd(Measurement, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(stat = "identity", aes(x = Days, y = Mean, group = Genotype, colour = Genotype)) +
  geom_point(size = 10) +
  geom_line(linetype = "solid", size = 1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 3, size = 0.8) +
  scale_colour_manual(values = c("#ebabcf", "#d93290")) +
  theme_classic() +
  scale_x_continuous(breaks = c( 30, 40, 50, 60, 70, 80, 90) ) +
  coord_capped_cart(bottom = "both", left = "both" , gap = 0.03, ylim = c(0, 15),   xlim = c(30, 90), expand = TRUE) +
  ylab("Cadence females") +
  theme(aspect.ratio = 1.7,
        text = element_text(size = 22, color = "black", family = "Arial"),
        legend.position = "none",
        axis.ticks.length = unit(0.2, "cm"),
        axis.text = element_text(color = "black", size = 26),
        axis.title.y = element_text(vjust = +3))
#ggsave("cadence-females-082022.pdf", width = 7, height = 7, device = cairo_pdf)