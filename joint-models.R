# joint-models.R - fitting models for combined data sets
# this script must be called from within joint.R or afterwards

cat("\nfitting models to catwalk data...\n")

sink("joint-models.txt")
pdf(file = "joint-models.pdf")

library(MuMIn) # r.squaredGLMM

model.lm = lm(`BOS_HindPaws_Mean_(cm)` ~ Days * Genotype, data = catwalk.filtered.m)
summary(model.lm)
#plot(`BOS_HindPaws_Mean_(cm)` ~ Days, col = Genotype, data = catwalk.filtered.m)

model.lm.0 = lm(`BOS_HindPaws_Mean_(cm)` ~ 1, data = catwalk.filtered.m)
model.lm.1 = lm(`BOS_HindPaws_Mean_(cm)` ~ Days, data = catwalk.filtered.m)
print(drop1(model.lm.1, test = "Chisq"))
print(anova(model.lm.0, model.lm.1))
model.lm.2 = lm(`BOS_HindPaws_Mean_(cm)` ~ Genotype, data = catwalk.filtered.m)
print(drop1(model.lm.2, test = "Chisq"))
print(anova(model.lm.0, model.lm.2))
model.lm.3 = lm(`BOS_HindPaws_Mean_(cm)` ~ Days + Genotype, data = catwalk.filtered.m)
print(drop1(model.lm.3, test = "Chisq"))
print(anova(model.lm.1, model.lm.3))
print(anova(model.lm.2, model.lm.3))
model.lm.4 = lm(`BOS_HindPaws_Mean_(cm)` ~ Days * Genotype, data = catwalk.filtered.m)
print(drop1(model.lm.4, test = "Chisq"))
print(anova(model.lm.3, model.lm.4))
print(anova(model.lm.2, model.lm.4))

model.lmer.0 = lmer(`BOS_HindPaws_Mean_(cm)` ~ (1 | Genotype : Animal), data = catwalk.filtered.m, REML = FALSE)
model.lmer.1 = lmer(`BOS_HindPaws_Mean_(cm)` ~ Days + (1 | Genotype : Animal), data = catwalk.filtered.m, REML = FALSE)
print(drop1(model.lmer.1, test = "Chisq"))
print(anova(model.lmer.0, model.lmer.1))
model.lmer.2 = lmer(`BOS_HindPaws_Mean_(cm)` ~ Genotype + (1 | Genotype : Animal), data = catwalk.filtered.m, REML = FALSE)
print(drop1(model.lmer.2, test = "Chisq"))
print(anova(model.lmer.0, model.lmer.2))
model.lmer.3 = lmer(`BOS_HindPaws_Mean_(cm)` ~ Days + Genotype + (1 | Genotype : Animal), data = catwalk.filtered.m, REML = FALSE)
print(drop1(model.lmer.3, test = "Chisq"))
#          npar    AIC    LRT Pr(Chi)  
# <none>        2.7755                 
# Days        1 0.8049 0.0293 0.86402  
# Genotype    1 6.6827 5.9072 0.01508 *
print(anova(model.lmer.1, model.lmer.3))
print(anova(model.lmer.2, model.lmer.3))
model.lmer.4 = lmer(`BOS_HindPaws_Mean_(cm)` ~ Days * Genotype + (1 | Genotype : Animal), data = catwalk.filtered.m, REML = FALSE)
print(drop1(model.lmer.4, test = "Chisq"))
print(anova(model.lmer.3, model.lmer.4))
print(anova(model.lmer.2, model.lmer.4))
#              npar      AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)  
# model.lmer.2    4  0.80488 15.272 3.5976  -7.1951                       
# model.lmer.4    6 -0.76364 20.937 6.3818 -12.7636 5.5685  2    0.06177 .
# run permutation test to be on the safe side
N = 0 # 5000
if(N > 0)
{
    ll = replicate(N, logLik(lmer(`BOS_HindPaws_Mean_(cm)` ~ sample(Days) * Genotype + (1 | Genotype : Animal),
        data = catwalk.filtered.m, REML = FALSE)) )
    cat("permutation test for Days in model ~ Days * Genotype + (1 | Genotype : Animal) :",
        mean(ll > logLik(model.lmer.4)), "\n")
    # permutation test for Days in model ~ Days * Genotype + (1 | Genotype : Animal) : 0.0584
}

cat("R squared for fixed-only/mixed models\n")
cat(format(model.lm.2$call), ":", summary(model.lm.2)$r.squared, "\n")
cat(format(model.lmer.2@call), ":", r.squaredGLMM(model.lmer.2), "\n")
delta.ll = c(logLik(model.lm.2) - logLik(model.lmer.2))
cat("LRT test for random effect :", pchisq(-2 * delta.ll, df = 1, lower.tail = FALSE), "\n")

cat("R squared for fixed-only/mixed models\n")
cat(format(model.lm.3$call), ":", summary(model.lm.3)$r.squared, "\n")
cat(format(model.lmer.3@call), ":", r.squaredGLMM(model.lmer.3), "\n")
delta.ll = c(logLik(model.lm.3) - logLik(model.lmer.3))
cat("LRT test for random effect :", pchisq(-2 * delta.ll, df = 1, lower.tail = FALSE), "\n")

cat("R squared for fixed-only/mixed models\n")
cat(format(model.lm.4$call), ":", summary(model.lm.4)$r.squared, "\n")
cat(format(model.lmer.4@call), ":", r.squaredGLMM(model.lmer.4), "\n")
delta.ll = c(logLik(model.lm.4) - logLik(model.lmer.4))
cat("LRT test for random effect :", pchisq(-2 * delta.ll, df = 1, lower.tail = FALSE), "\n")

summary(model.lmer.4)

n.levels = nlevels(as.factor(catwalk.filtered.m$Days))

plot(`BOS_HindPaws_Mean_(cm)` ~ interaction(Days, Genotype),
    col = rep(Circular_Palette(2, alpha = 0.5)[1:2], each = n.levels), data = catwalk.filtered.m)
legend("topright", legend = levels(catwalk.filtered.m$Genotype), fill = Circular_Palette(2, alpha = 0.5))

plot(`BOS_HindPaws_Mean_(cm)` ~ interaction(Genotype, Days),
    col = rep(Circular_Palette(2, alpha = 0.5)[1:2], n.levels), data = catwalk.filtered.m)
legend("topright", legend = levels(catwalk.filtered.m$Genotype), fill = Circular_Palette(2, alpha = 0.5))


cat("\nfitting models to OSS data...\n")
#cat("\n--- OSS ---\n\n")

oss.model.lm.0 = lm(Correct ~ 1, data = oss)
oss.model.lm.1 = lm(Correct ~ Days, data = oss)
print(anova(oss.model.lm.0, oss.model.lm.1))
oss.model.lm.2 = lm(Correct ~ Genotype, data = oss)
print(anova(oss.model.lm.0, oss.model.lm.2))
oss.model.lm.3 = lm(Correct ~ Days + Genotype, data = oss)
print(anova(oss.model.lm.1, oss.model.lm.3))
print(anova(oss.model.lm.2, oss.model.lm.3))
print(drop1(oss.model.lm.3, test = "Chisq"))
oss.model.lm.4 = lm(Correct ~ Days * Genotype, data = oss)
print(anova(oss.model.lm.3, oss.model.lm.4))
print(drop1(oss.model.lm.4, test = "Chisq"))

oss.correct.lm = lm(Correct ~ Days - 1, data = oss)

#oss.correct.lmer.0 = lmer(Correct ~ (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)
#oss.correct.lmer.1 = lmer(Correct ~ Days + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)
#oss.correct.lmer = lmer(Correct ~ Days * Genotype - 1 + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)

oss.correct.lmer = lmer(Correct ~ Days : Genotype - 1 + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)
#oss.correct.lmer = lmer(Correct ~ Genotype / Days - 1 + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)

summary(oss.correct.lmer)
print(drop1(oss.correct.lmer, test = "Chisq"))
oss.correct.lmer.ref = lmer(Correct ~ Days : Genotype - 1 + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)
#predict(oss.correct.lmer, newdata = data.frame(Animal = as.factor("383M"), Genotype = Animal_Genotype("383M"), Days = 1))

# cannot do this - nesting relation does not hold
#oss.correct.lmer.0 = lmer(Correct ~ Days - 1 + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)
#anova(oss.correct.lmer.0, oss.correct.lmer)
# this should be fine (?)
oss.correct.lmer.0 = lmer(Correct ~ 0 + (Days - 1 | Genotype : Animal), data = oss, REML = FALSE)
print(anova(oss.correct.lmer.0, oss.correct.lmer))

# retrieve random effects
random.effects = ranef(oss.correct.lmer)[[1]]
random.effects$Genotype = as.factor(substr(rownames(random.effects), 1, 3))
plot(Days ~ Genotype, data = random.effects, main = "random effects in model
    Correct ~ Days : Genotype - 1 + (Days - 1 | Genotype : Animal)")
# plot for a different model in comparison
oss.correct.lmer.ref = lmer(Correct ~ Days : Genotype - 1 + (Days - 1 | Animal), data = oss, REML = FALSE)
random.effects = ranef(oss.correct.lmer.ref)[[1]]
random.effects$Genotype = Animal_Genotype(rownames(random.effects))
plot(Days ~ Genotype, data = random.effects, main = "random effects in model
    Correct ~ Days : Genotype - 1 + (Days - 1 | Animal)")

oss.correct.lm = lm(Correct ~ Days : Genotype, data = oss)
delta.ll = c(logLik(oss.correct.lm) - logLik(oss.correct.lmer))
cat("LRT test for random effect :", pchisq(-2 * delta.ll, df = 1, lower.tail = FALSE), "\n")

# check if random effects are equal
#all(sort(ranef(oss.correct.lmer)[[1]]$Days) == sort(ranef(oss.correct.lmer.ref)[[1]]$Days)) # TRUE

# table of representatives of all animals + Genotype
represent.tab = data.frame(Animal = unique(oss$Animal), Genotype = Animal_Genotype(unique(oss$Animal)))
represent.tab$Days = 0
represent.tab$Intercept = predict(oss.correct.lmer, newdata = represent.tab)
represent.tab$Days = 1
represent.tab$Slope = predict(oss.correct.lmer, newdata = represent.tab) - represent.tab$Intercept

varname = "oss"
print(
    ggplot(data = get(varname),
        aes(x = Days, y = Correct, colour = Genotype)
    ) +
    geom_line() +
    geom_point(color = "black") +
    facet_wrap(~ Animal) +
    geom_abline(data = represent.tab, aes(intercept = Intercept, slope = Slope), colour = "gray") +
    theme(legend.position = "bottom") +
    ggtitle(paste("Individual measurements in groups -", varname),
        subtitle = format(formula(oss.correct.lmer))) +
    theme(plot.title = element_text(face = "bold"))
)

print(
    ggplot(data = get(varname),
        aes(x = Days, y = Incorrect/(Correct + Incorrect), colour = Genotype)
    ) +
    geom_line() +
    geom_point(color = "black") +
    facet_wrap(~ Animal) +
    theme(legend.position = "bottom") +
    ggtitle(paste("Individual measurements in groups -", varname)) +
    theme(plot.title = element_text(face = "bold"))
)

cat("\nfitting models to OSS data without training period...\n")

# additional analysis, with training sessions removed
oss.trained = subset(oss, Days > 5)

oss.trained.correct.lmer.2 = lmer(Correct ~ Days * Genotype + (1 | Genotype : Animal), data = oss.trained, REML = FALSE)
print(summary(oss.trained.correct.lmer.2))
print(drop1(oss.trained.correct.lmer.2, test = "Chisq"))
oss.trained.correct.lmer.3 = lmer(Correct ~ Days * Genotype + (Days | Genotype : Animal), data = oss.trained, REML = FALSE)
print(summary(oss.trained.correct.lmer.3))
# convergence problem
#print(drop1(oss.correct.lmer.3, test = "Chisq"))
# explicit : would not converge - have to fiddle with lmerConrols (to do later)
#oss.correct.lmer.ref = lmer(Correct ~ Days + Genotype + (Days | Genotype : Animal), data = oss.trained, REML = FALSE)

print(with(ranef(oss.trained.correct.lmer.3)[[1]], shapiro.test(`(Intercept)`)))
print(with(ranef(oss.trained.correct.lmer.3)[[1]], shapiro.test(Days)))

with(ranef(oss.trained.correct.lmer.3)[[1]], hist(`(Intercept)`, main = "random effect of Animal"))
with(ranef(oss.trained.correct.lmer.3)[[1]], hist(Days, main = "random effect of Animal"))


oss.trained.correct.lmer.4 = lmer(Correct ~ Days + Genotype + (1 | Genotype : Animal), data = oss.trained, REML = FALSE)
print(drop1(oss.trained.correct.lmer.4, test = "Chisq"))

oss.trained.correct.lmer.5 = lmer(Correct ~ Days + (1 | Genotype : Animal), data = oss.trained, REML = FALSE)

print(anova(oss.trained.correct.lmer.2, oss.trained.correct.lmer.3))
print(anova(oss.trained.correct.lmer.2, oss.trained.correct.lmer.4))
print(anova(oss.trained.correct.lmer.2, oss.trained.correct.lmer.5))

oss.trained.model.lm.2 = lm(Correct ~ Days * Genotype, data = oss.trained)
delta.ll = c(logLik(oss.trained.model.lm.2) - logLik(oss.trained.correct.lmer.2))
cat("LRT test for random effect :", pchisq(-2 * delta.ll, df = 1, lower.tail = FALSE), "\n")


# table of representatives of all animals + Genotype
represent.tab = data.frame(Animal = unique(oss$Animal), Genotype = Animal_Genotype(unique(oss$Animal)))
represent.tab$Days = 0
represent.tab$Intercept = predict(oss.trained.correct.lmer.3, newdata = represent.tab)
represent.tab$Days = 1
represent.tab$Slope = predict(oss.trained.correct.lmer.3, newdata = represent.tab) - represent.tab$Intercept

varname = "oss.trained"
print(
    ggplot(data = get(varname),
        aes(x = Days, y = Correct, colour = Genotype)
    ) +
    geom_line() +
    geom_point(color = "black") +
    facet_wrap(~ Animal) +
    geom_abline(data = represent.tab, aes(intercept = Intercept, slope = Slope), colour = "gray") +
    theme(legend.position = "bottom") +
    ggtitle(paste("Individual measurements in groups -", varname),
        subtitle = format(formula(oss.trained.correct.lmer.3))) +
    theme(plot.title = element_text(face = "bold"))
)

represent.tab = data.frame(Animal = unique(oss$Animal), Genotype = Animal_Genotype(unique(oss$Animal)))
represent.tab$Days = 0
represent.tab$Intercept = predict(oss.trained.correct.lmer.2, newdata = represent.tab)
represent.tab$Days = 1
represent.tab$Slope = predict(oss.trained.correct.lmer.2, newdata = represent.tab) - represent.tab$Intercept

varname = "oss.trained"
print(
    ggplot(data = get(varname),
        aes(x = Days, y = Correct, colour = Genotype)
    ) +
    geom_line() +
    geom_point(color = "black") +
    facet_wrap(~ Animal) +
    geom_abline(data = represent.tab, aes(intercept = Intercept, slope = Slope), colour = "gray") +
    theme(legend.position = "bottom") +
    ggtitle(paste("Individual measurements in groups -", varname),
        subtitle = format(formula(oss.trained.correct.lmer.2))) +
    theme(plot.title = element_text(face = "bold"))
)

dev.off()
sink()

if(do.view) system("xdg-open joint-models.pdf")


