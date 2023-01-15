# catwalk-models.R - models fit to preprocessed catwalk data pointed to by varname
# this script must be called from within catwalk.R or afterwards

library(lme4)
library(MuMIn) # r.squaredGLMM

varname = "catwalk.clean" # temporarily fixed
# varname contains the name of the preprocessed dataset
# the dataset contains Animal, Genotype, Treatment, Sex, Date
# and a certain number of numerical attributes
stopifnot(exists(varname))
stopifnot(is.data.frame(get(varname)))
stopifnot(c("Animal", "Genotype", "Treatment", "Sex", "Date") %in% colnames(get(varname)))

Model_Accuracy = function(model, data = NULL)
{
    if (verbose) cat(format(match.call()), "\n")
    # all(predict(model.glm) == model.glm$linear.predictors) # TRUE
    # all(predict(model.glm, type = "response") == model.glm$fitted.values) # TRUE
    if (is.null(data)) data = model$data
    response = with(data, eval(summary(model)$terms[[2]]))
    stopifnot(is.factor(response))
    #levels(response)[predict(model.glm, type = "response") + 1.5]
    return(mean(levels(response)[predict(model, type = "response", newdata = data) + 1.5] == response))
}

#Model_Accuracy = function(model, data = NULL)
#{
#    return (mean(round(predict(model, type = "response", newdata = data)) == model$y))
#}

Print_Significant = function(p.val.tab)
{
    for(name in colnames(p.val.tab))
    {
        cat("variables with significant effect of", name, ":\n")
        p.val = p.val.tab[, name]
        i = p.val <= 0.001
        if(any(i)) cat(" (***) ", rownames(p.val.tab)[i], "\n")
        i = p.val > 0.001 & p.val <= 0.01
        if(any(i)) cat(" (**) ", rownames(p.val.tab)[i], "\n")
        i = p.val > 0.01 & p.val <= 0.05
        if(any(i)) cat(" (*) ", rownames(p.val.tab)[i], "\n")
        i = p.val > 0.05 & p.val <= 0.1
        if(any(i)) cat(" (.) ", rownames(p.val.tab)[i], "\n")
    }
}

model.glm = glm(Genotype ~ . - Animal - Sex - Treatment - Date, family = binomial(link = "logit"), data = get(varname))
#cat("model :", format(model.glm$call), "\n")
#print(summary(model.glm.3))
print(Model_Accuracy(model.glm))
print(head(sort((summary(model.glm)$coefficients)[, "Pr(>|z|)"])))

i.num.models = which(as.vector(sapply(get(varname), is.numeric)))

# linear regression model

cat("building multiple linear models regressing numerical variable on Genotype + Sex ...\n")
#model.lm = with(get(varname), lm(get(varname)[, i.num] ~ Genotype + Date + Sex))
model.mlm = with(get(varname), lm(as.matrix(get(varname)[, i.num.models]) ~ Genotype + Sex))
# retrive p-values and arrange as matrix
p.val.matrix = sapply(summary(model.mlm), function(m) m$coef[, 4])
df.genotype = data.frame(p.val = sort(p.val.matrix["Genotypemut", ]))
df.genotype$p.val.adjusted = p.adjust(df.genotype$p.val, "BY")
#$p.adjust(genotype.p.val, "BY")
cat("p-values before and after adjustment for response variables most significantly dependent on Genotype\n")
print(head(df.genotype))

df.sex = data.frame(p.val = sort(p.val.matrix["Sexmale", ]))
df.sex$p.val.adjusted = p.adjust(df.sex$p.val, "BY")
cat("p-values before and after adjustment for response variables most significantly dependent on Sex\n")
print(head(df.sex))


cat("building multiple linear models regressing numerical variable on Genotype * Sex ...\n")
model.mlm.2 = with(get(varname), lm(as.matrix(get(varname)[, i.num.models]) ~ Genotype * Sex))
# retrive p-values and arrange as matrix
p.val.matrix.2 = sapply(summary(model.mlm.2), function(m) m$coef[, 4])
df.genotype.2 = data.frame(p.val = sort(p.val.matrix.2["Genotypemut", ]))
df.genotype.2$p.val.adjusted = p.adjust(df.genotype.2$p.val, "BY")
#$p.adjust(genotype.p.val, "BY")
cat("p-values before and after adjustment for response variables most significantly dependent on Genotype\n")
print(head(df.genotype.2))

df.sex = data.frame(p.val = sort(p.val.matrix["Sexmale", ]))
df.sex$p.val.adjusted = p.adjust(df.sex$p.val, "BY")
cat("p-values before and after adjustment for response variables most significantly dependent on Sex\n")
print(head(df.sex))

#df.date = data.frame(p.val = sort(p.val.matrix["Date", ]))
#df.date$p.val.adjusted = p.adjust(df.date$p.val, "BY")
#print(df.date)

model.glm.mini = glm(Genotype ~ `BOS_HindPaws_Mean_(cm)`, family = binomial(link = "logit"), data = get(varname))
#cat("model :", format(model.glm$call), "\n")
print(summary(model.glm.mini))
print(Model_Accuracy(model.glm.mini))
print(head(sort((summary(model.glm.mini)$coefficients)[, "Pr(>|z|)"])))

LRT_Diag_Param_P = function(model, update.formula, do.print = FALSE)
{
    #stopifnot(is.character(param))
    model.reduced = update(model, update.formula)
    lrt = anova(model, model.reduced)
    if(do.print) print(lrt)
    return(lrt[["Pr(>Chisq)"]][2]) # return p-value
}

# try linear mixed effects model
library(lme4)

model.depend = "Genotype + Sex + Date + (1 | Animal)"
cat("mixed effects models ~", model.depend, "\n")
model.list.lmer = sapply(colnames(get(varname))[i.num.models],
    function(x) lmer(paste0("`", x, "` ~ ", model.depend),
    data = get(varname), REML = FALSE))

lmer.p.val = data.frame(
    Genot.p.val = sapply(model.list.lmer, LRT_Diag_Param_P, . ~ . - Genotype),
    Sex.p.val = sapply(model.list.lmer, LRT_Diag_Param_P, . ~ . - Sex),
    Date.p.val = sapply(model.list.lmer, LRT_Diag_Param_P, . ~ . - Date)
)
cat("All p-values before FDR adjustment :\n")
rownames(lmer.p.val) = substr(rownames(lmer.p.val), 1, 24) # shorten row names
print(lmer.p.val)
Print_Significant(lmer.p.val)
cat("Benjamini-Yekutieli adjusted Genotype p-values :\n")
print(p.adjust(lmer.p.val$Genot.p.val, "BY"))

model.depend = "Genotype * Sex * Date + (1 | Animal)"
cat("mixed effects models ~", model.depend, "\n")
model.list.lmer.int = sapply(colnames(get(varname))[i.num.models],
    function(x) lmer(paste0("`", x, "` ~ ", model.depend),
    data = get(varname), REML = FALSE))

lmer.int.p.val = data.frame(
    Genot.p.val = sapply(model.list.lmer.int, LRT_Diag_Param_P, . ~ Sex * Date + (1 | Animal)),
    Sex.p.val = sapply(model.list.lmer.int, LRT_Diag_Param_P, . ~ Genotype * Date + (1 | Animal)),
    Date.p.val = sapply(model.list.lmer.int, LRT_Diag_Param_P, . ~ Genotype * Sex + (1 | Animal))
)
rownames(lmer.int.p.val) = substr(rownames(lmer.int.p.val), 1, 24) # shorten row names for legibility
print(lmer.int.p.val)
Print_Significant(lmer.int.p.val)
cat("Benjamini-Yekutieli adjusted Genotype p-values :\n")
print(p.adjust(lmer.int.p.val$Genot.p.val, "BY"))


model.depend.fixed = "Genotype + Sex + Date"
model.list.lm = lapply(colnames(get(varname))[i.num.models],
    function(x) lm(paste0("`", x, "` ~ ", model.depend.fixed),
    data = get(varname)))
r.sq.fixed = unlist(sapply(model.list.lm, function(m) summary(m)$r.squared ))
r.sq.adj.fixed = unlist(sapply(model.list.lm, function(m) summary(m)$adj.r.squared ))

model.depend = "Genotype + Sex + Date + (1 | Genotype : Animal)"
cat("mixed effects models ~", model.depend, "\n")
model.list.lmer.3 = sapply(colnames(get(varname))[i.num.models],
    function(x) lmer(paste0("`", x, "` ~ ", model.depend),
    data = get(varname), REML = FALSE))
lmer.3.p.val = data.frame(
    Genot.p.val = sapply(model.list.lmer.3, LRT_Diag_Param_P, . ~ . - Genotype),
    Sex.p.val = sapply(model.list.lmer.3, LRT_Diag_Param_P, . ~ . - Sex),
    Date.p.val = sapply(model.list.lmer.3, LRT_Diag_Param_P, . ~ . - Date)
)
r.sq.mixed = t(unlist(sapply(model.list.lmer.3, r.squaredGLMM)))
# Plot

pdf(file = "catwalk-models.pdf")
plot(rep(seq_along(r.sq.mixed[, 1]), 2), r.sq.mixed,
    main = paste("lmer model . ~", model.depend, "
    r-squared as bars (lower = marginal, upper = conditional),
    lm model . ~", model.depend.fixed, "r.squared (red), adj.r.squared (green)")
)
segments(seq_along(r.sq.mixed[, 1]), r.sq.mixed[, 1], y1 = r.sq.mixed[, 2])
points(seq_along(r.sq.fixed), r.sq.fixed, col = "red", new = TRUE)
points(seq_along(r.sq.fixed), r.sq.adj.fixed, col = "green", new = TRUE)
dev.off()


cat("All p-values before FDR adjustment :\n")
rownames(lmer.3.p.val) = substr(rownames(lmer.3.p.val), 1, 24) # shorten row names
print(lmer.3.p.val)
Print_Significant(lmer.3.p.val)
cat("Benjamini-Yekutieli adjusted Genotype p-values :\n")
print(p.adjust(lmer.3.p.val$Genot.p.val, "BY"))

# 12.07.2021 - badanie osobliwoœci modeli
pdf(file = "catwalk-singular.pdf")
i.sing = which(sapply(model.list.lmer, isSingular))
for (z in names(i.sing))
{
    plot(formula(paste0("`", z, "` ~ Date")), data = get(varname), col = Genotype)
    with(get(varname), eval(parse(text = paste0("plot(table(`", z, "`))"))))
    # which groups (Animal level) have zero variance ?
    #print(which(as.list(by(catwalk.clean, catwalk.clean$Animal, function(t) var(t[, z]))) == 0))
}
dev.off()
