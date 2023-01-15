# joint-aov.R - analysis of variance and post-hoc tests on joint data

stopifnot(exists("varname"))
stopifnot(class(varname) == "character")
stopifnot(exists(varname))
stopifnot(class(get(varname)) == "data.frame")
stopifnot(c("Animal", "Genotype", "Period") %in% colnames(get(varname)))

if (! exists("verbose")) verbose = FALSE

if (! exists("do.print")) do.print = FALSE

#pdf("joint-aov.pdf")
#sink("joint-aov.txt")

cat("joint multivariate Henze-Zirkler normality test for variables in the joint set :\n")
if (require(MVN))
{
    print(mvn(as.matrix(subset(get(varname), select = -c(Animal, Genotype, Period)))))
} else cat("MVN package missing, skipping multivariate normality test")

cat("univariate Shapiro-Wilk normality test individually for variables in the joint set :\n")
#print(t(sapply(tail(vars.select, -6), function(var) shapiro.test(get(varname)[, var]))))
tab.shapiro = t(sapply(subset(get(varname), select = -c(Animal, Genotype, Period)), shapiro.test))[, 1:2]
#signif.levels = c(1, 0.1, 0.05, 0.01, 0.001, 0)
signif.levels = c(0, 0.001, 0.01, 0.05, 0.1, 1)
stars = ordered(c("***", "** ", "*  ", ".  ", "   "))
tab.shapiro = data.frame(
    tab.shapiro,
    sig = stars[cut(as.numeric(tab.shapiro[, "p.value"]), signif.levels, labels = FALSE)]
)
print(tab.shapiro)

vars.dep = colnames(subset(get(varname), select = -c(Animal, Period, Genotype)))
joint.formula = as.formula(paste0("cbind(`", paste0(vars.dep, collapse = "` ,`"), "`) ~ Genotype * Period"))

model.aov = aov(joint.formula, data = get(varname))
print(model.aov)
print(manova(model.aov))

cat("between group variance equality tests...\n")
library(car) # leveneTest()
for (var in vars.dep)
{
    print(with(get(varname), eval(parse(text = paste0("bartlett.test(`", var, "`, Period)")))))
    print(with(get(varname), eval(parse(text = paste0("leveneTest(`", var, "`, Period)")))))
}
for (var in vars.dep)
{
    print(with(get(varname), eval(parse(text = paste0("bartlett.test(`", var, "`, Genotype)")))))
    print(with(get(varname), eval(parse(text = paste0("leveneTest(`", var, "`, Genotype)")))))
}
for (var in vars.dep)
{
    cat(paste0("\n`", var, "` ~ Genotype:Period\n"))
    print(eval(parse(text = paste0("leveneTest(`", var, "` ~ Genotype:Period, data = get(varname))"))))
}

library(agricolae) # HSD.test() etc.
cat("post hoc HSD tests...\n")
for (var in vars.dep)
{
    model.aov = aov(formula(paste0("`", var, "` ~ Genotype * Period")), data = get(varname))
    #TukeyHSD(model.aov, which = "Period")
    HSD.test(model.aov, "Genotype", console = do.print)
    HSD.test(model.aov, "Period", console = do.print)
}

cat("post hoc HSD tests...\n")
for (var in vars.dep)
{
    model.aov = aov(formula(paste0("`", var, "` ~ Genotype * Period")), data = get(varname))
    #TukeyHSD(model.aov, which = "Period")
    main = paste("HSD test for", var)
    sub = paste("model formula :", format(formula(model.aov)))
    plot(HSD.test(model.aov, "Genotype", console = do.print), main = main, sub = sub)
    plot(HSD.test(model.aov, "Period", console = do.print), main = main, sub = sub)
}

cat("post hoc SNK tests...\n")
for (var in vars.dep)
{
    model.aov = aov(formula(paste0("`", var, "` ~ Genotype * Period")), data = get(varname))
    main = paste("Student-Newman-Keuls for", var)
    sub = paste("model formula :", format(formula(model.aov)))
    plot(SNK.test(model.aov, "Genotype", console = do.print), main = main, sub = sub)
    plot(SNK.test(model.aov, "Period", console = do.print), main = main, sub = sub)
}

cat("post hoc LSD tests...\n")
for (var in vars.dep)
{
    model.aov = aov(formula(paste0("`", var, "` ~ Genotype * Period")), data = get(varname))
    main = paste("Least significant differneces for", var)
    sub = paste("model formula :", format(formula(model.aov)))
    plot(LSD.test(model.aov, "Genotype", console = do.print), main = main, sub = sub)
    plot(LSD.test(model.aov, "Period", console = do.print), main = main, sub = sub)
}

cat("post hoc Scheffé tests...\n")
for (var in vars.dep)
{
    model.aov = aov(formula(paste0("`", var, "` ~ Genotype * Period")), data = get(varname))
    main = paste("Scheffé for", var)
    sub = paste("model formula :", format(formula(model.aov)))
    plot(scheffe.test(model.aov, "Genotype", console = do.print), main = main, sub = sub)
    plot(scheffe.test(model.aov, "Period", console = do.print), main = main, sub = sub)
}

cat("combining post hoc overview tables...\n")

tests.list = list("HSD.test", "SNK.test", "LSD.test", "scheffe.test")
names(tests.list) = tests.list
periods = levels(get(varname)$Period)
g.types = levels(get(varname)$Genotype)
strat.tab.Genotype = list()
strat.tab.Period = list()
for (var in vars.dep)
{
    model.aov = aov(formula(paste0("`", var, "` ~ Genotype * Period")), data = get(varname))
    args.list = list(model.aov, "Genotype")
    strat.tab.Genotype[[var]] = sapply(tests.list, function(f) do.call(f, args.list)$groups[g.types, "groups"])
    dimnames(strat.tab.Genotype[[var]]) = list(g.types, tests.list)
    args.list = list(model.aov, "Period")
    strat.tab.Period[[var]] = sapply(tests.list, function(f) do.call(f, args.list)$groups[periods, "groups"])
    dimnames(strat.tab.Period[[var]]) = list(periods, tests.list)
}

print(lapply(lapply(strat.tab.Genotype, t), as.data.frame))
print(lapply(lapply(strat.tab.Period, t), as.data.frame))

#sink()
#dev.off()

