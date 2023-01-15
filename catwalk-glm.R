# catwalk-glm.R - variable selection for the logistic regression for catwalk
# this script must be called from within catwalk.R or afterwards
# varname contains name of the dataset to use

if(! exists("varname")) varname = "catwalk.clean"
stopifnot(is.character(varname))
stopifnot(exists(varname))
stopifnot(is.data.frame(get(varname)))
stopifnot(! any(is.na(get(varname))))

if (! exists("base.formula")) base.formula = Genotype ~ Sex + Days
base.vars = rownames(attr(terms(base.formula), "factors"))
if (! exists("required.vars")) required.vars = c("Animal", "Sex", "Genotype", "Treatment", "Date", "Days")
stopifnot(required.vars %in% colnames(get(varname)))

if (! exists("verbose")) verbose = FALSE
stopifnot(is.logical(verbose))

cat("base variables used in the model :\n")
print(with(catwalk.clean, sapply(base.vars, function(var) class(eval(parse(text = var))))))

library(lme4)

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

# apply N-fold crossvalidation to assess the real classification error/accuracy
Crossvalidation = function(base.model, N = 10)
{
    if (verbose) cat(format(match.call()), "\n")

    group = sample(seq_along(rownames(base.model$data)) %% N + 1)
    cv.acc = sapply(1 : N, function(n) {
        data.train = subset(base.model$data, group != n)
        data.validate = subset(base.model$data, group == n)
        model.cv = glm(formula(base.model), data = data.train, family = binomial)
        Model_Accuracy(model.cv, data = data.validate)
    })
    if (verbose) print(cv.acc)

    return(mean(cv.acc))
}

#lower.formula = formula(paste(required.vars[1], "~", paste(tail(required.vars, -1), collapse = "+")))
cat("variable selection in logistic regression with base model :", format(base.formula), "\n")

GLM_Stepwise_Var_Selection = function(base.model)
{
    if (verbose) cat(format(match.call()), "\n")

    data = base.model$data
    # select only numeric variables from the dataset into search scope
    var.list = paste0("`", colnames(data[, sapply(data, is.numeric)]), "`")
    lower.formula = formula(base.model)
    #formula(paste(required.vars[1], "~", paste(tail(required.vars, -1), collapse = "+")))
    upper.formula = formula(paste(format(lower.formula), "* (1 + ", paste(var.list, collapse = "+"), ")"))
    #model.0 = glm(lower.formula, data = data, family = binomial)
    #with(data, data.frame(pred = levels(Genotype)[predict(model.0, type = "response") + 1], true = Genotype))
    #mean(with(catwalk.clean, levels(Genotype)[predict(model.0, type = "response") + 1] ==  Genotype))
    model.select = step(base.model, scope = list(lower = lower.formula, upper = upper.formula, direction = "both"))
    #cat(paste("Base model accuracy :", Model_Accuracy(model.0), "\n"))
    #cat(paste("Selected model accuracy :", Model_Accuracy(model.select), "\n"))
    print(formula(model.select))

    return(model.select)
}

model.0 = glm(base.formula, data = get(varname), family = binomial)
model.select = GLM_Stepwise_Var_Selection(model.0)

terms.select = attr(terms(model.select), "term.labels")
cat("selected terms :", terms.select, sep = "\n")
selected.vars = rownames(attr(terms(model.select), "factors"))

# not all predictors are statistically significant
print(summary(model.select))

# further reduce the model
GLM_Significant = function(model.select)
{
    if (verbose) cat(format(match.call()), "\n")

    i = which(summary(model.select)$coefficients[, 4] < 0.05)
    labels(terms(formula(model.select)))
    #terms.signif = c(1, colnames(attr(terms(model.select), "factors")))[i]
    terms.signif = c(1, labels(terms(formula(model.select))))[i]
    #terms.signif = attr(terms(model.select), "term.labels")
    formula.signif = as.formula(paste(c("Genotype ~ 1", terms.signif), collapse = "+"))
    model.signif = glm(formula.signif, data = model.select$data, family = binomial)

    return (model.signif)
}
model.signif = GLM_Significant(model.select)

# some predictors can still lose significance !
print(summary(model.signif))

terms.signif = attr(terms(model.signif), "term.labels")
cat("selected significant terms :", terms.signif, sep = "\n")
vars.signif = rownames(attr(terms(model.signif), "factors"))

cat(paste("Base model accuracy :",
    Model_Accuracy(model.0), "(reclassification)",
    Crossvalidation(model.0), "(cross-validation)\n"))

cat(paste("Selected model accuracy :",
    Model_Accuracy(model.select), "(reclassification)",
    Crossvalidation(model.select), "(cross-validation)\n"))

cat(paste("Reduced model accuracy :",
    Model_Accuracy(model.signif), "(reclassification)",
    Crossvalidation(model.signif), "(cross-validation)\n"))

# finalize variable selection
if (! exists("do.select.significant")) do.select.significant = FALSE
stopifnot(is.logical(do.select.significant))
if (do.select.significant) {
    vars.select = gsub("`", "", setdiff(vars.signif, required.vars))
} else {
    vars.select = gsub("`", "", setdiff(selected.vars, required.vars))
}
catwalk.select = catwalk.reduced[, c(required.vars, vars.select)]

if (FALSE)
{
pdf("catwalk-glm.pdf")

# draw distributions
pal = Circular_Palette(2, alpha = 0.125)
plot(get(varname)[, gsub("`", "", vars.signif)], col = pal[get(varname)$Genotype], pch = 16)

for (z in setdiff(vars.signif, required.vars))
{
    plot(formula(paste(z, "~ Date")), data = get(varname), col = Genotype)
    #with(get(varname), plot(table(eval(parse(text = z)))))
    with(get(varname), eval(parse(text = paste("hist(", z, ", breaks = 50)"))))
}
i.couplings = grep("Couplings_*", colnames(catwalk.clean))
sapply(i.couplings, function(i) Plot_Hist_Groups(i, "catwalk.clean", groupname = "Genotype"))

dev.off()
}

# do we fit linear models for each variable ?
if (FALSE)
{

cat("fitting linear models ...\n")
lm.list = lapply(setdiff(vars.signif, required.vars), function(term) {
    formula.model = formula(paste(term, "~", paste(base.vars, collapse = "+")))
    lm(formula.model, data = model.select$data)
})
names(lm.list) = setdiff(vars.signif, required.vars)
p.val.lm = t(sapply(lm.list, function(model) {
    fstat = summary(model)$fstatistic
    p.val = pf(fstat[1], df1 = fstat[2], df2 = fstat[3], lower.tail = FALSE)
    c(model = p.val, summary(model)$coefficients[, 4])
}))
print(p.val.lm)

cat("fitting linear mixed effects models ...\n")
lmer.list = lapply(setdiff(vars.signif, required.vars), function(term) {
    formula.model = formula(paste(term, "~ (1 | Animal) +", paste(base.vars, collapse = "+")))
    lmer(formula.model, data = model.select$data, REML = FALSE)
})
names(lmer.list) = setdiff(vars.signif, required.vars)
}
