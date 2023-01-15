# joint.R - plots for joint analysis of selected features from all data sets
# this script must be called from within joint.R or afterwards (uses pre-computed variables)

# male sets
stopifnot(sapply(c("saccharin.clean", "oss.m", "catwalk.m", "weight.m", "olfactory.clean"), exists))
# female sets
stopifnot(sapply(c("catwalk.f", "weight.f", "intelli.clean"), exists))

library(ggplot2) # ggplot

# create overview dataframe for plotting
# males
time.course = rbind(
    data.frame(
        Date = unique(as.Date(saccharin.clean$Start)),
        Experiment = factor("Saccharin")),
    data.frame(
        Date = unique(as.Date(oss.m$Date)),
        Experiment = factor("OSS")),
    data.frame(
        Date = unique(as.Date(catwalk.m$Date)),
        Experiment = factor("Catwalk")),
    data.frame(
        Date = unique(as.Date(weight.m$Date)),
        Experiment = factor("Weight")),
    data.frame(
        Date = unique(as.Date(olfactory.clean$Date)),
        Experiment = factor("Olfactory"))
)
start.date = as.Date(experiment.start, format = date.format)
week.seq = data.frame(
    Sat = start.date + as.difftime(7, units = "days") * seq(0, 18) - 1,
    Sun = start.date + as.difftime(7, units = "days") * seq(0, 18),
    Mon = start.date + as.difftime(7, units = "days") * seq(0, 18) + 1
)

theme_set(theme_classic())
time.plot = ggplot(data = time.course, mapping = aes(x = Date, y = Experiment))
time.plot = time.plot + geom_point()
time.plot = time.plot + geom_vline(data = week.seq,
    mapping = aes(xintercept = Sun), size = 2, colour = "red", alpha = 0.25)
time.plot = time.plot + geom_vline(data = data.frame(Boundary),
    mapping = aes(xintercept = as.Date(Boundary)), linetype = "dashed")
print(time.plot)

# females

time.course.f = rbind(
    data.frame(
        Date = unique(as.Date(catwalk.f$Date)),
        Experiment = factor("Catwalk")),
    data.frame(
        Date = unique(as.Date(weight.f$Date)),
        Experiment = factor("Weight")),
    data.frame(
        Date = unique(as.Date(intelli.clean$StartDateTime)),
        Experiment = factor("Intelli"))
)

theme_set(theme_classic())
time.plot = ggplot(data = time.course.f, mapping = aes(x = Date, y = Experiment))
time.plot = time.plot + geom_point()
time.plot = time.plot + geom_vline(data = week.seq,
    mapping = aes(xintercept = Sun), size = 2, colour = "red", alpha = 0.25)
time.plot = time.plot + geom_vline(data = data.frame(Boundary),
    mapping = aes(xintercept = as.Date(Boundary)), linetype = "dashed")
print(time.plot)

# hierarchic clustering for attributes -- males
Plot_Matrix(cor.matrix.m)
Plot_Matrix(cor.matrix.m.complete)
#Plot_Matrix(dist.matrix.m.complete)
#plot(hc.complete, labels = FALSE)
lab = hc.complete.m$labels
#lab[11:length(lab)] = ""
plot(hc.complete.m, hang = -1, labels = lab, cex = 0.75, lwd = 0.5, main = "Variable clustering tree - males")
#abline(sqrt(1 - 0.5 ** 2), 0, col = "red", lty = 2)
abline(sqrt(1 - cor.threshold.m ** 2), 0, col = "red")

# draw histograms of variables in the joint set

# hierarchic clustering for attributes -- females
# output plots
Plot_Matrix(cor.matrix.f)
Plot_Matrix(cor.matrix.f.complete)
#Plot_Matrix(dist.matrix.f.complete)
#plot(hc.complete, labels = FALSE)
lab = hc.complete.f$labels
#lab[11:length(lab)] = ""
plot(hc.complete.f, hang = -1, labels = lab, cex = 0.75, lwd = 0.5, main = "Variable clustering tree - females")
#abline(sqrt(1 - 0.5 ** 2), 0, col = "red", lty = 2)
abline(sqrt(1 - cor.threshold.f ** 2), 0, col = "red")

# plot histograms of selected variables after binning
if (FALSE)
{
sapply(colnames(subset(joint.data.m, select = -c(Animal, Genotype, Period))),
    function(var) hist(joint.data.m[, var], breaks = 50, main = paste("Histogram of", var, "in joint.data.m")))

sapply(colnames(subset(joint.data.f, select = -c(Animal, Genotype, Period))),
    function(var) hist(joint.data.f[, var], breaks = 50, main = paste("Histogram of", var, "in joint.data.f")))
}
