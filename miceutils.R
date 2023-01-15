# miceutils.R - utilities for convenient retrieval of mice attributes

# table with animal attributes
animals = read.csv("podzial_na_peletki.csv", sep = ";")
# fix according to analiza_lme4 :
# dane.long.2$Genotype[which(dane.long.2$Animal == "380F")] = "con"
animals[animals$Animal.ID == "380F", ]$Genotype = "con"

Animal_Param = function(animal, param = c("Genotype", "Pellet.type"))
    return(animals[match(animal, animals$Animal.ID), param])
Animal_Genotype = function(animal)
    return(Animal_Param(animal, "Genotype"))
Animal_Treatment = function(animal)
    return(Animal_Param(animal, "Pellet.type"))
Animal_Sex = function(animal)
    return(c("female", "male")[factor(substr(animal, 4, 5), levels = c("F", "M"))])

animals.tab = with(animals,
{
    Sex = Animal_Sex(Animal.ID); table(Genotype, Pellet.type, Sex)
})
summary(animals.tab)

library(bezier)
# build a better looking palette
Circular_Palette = function(n, alpha = NULL, invert = FALSE, reverse = FALSE)
{
    stopifnot(round(n) - n == 0)
    stopifnot(n > 1)
    control.points = t(matrix(c(0, 0.75, 0,  1, 0.5, 0,  1, 0, 0,  1, 0, 1,  0, 0, 1,  0, 0.5, 1,  0, 0.75, 0), nrow = 3, ncol = 7))
    # invert palette
    if(invert) control.points = 1 - control.points
    # reverse palette
    if(reverse) control.points = control.points[seq(5, 1), ]
    samples = seq(0, 3, 3 / n)
    # fix biased sample distribution
    #samples = 0.5 * samples + 0.5 * (sin(pi * samples / 2)) ** 2
    component = bezier(samples, control.points, deg = 2)
    if(is.null(alpha))
    {
        pal = rgb(component[, 1], component[, 2], component[, 3])
    }
    else
    {
        pal = rgb(component[, 1], component[, 2], component[, 3], alpha)
    }
    return (pal)
}
Palette = Circular_Palette

Bipolar_Palette = function(n, alpha = NULL, p = 1, invert = FALSE, reverse = FALSE)
{
    stopifnot(round(n) - n == 0)
    stopifnot(n > 1)
    
    # midpoint is black
    control.points = t(matrix(c(0, 1, 1,  0, 0, 1,  0, 0, 0,  1, 0, 0,  1, 1, 0), nrow = 3, ncol = 5))
    # invert palette
    if(invert) control.points = 1 - control.points
    # reverse palette
    if(reverse) control.points = control.points[seq(5, 1), ]
    samples = seq(-1, +1, 2 / n)
    samples = p * samples + (1 - p) * samples ** 3
    component = bezier(1 + samples, control.points, deg = 2)
    pal = rgb(component[, 1], component[, 2], component[, 3])
    return (pal)
}


# palette dimming by scaling with absolute value of a factor
# dim.factor is a numeric scalar in [-1, +1] :
# -1 shrinks to back
# negative value means shrink towards black by (1 + dim.factor) ; (0, 0, 0) is the zero point of scaling transformation
# 0 yields no change (identity function)
# positive value means shrink towards white by (1 - dim.factor); (1, 1, 1) is the zero point of scaling transformation
# 1 shrinks to all white
Dim = function(pal, dim.factor = 0)
{
    stopifnot(dim.factor >= -1)
    stopifnot(dim.factor <= +1)
    if(dim.factor > 0)
    {
        pal = rgb(t((1 - dim.factor ) * col2rgb(pal) + dim.factor * 255 * c(1, 1, 1)), maxColorValue = 255)
    }
    else
    {
        pal = rgb(t((1 + dim.factor) * col2rgb(pal)), maxColorValue = 255)
    }
    return(pal)
}


library(lattice) # levelplot()

Cor_Dist = function(cor.matrix)
{
    return (sqrt(1 - cor.matrix ** 2)) # sin
    #return (acos(cor.matrix) / pi)
    #return (1 - abs(acos(cor.matrix) / pi * 2 - 1)) # acute
    #return ((1 - cor.matrix) / 2) # = corrplot
}

Plot_Matrix = function(matrix.arg, title = NULL, C = 64)
{
    if (verbose) cat(format(match.call()))
    matrix.arg.name = match.call()["matrix.arg"]
    if (is.character(matrix.arg))
    {
        if (verbose) cat(" # matrix.arg =", matrix.arg)
        matrix.arg.name = matrix.arg
        matrix.arg = get(matrix.arg)
    }
    if (verbose) cat("\n")
    if (is.null(title)) title = paste(matrix.arg.name, "as color map")
    stopifnot (is.matrix(matrix.arg))
    stopifnot (class(title) == "character")
    stopifnot (class(C) %in% c("numeric", "integer"))
    stopifnot (C > 2)
    if (prod(range(matrix.arg)) < 0)
    {
        pal = Bipolar_Palette(C)
        samples = max(abs(matrix.arg)) * seq(-1, +1, 2 / C)
    }
    else
    {
        pal = gray(seq(1, C) / C)
        samples = max(abs(matrix.arg)) * seq(0, +1, 1 / C)
    }
    n = dim(matrix.arg)
    print(levelplot(matrix.arg, col.regions = pal, xlim = rep("", n[1]), ylim = rep("", n[2]), xaxt = "n",
        useRaster = TRUE, cuts = C + 1,
        at = samples, colorkey = list(at = samples, col = pal),
        main = title
    ))
}
