# joint-cor.R  - analysis of correlations of joint data
# this script must be called from within joint.R or afterwards

if (! exists("plot.file")) plot.file = "joint-cor.pdf"
if (is.character(plot.file)) pdf(file = plot.file, title = plot.file)

# Bonferroni (or another) adjustment to account for the number of pairwise correlations
if (! exists("correction")) correction = "bonferroni"

# define data to be analyzed
if (! exists("varname")) varname = "joint.data.f"

data.num = subset(get(varname), select = sapply(get(varname), is.numeric))

R_na_P = function(r.matrix, deg.f)
{
    r.dist = as.dist(r.matrix) # takes everything above diagonal
    r.tab = as.numeric(r.dist)
    t.tab = sqrt(deg.f) * r.tab / sqrt(1 - r.tab ** 2)
    p.tab = 2 * pt(-abs(t.tab), deg.f)
    return (p.tab)
}

P_na_R = function(r.matrix, deg.f, p.tab)
{
    r.dist = as.dist(r.matrix) # takes everything above diagonal
    t.tab = - qt(p.tab / 2, deg.f) * sign(as.numeric(r.dist))
    r.tab = t.tab / sqrt(t.tab ** 2 + deg.f)
    #plot(r.tab, r.tab.adj, pch = ".", ylim = c(-1, 1))
    r.dist[seq(length(r.tab))] = r.tab
    #R_na_P = function(r.matrix)
    r.matrix = as.matrix(r.dist)
    diag(r.matrix) = 1
    return (r.matrix)
}

Plot_R_Adjustment(data, correction = "bonferroni")
{
    stopifnot(is.character(correction))
    deg.f = nrow(data.num) - 2
    r.matrix = cor(data.num, use = "complete.obs")
    r.tab = as.numeric(as.dist(r.matrix))
    p.tab = R_na_P(r.matrix, deg.f)
    p.tab.adj = list()
    r.tab.adj = list()
    #p.tab.adj[[c]]
    for (c in correction)
    {
        p.tab.adj[[c]] = p.adjust(p.tab, c)
        r.matrix = P_na_R(r.matrix, deg.f, p.tab.adj[[c]])
        r.tab.adj[[c]] = as.numeric(as.dist(r.matrix))
    }
    plot.ts(as.ts(r.tab), as.ts(as.data.frame(r.tab.adj)), ylim = c(-1, +1))
}

deg.f = nrow(data.num) - 2
r.matrix = cor(data.num, use = "complete.obs")
r.dist = as.dist(r.matrix) # takes everything above diagonal
r.tab = as.numeric(r.dist)
t.tab = sqrt(deg.f) * r.tab / sqrt(1 - r.tab ** 2)
p.tab = 2 * pt(-abs(t.tab), deg.f)
plot(p.tab, pch = ".")

p.tab.adj = p.adjust(p.tab, correction)
#p.tab.adj = sapply(p.tab * (ncol(data.num) - 1), function(x) min(x, 1))

plot(p.tab.adj, pch = ".")
t.tab.adj = - qt(p.tab.adj / 2, deg.f) * sign(t.tab)
r.tab.adj = t.tab.adj / sqrt(t.tab.adj ** 2 + deg.f)
plot(r.tab, r.tab.adj, pch = ".", ylim = c(-1, 1))
r.dist.adj = r.dist
r.dist.adj[seq(length(r.tab.adj))] = r.tab.adj
r.matrix.adj = as.matrix(r.dist.adj)
Plot_Matrix(r.matrix)
Plot_Matrix(r.matrix.adj)

#d.dist = as.dist(Cor_Dist(r.matrix.adj))
d.dist = Cor_Dist(r.dist.adj)
d.matrix = as.matrix(d.dist)
# triangle inequality test
for (i in 3:nrow(d.matrix))
{
    #cat(i, "\n")
    for (j in 2:i)
    {
        for (k in 1:j)
        {
            if(d.matrix[i, j] + d.matrix[i, k] < d.matrix[j, k]) cat(i, j, k, "\n", sep = ", ")
        }
    }
}

hc = hclust(d.dist)
lab = hc.complete$labels
lab[11:length(lab)] = ""
plot(hc, cex = 0.75, labels = lab)
abline(sqrt(1 - cor.threshold **2), 0, col = "red")

if (is.character(plot.file)) dev.off()
