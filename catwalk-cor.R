# catwalk-cor.R - correlation analysis
# this script must be called from within catwalk.R or interactively afterwards (assumes pre-computed variables)

# input given by data.frame whose name is stored in varname
if(! exists("varname")) varname = "catwalk.clean"
stopifnot(exists(varname))
stopifnot(is.data.frame(get(varname)))

pdf(file = "catwalk-cor.pdf")

i.num.selected = which(as.vector(sapply(get(varname), is.numeric)))

#Plot_Cor_Matrix(varname = "catwalk.reduced")
cor.matrix = cor(get(varname)[, i.num.selected], use = "complete.obs")
var.dist = as.dist(m = Cor_Dist(cor.matrix))

hc.list = list()
cor.list = list()

for (linkage in c("single", "average", "complete", "centroid", "ward"))
{
    hc.list[[linkage]] = hclust(var.dist, method = linkage)
    cor.list[[linkage]] =
    cor.matrix[hc.list[[linkage]]$order, hc.list[[linkage]]$order]
    eval(parse(text = paste0("Plot_Matrix(cor.list[[\"", linkage, "\"]])")))
    #dist.matrix = Cor_Dist(get(varname))
    eval(parse(text = paste0("Plot_Matrix(Cor_Dist(cor.list[[\"", linkage, "\"]]))")))
    #Plot_Matrix(Cor_Dist(cor.list[[linkage]]))
    plot(hc.list[[linkage]], labels = FALSE)
}

dev.off()

if(do.view) system("xdg-open catwalk-cor.pdf")

