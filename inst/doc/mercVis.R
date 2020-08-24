## ----setup, include=FALSE, results="hide"-------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=6, fig.height=6, echo=TRUE)

## ----library------------------------------------------------------------------
suppressMessages( suppressWarnings( library(Mercator) ) )

## ----fakedata-----------------------------------------------------------------
set.seed(36766)
data(fakedata)
ls()
dim(fakedata)
dim(fakeclin)

## ----mercury------------------------------------------------------------------
mercury <- Mercator(dist(t(fakedata)), "euclid", "hclust", 4)
summary(mercury)

## ----euc-hclust, fig.cap = "Hierarchical clustering."-------------------------
plot(mercury, view = "hclust")

## ----euc-tsne5, fig.cap = "A t-SNE Plot."-------------------------------------
mercury <- addVisualization(mercury, "tsne")
plot(mercury, view = "tsne", main="t-SNE; Euclidean Distance")

## ----euc-tsne10,fig.cap = "A t-SNE plot with smaller perplexity."-------------
mercury <- addVisualization(mercury, "tsne", perplexity = 15)
plot(mercury, view = "tsne",  main="t-SNE; Euclidean Distance; perplexity = 15")

## ----euc-mds1, fig.cap = "Multi-dimensional scaling."-------------------------
mercury <- addVisualization(mercury, "mds")
plot(mercury, view = "mds", main="MDS; Euclidean Distance")

## ----igraph, fig.cap = "iGraph views."----------------------------------------
set.seed(73633)
mercury <- addVisualization(mercury, "graph", Q = 24)

plot(mercury, view = "graph", layout = "tsne", main="T-SNE Layout")
plot(mercury, view = "graph", layout = "mds", main = "MDS Layout")
plot(mercury, view = "graph", layout = "nicely", main = "'Nicely' Layout")

## ----cluster1Identity---------------------------------------------------------
my.clust <- getClusters(mercury)
table(my.clust)

## ----comptrue-----------------------------------------------------------------
table(my.clust, fakeclin$Type)

## ----bar, fig.cap="Silhouette widths.", fig.width=7, fig.height=5-------------
barplot(mercury)

## ----recluster, fig.cap ="A t-SNE plot after reclustering."-------------------
mercury <- recluster(mercury, K = 8)
plot(mercury, view = "tsne")

## ----bar8, fig.cap="Silhouette widths with eight clusters.", fig.width=7, fig.height=5----
barplot(mercury)

## ----hclass-------------------------------------------------------------------
hclass <- cutree(mercury@view[["hclust"]], k = 8)
neptune <- setClusters(mercury, hclass)
neptune <- remapColors(mercury, neptune)

## ----neptune, fig.cap="A t-SNE plot colored by heierachical clustering."------
plot(neptune, view = "tsne")

## ----hcworks------------------------------------------------------------------
barplot(neptune)

## ----truth, fig.cap = "A t-SNE plot with true cluster labels."----------------
venus <- setClusters(neptune, fakeclin$Type)
venus <- remapColors(neptune, venus)
plot(venus, view = "tsne")

## ----barnone, fig.cap="Silhouette widths with true clusters.", fig.width=7, fig.height=5----
barplot(venus)

## ----hctrue-------------------------------------------------------------------
table(getClusters(neptune), getClusters(venus))

## ----si-----------------------------------------------------------------------
sessionInfo()

