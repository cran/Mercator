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

## ----udirect, fig.cap="UMAP visualization of fake data.", fig.width=7, fig.height=7----
library(umap)
udirect <- umap(t(fakedata))
plot(udirect$layout, main="Original UMAP")

## ----mercury, fig.cap="MDS visualization from a distance matrix.", fig.width=7, fig.height=7----
mercury <- Mercator(dist(t(fakedata)), "euclid", "mds", 7)
plot(mercury, main = "MDS")

## ----peek---------------------------------------------------------------------
summary(mercury)
slotNames(mercury)
mercury@metric
mercury@palette
mercury@symbols
table(mercury@clusters)
class(mercury@view)
names(mercury@view)
class(mercury@view[["hclust"]])
class(D <- mercury@distance)

## ----umapper, fig.cap="UMAP visualization from a distance matrix.", fig.width=7, fig.height=7----
mercury <- addVisualization(mercury, "umap")
plot(mercury, view = "umap", main="UMAP from distance matrix")

## ----me-----------------------------------------------------------------------
Mercator:::makeEuclidean

## ----newdist------------------------------------------------------------------
X <- Mercator:::makeEuclidean(D)
dim(X)

## ----delta--------------------------------------------------------------------
D2 <- dist(X)
delta <- as.matrix(D) - as.matrix(D2)
summary(as.vector(delta))

## ----som, fig.cap="SOM visualizations from a distance matrix.", fig.width=7, fig.height=5----
mercury <- addVisualization(mercury, "som",
                            grid = kohonen::somgrid(6, 5, "hexagonal"))
plot(mercury, view = "som")

## ----oldsom-------------------------------------------------------------------
library(kohonen)
mysom <- som(t(fakedata), grid = somgrid(6, 5, "hexagonal"))
plot(mysom, type = "mapping")

## ----colplot, fig.cap="SOM visualizations from a distance matrix.", fig.width=7, fig.height=5----
plot(mysom, type = "mapping", pchs=16, col=Mercator:::colv(mercury))

## ----codes, fig.cap="SOM codes.", fig.width=7, fig.height=5-------------------
plot(mysom, type = "codes", main="Original", shape = "straight")
plot(mercury, view = "som", type="codes", main="After MDS")

## ----si-----------------------------------------------------------------------
sessionInfo()

