## ----setup, include=FALSE, results="hide"--------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=7,fig.height=5,echo=TRUE)

## ----library-------------------------------------------------------------
suppressMessages( suppressWarnings( library(Mercator) ) )

## ----data----------------------------------------------------------------
filename <- system.file("Examples/Mercator_Test_Data.csv", package="Mercator")
my.data <- read.csv(filename, header=TRUE)
dim(my.data)

## ------------------------------------------------------------------------
my.data <- as.matrix(my.data)
my.binmat <- BinaryMatrix(my.data)
summary(my.binmat)

## ------------------------------------------------------------------------
my.binmat <- t(my.binmat)
summary(my.binmat)

## ----duplicateFeatures---------------------------------------------------
my.binmat <- removeDuplicateFeatures(my.binmat)
summary(my.binmat)

## ------------------------------------------------------------------------
length(my.binmat@info$notUsed)
head(my.binmat@info$notUsed)

## ------------------------------------------------------------------------
length(my.binmat@info$redundant)

## ----thresher------------------------------------------------------------
set.seed(21348)
my.binmat <- threshLGF(my.binmat, cutoff=0.3)
summary(my.binmat)

## ----delta, fig.cap="Histogram of weight vectors."-----------------------
Delta <- my.binmat@thresher@delta
hist(Delta, breaks=20, main="", xlab="Weight", col="gray")
abline(v=0.3, col='red')

## ----pcdim---------------------------------------------------------------
my.binmat@reaper@pcdim
my.binmat@reaper@nGroups

## ----pcVis-1, fig.cap="Auer-Gervini plot."-------------------------------
plot(my.binmat@reaper@ag, ylim=c(0, 30))
abline(h=my.binmat@reaper@pcdim, col="forestgreen", lwd=2)
abline(h=7, col="orange", lwd=2)

## ----pcVis-2, fig.cap="Scree plot."--------------------------------------
pts <- screeplot(my.binmat@reaper, xlim=c(0,30))
abline(v=pts[my.binmat@reaper@pcdim], col="forestgreen", lwd=2)
abline(v=pts[7], col="orange", lwd=2)

## ----kk------------------------------------------------------------------
kk <- 8

## ----distance, echo=TRUE-------------------------------------------------
jacc.Vis <- Mercator(my.binmat, "jaccard", "hclust", K=kk)

## ----jacc-hist-----------------------------------------------------------
hist(jacc.Vis, 
     xlab="Jaccard Distance", main="Histogram of Distances")

## ----jacc-hclust---------------------------------------------------------
names(jacc.Vis@view)
plot(jacc.Vis, view = "hclust")

## ----jacc-tsne5----------------------------------------------------------
par(pty="s")
jacc.Vis <- addVisualization(jacc.Vis, "tsne", 
            perplexity=5, 
            xlab="T1", ylab="T2")
names(jacc.Vis@view)
plot(jacc.Vis, view = "tsne", main="t-SNE; Jaccard Distance; perplexity=5")

## ----jacc-tsne10---------------------------------------------------------
jacc.Vis <- addVisualization(jacc.Vis, "tsne", 
            perplexity=10)
names(jacc.Vis@view)
plot(jacc.Vis, view = "tsne",  main="t-SNE; Jaccard Distance; perplexity=10")
par(pty="m")

## ----jacc-mds1-----------------------------------------------------------
jacc.Vis <- addVisualization(jacc.Vis, "mds")
names(jacc.Vis@view)
plot(jacc.Vis, view = "mds", main="MDS; Jaccard Distance")

## ----downsample----------------------------------------------------------
X <- jacc.Vis
N <- as.matrix(X@distance)
set.seed(87530)
P <- downsample(40, N, 0.1)
J <- jacc.Vis[P]
J <- addVisualization(J, "tsne", perplexity=5)
names(J@view)
par(pty="s")
plot(J, view = "tsne", main="Down-sampled t-SNE Plot")
par(pty="m")

## ----igraph--------------------------------------------------------------
set.seed(10967)
J <- addVisualization(J, "mds")
J <- addVisualization(J, "graph")

plot(J, view = "graph", layout = "mds")
plot(J, view = "graph", layout = "nicely", 
     main="Graphical View of Down-sampled Jaccard Distance Matrix",
     xlim=c(-1,1))


plot(J, view = "graph", layout = "tsne", main="T-SNE Layout")

## ----cluster1Identity----------------------------------------------------
my.clust <- getClusters(jacc.Vis)
tab <- table(my.clust)
tab

## ------------------------------------------------------------------------
C <- my.binmat@columnInfo
Cl4 <- C[my.clust == 4 ,]
Cl4

## ----sokal---------------------------------------------------------------
set.seed(8642)
sokal.Vis <- Mercator(my.binmat, "sokal", "tsne", K=kk, peplexity = 10)
table(getClusters(sokal.Vis), getClusters(jacc.Vis))
plot(sokal.Vis, view = "tsne", main="t-SNE; Sokal-Michener Distance; perplexity=10")

## ----recolored-----------------------------------------------------------
SV <- remapColors(jacc.Vis, sokal.Vis)
table(getClusters(SV), getClusters(jacc.Vis))
plot(SV, view = "tsne", main="t-SNE; Sokal-Michener Distance; perplexity=10")

