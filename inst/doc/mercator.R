## ----setup, include=FALSE, results="hide"-------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=6,fig.height=5,echo=TRUE)

## ----library------------------------------------------------------------------
suppressMessages( suppressWarnings( library(Mercator) ) )

## ----data---------------------------------------------------------------------
filename <- system.file("Examples/Mercator_Test_Data.csv", package="Mercator")
my.data <- read.csv(filename, header=TRUE)
dim(my.data)

## -----------------------------------------------------------------------------
my.data <- as.matrix(my.data)
my.binmat <- BinaryMatrix(my.data)
summary(my.binmat)

## -----------------------------------------------------------------------------
my.binmat <- t(my.binmat)
summary(my.binmat)

## ----duplicateFeatures--------------------------------------------------------
my.binmat <- removeDuplicateFeatures(my.binmat)
summary(my.binmat)

## -----------------------------------------------------------------------------
length(my.binmat@info$notUsed)
head(my.binmat@info$notUsed)

## -----------------------------------------------------------------------------
length(my.binmat@info$redundant)

## ----thresher-----------------------------------------------------------------
set.seed(21348)
my.binmat <- threshLGF(my.binmat, cutoff=0.3)
summary(my.binmat)

## ----delta, fig.cap="Histogram of weight vectors."----------------------------
Delta <- my.binmat@thresher@delta
hist(Delta, breaks=20, main="", xlab="Weight", col="gray")
abline(v=0.3, col='red')

## ----pcdim--------------------------------------------------------------------
my.binmat@reaper@pcdim
my.binmat@reaper@nGroups

## ----pcVis-1, fig.cap="Auer-Gervini plot."------------------------------------
plot(my.binmat@reaper@ag, ylim=c(0, 30))
abline(h=my.binmat@reaper@pcdim, col="forestgreen", lwd=2)
abline(h=7, col="orange", lwd=2)

## ----pcVis-2, fig.cap="Scree plot."-------------------------------------------
pts <- screeplot(my.binmat@reaper, xlim=c(0,30))
abline(v=pts[my.binmat@reaper@pcdim], col="forestgreen", lwd=2)
abline(v=pts[7], col="orange", lwd=2)

## ----kk-----------------------------------------------------------------------
kk <- 5

## ----distance, echo=TRUE------------------------------------------------------
jacc.Vis <- Mercator(my.binmat, "jaccard", "hclust", K=kk)

## ----jacc-hist, fig.cap = "Distribution of Jaccard distances."----------------
hist(jacc.Vis, 
     xlab="Jaccard Distance", main="Histogram of Distances")

## ----jacc-hclust, fig.cap = "Hierarchical clustering using Jaccard distances."----
plot(jacc.Vis, view = "hclust")

## ----jacc-tsne5, fig.width=5, fig.height=5, fig.cap = "A t-SNE plot."---------
jacc.Vis <- addVisualization(jacc.Vis, "tsne", perplexity=25)
plot(jacc.Vis, view = "tsne", main="t-SNE; Jaccard Distance")

## ----jacc-tsne10, fig.width=5, fig.height=5, fig.cap = "Another t-SNE plot."----
temp.Vis <- addVisualization(jacc.Vis, "tsne", perplexity = 10)
plot(temp.Vis, view = "tsne",  main="t-SNE; Jaccard Distance; perplexity=10")

## ----jacc-mds1, fig.width=5, fig.height=5, fig.cap = "A multi-dimensioanl scaling plot."----
jacc.Vis <- addVisualization(jacc.Vis, "mds")
plot(jacc.Vis, view = "mds", main="MDS; Jaccard Distance")

## ----barp, fig.width=5, fig.height=4, fig.cap = "Histogram of silhouette widths."----
barplot(jacc.Vis)

## ----reclue, fig.width=5, fig.height=4, fig.cap = "Silhouette widths for different K."----
jacc.Vis6 <- recluster(jacc.Vis, K = 6)
barplot(jacc.Vis6)
jacc.Vis7 <- recluster(jacc.Vis, K = 7)
barplot(jacc.Vis7)

## ----reset--------------------------------------------------------------------
kk <- 6
jacc.Vis <- jacc.Vis6
rm(jacc.Vis6, jacc.Vis7)

## ----downsample, fig.width=5, fig.height=5, fig.cap = "Downsample t-SNE plot."----
X <- jacc.Vis
X@view[["hclust"]] <- NULL # remove this view
N <- as.matrix(X@distance)
set.seed(87530)
P <- downsample(40, N, 0.1) # create a downsampled subset
J <- X[P]
names(J@view) # need to compute a new dendrogram
J <- addVisualization(J, "hclust", perplexity=5)
names(J@view)
plot(J, view = "tsne", main="Down-sampled t-SNE Plot")

## ----igraph, fig.width=4, fig.height=4, fig.cap = "Networks."-----------------
jacc.Vis <- addVisualization(jacc.Vis, "graph", Q =0.5)
plot(jacc.Vis, view = "graph", layout = "tsne", main="T-SNE Layout")
plot(jacc.Vis, view = "graph", layout = "mds", main="MDS Layout")
plot(jacc.Vis, view = "graph", layout = "nicely", 
     main="Laid Out 'Nicely'",
     xlim=c(-1,1))

## ----cluster1Identity---------------------------------------------------------
my.clust <- getClusters(jacc.Vis)
tab <- table(my.clust)
tab

## -----------------------------------------------------------------------------
C <- my.binmat@columnInfo
Cl4 <- C[my.clust == 4 ,]
Cl4

## ----sokal, fig.width=5, fig.height=5, fig.cap="Sokal-Michener distance t-SNE plot."----
set.seed(8642)
sokal.Vis <- Mercator(my.binmat, "sokal", "tsne", K=kk, peplexity = 10)
table(getClusters(sokal.Vis), getClusters(jacc.Vis))
plot(sokal.Vis, view = "tsne", main="t-SNE; Sokal-Michener Distance; perplexity=10")

## ----recolored----------------------------------------------------------------
SV <- remapColors(jacc.Vis, sokal.Vis)
table(getClusters(SV), getClusters(jacc.Vis))
plot(SV, view = "tsne", main="t-SNE; Sokal-Michener Distance; perplexity=10")

## ----sv2, fig.width=5, fig.height=5, fig.cap = "Recolored Jaccard t-SNE plot."----
slot(jacc.Vis, "palette") <- c("red", "orange", "green", "blue",
                               "cyan", "magenta", "purple", "black")
plot(jacc.Vis, view = "tsne")

## ----small,  fig.width=5, fig.height=5, fig.cap = "Recolored Jaccard t-SNE plot."----
slot(jacc.Vis, "palette") <- c("red", "green", "blue", "purple")
plot(jacc.Vis, view = "tsne")

