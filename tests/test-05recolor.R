library("Mercator")
data("CML500")

# remove extra identical feature vectors
CML500 <- removeDuplicateFeatures(CML500)
# jacc, pear, manh, euc
vis1 <- Mercator(CML500, "jacc", "mds", K=8)
vis2 <- Mercator(CML500, "sokal", "mds", K=8)
vis3 <- remapColors(vis1, vis2)

# par(mfrow=c(1,3),  cex=1.5)
plot(vis1)
plot(vis2)
plot(vis3)

A <- getClusters(vis2)
B <- getClusters(vis3)
table(A, B)
X <- getClusters(vis1)
table(A, X)
table(B, X)

library(cluster)
clus <- pam(vis1@distance, k = 12, diss=TRUE, cluster.only=TRUE)
vis4 <- setClusters(vis1, clus)
plot(vis4)

slot(vis4, "palette") <- c("red", "green", "blue",
                           "cyan", "purple", "black")
table(Mercator:::symv(vis4))
