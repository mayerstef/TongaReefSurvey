# Load the required packages
library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(pvclust)
library(RColorBrewer)
library(labdsv)
library(rioja)
library(indicspecies)
library(mvpart)
library(MVPARTwrap)
library(dendextend)
library(vegclust)
library(colorspace)
library(agricolae)
library(picante)

# Source additional functions that will be used later in this
# Chapter. Our scripts assume that files to be read are in
# the working directory.
source("drawmap.R") #--> see edit
source("drawmap3.R")
source("hcoplot.R")
source("test.a.R")
source("coldiss.R")
source("bartlett.perm.R")
source("boxplerk.R")
source("boxplert.R")

# Function to compute a binary dissimilarity matrix from clusters
grpdist <- function(X)
{
  require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- daisy(gr, "gower")
  distgr
}


install.packages("unikn")
library(unikn)
n <- 10
h1 <- hcl.colors(n, palette = "Dynamic")
palette(h1)

# Compute and plot dendrograms ====================================
#for all of the below, we are using density per transect, with chord transformations 
#fore reference:
#spe.norm<-tonga.dc
#spe.ch<-tonga.dc.d
#spe<-tonga.d

## Hierarchical agglomerative clustering of the species abundance (density) data


# Attach site names to object of class 'dist'
attr(tonga.dc.d, "Labels") <- rownames(tonga.dc)

####

# Compute single linkage agglomerative clustering
tonga.single <- hclust(tonga.dc.d, method = "single")
# Plot a dendrogram using the default options
plot(tonga.single, 
     labels = rownames(tonga.d), 
     main = "Chord - Single linkage")

# Compute complete-linkage agglomerative clustering
tonga.complete <- hclust(tonga.dc.d, method = "complete")
plot(tonga.complete, 
     labels = rownames(tonga.d), 
     main = "Chord - Complete linkage")

# Compute UPGMA agglomerative clustering
tonga.UPGMA <- hclust(tonga.dc.d, method = "average")
plot(tonga.UPGMA, 
     labels = rownames(tonga.d), 
     main = "Chord - UPGMA")

# Compute centroid clustering
tonga.centroid <- hclust(tonga.dc.d, method = "centroid")
plot(tonga.centroid, 
     labels = rownames(tonga.d), 
     main = "Chord - Centroid")

# Compute Ward's minimum variance clustering
tonga.ward<- hclust(tonga.dc.d, method = "ward.D2")
plot(tonga.ward, 
     labels = rownames(tonga.dc), 
     main = "Chord - Ward")


# Cophenetic correlations =========================================

# Single linkage clustering
tonga.single.coph <- cophenetic(tonga.single)
cor(tonga.dc.d, tonga.single.coph) 
#0.639

# Complete linkage clustering
tonga.comp.coph <- cophenetic(tonga.complete)
cor(tonga.dc.d, tonga.comp.coph)
#0.759

# Average clustering
tonga.UPGMA.coph <- cophenetic(tonga.UPGMA)
cor(tonga.dc.d, tonga.UPGMA.coph)
#0.8236

# Ward clustering
tonga.ward.coph <- cophenetic(tonga.ward)
cor(tonga.dc.d, tonga.ward.coph)
#0.7246

# Shepard-like diagrams
dev.new(
  title = "Cophenetic correlation",
  width = 8,
  height = 9,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 1))
plot(
  tonga.dc.d,
  tonga.ward.coph,
  xlab = "Chord distance",
  ylab = "Cophenetic distance",
  asp = 1,
  xlim = c(0, sqrt(2)),
  ylim = c(0, max(tonga.ward$height)),
  main = c("Ward", paste("Cophenetic correlation =",
                         round(
                           cor(tonga.dc.d, tonga.ward.coph), 3
                         )))
)
abline(0, 1)
lines(lowess(tonga.dc.d, tonga.ward.coph), col = "red")
# Shepard-like diagrams comparing chord distances (species data) to cophenetic distances. 
#A LOWESS smoother shows the trend in each plot

# Gower (1983) distance
(gow.dist.single <- sum((tonga.dc.d - tonga.single.coph) ^ 2))
(gow.dist.comp <- sum((tonga.dc.d - tonga.comp.coph) ^ 2))
(gow.dist.UPGMA <- sum((tonga.dc.d - tonga.UPGMA.coph) ^ 2))
(gow.dist.ward <- sum((tonga.dc.d - tonga.ward.coph) ^ 2))

#---> based on all of these tests, it seems like UPGMA agglomerative clustering is the best
##BUT
#Ward might be more interesting to us given the data, so we will do both seperately and compare 

# Graphs of fusion level values ===================================
dev.new(
  title = "Fusion levels",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 2))
plot(
  tonga.complete$height,
  nrow(tonga.d):2,
  type = "S",
  main = "Fusion levels - Chord - Complete",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(tonga.complete$height,
     nrow(tonga.d):2,
     nrow(tonga.d):2,
     col = "red",
     cex = 0.8)
# Plot the fusion level values of the UPGMA clustering
plot(
  tonga.UPGMA$height,
  nrow(tonga.d):2,
  type = "S",
  main = "Fusion levels - Chord - UPGMA",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(tonga.UPGMA$height,
     nrow(tonga.d):2,
     nrow(tonga.d):2,
     col = "red",
     cex = 0.8)
# Plot the fusion level values of the Ward clustering
plot(
  tonga.ward$height,
  nrow(tonga.d):2,
  type = "S",
  main = "Fusion levels - Chord - Ward",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(tonga.ward$height,
     nrow(tonga.d):2,
     nrow(tonga.d):2,
     col = "red",
     cex = 0.8)
# Plot the fusion level values of the beta-flexible 
# clustering (-0.25)
plot(
  tonga.beta2$height,
  nrow(tonga.d):2,
  type = "S",
  main = "Fusion levels - Chord - Beta-flexible",
  ylab = "k (number of clusters)",
  xlab = "h (node height)",
  col = "grey"
)
text(tonga.beta2$height,
     nrow(tonga.d):2,
     nrow(tonga.d):2,
     col = "red",
     cex = 0.8)

### Graphs of the fusion level values seem to suggest 7 or 8 groups.


# Multiscale bootstrap resampling =================================

# Hierarchical clustering with p-values via multiscale bootstrap
# resampling

# Compute p-values for all clusters (edges) of the dendrogram
UPGMA.pv <- pvclust(t(tonga.dc),
                    method.hclust = "average",
                    method.dist = "euc",
                    parallel=TRUE)

# Plot dendrogram with p-values
dev.new(
  title = "Fish - Chord - p-values for UPGMA agglomerative clustering",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 1))
plot(UPGMA.pv)
# Highlight clusters with high "au" p-values
pvrect(UPGMA.pv, alpha = 0.95, pv = "au")
lines(UPGMA.pv)
pvrect(UPGMA.pv, alpha = 0.91, border = 4)

###
ward.pv <- pvclust(t(tonga.dc),
                   method.hclust = "ward.D2",
                   method.dist = "euc",
                   parallel=TRUE)

# Plot dendrogram with p-values
dev.new(
  title = "Fish - Chord - p-values for Ward's minimum variance clustering",
  width = 12,
  height = 8,
  noRStudioGD = TRUE
)
par(mfrow = c(1, 1))
plot(ward.pv)
# Highlight clusters with high "au" p-values
pvrect(ward.pv, alpha = 0.95, pv = "au")
lines(ward.pv)
pvrect(ward.pv, alpha = 0.91, border = 4)

# "This function plots a dendrogram with p-values for given object
# of class pvclust. AU p-value (printed in red color in default) 
# is the abbreviation of "approximately unbiased" p-value, which is 
# calculated by multiscale bootstrap resampling. BP value (printed
# in green color by default) is "bootstrap probability" value,
# which is less accurate than AU value as p-value. One can consider
# that clusters (edges) with high AU values (e.g. 95%) are strongly
# supported by data."

# Optimal number of clusters ======================================

# Function to compute a binary dissimilarity matrix from clusters
grpdist <- function(X)
{
  require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- daisy(gr, "gower")
  distgr
}

#For for UPGMA agglomerative clustering
dev.new(
  title = "Optimal number of clusters",
  width = 12,
  height = 8,
  noRStudioGD = TRUE)
par(mfrow = c(1, 2))

# Average silhouette widths (Rousseeuw quality index)
Si <- numeric(nrow(tonga.d))
for (k in 2:(nrow(tonga.d) - 1))
{
  sil <- silhouette(cutree(tonga.UPGMA, k = k), tonga.dc.d)
  Si[k] <- summary(sil)$avg.width
}
k.best <- which.max(Si)
plot(
  1:nrow(tonga.d),
  Si,
  type = "h",
  main = "Silhouette-optimal number of clusters (UPGMA)",
  xlab = "k (number of clusters)",
  ylab = "Average silhouette width"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(Si),
       pch = 16,
       col = "red",
       cex = 1.5
)
# Optimal number of clusters according to matrix correlation 
# statistic (Pearson)
kt <- data.frame(k = 1:nrow(tonga.d), r = 0)
for (i in 2:(nrow(tonga.d) - 1)) 
{
  gr <- cutree(tonga.UPGMA, i)
  distgr <- grpdist(gr)
  mt <- cor(tonga.dc.d, distgr, method = "pearson")
  kt[i, 2] <- mt
}
k.best <- which.max(kt$r)
plot(
  kt$k,
  kt$r,
  type = "h",
  main = "Matrix correlation-optimal number of clusters (UPGMA)",
  xlab = "k (number of clusters)",
  ylab = "Pearson's correlation"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(kt$r),
       pch = 16,
       col = "red",
       cex = 1.5)

#For for Ward's minimum variance clustering
dev.new(
  title = "Optimal number of clusters",
  width = 12,
  height = 8,
  noRStudioGD = TRUE)
par(mfrow = c(1, 2))

# Average silhouette widths (Rousseeuw quality index)
Si <- numeric(nrow(tonga.d))
for (k in 2:(nrow(tonga.d) - 1))
{
  sil <- silhouette(cutree(tonga.ward, k = k), tonga.dc.d)
  Si[k] <- summary(sil)$avg.width
}
k.best <- which.max(Si)
plot(
  1:nrow(tonga.d),
  Si,
  type = "h",
  main = "Silhouette-optimal number of clusters (Ward)",
  xlab = "k (number of clusters)",
  ylab = "Average silhouette width"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(Si),
       pch = 16,
       col = "red",
       cex = 1.5
)


# Optimal number of clusters according to matrix correlation 
# statistic (Pearson)
kt <- data.frame(k = 1:nrow(tonga.d), r = 0)
for (i in 2:(nrow(tonga.d) - 1)) 
{
  gr <- cutree(tonga.ward, i)
  distgr <- grpdist(gr)
  mt <- cor(tonga.dc.d, distgr, method = "pearson")
  kt[i, 2] <- mt
}
k.best <- which.max(kt$r)
plot(
  kt$k,
  kt$r,
  type = "h",
  main = "Matrix correlation-optimal number of clusters (Ward)",
  xlab = "k (number of clusters)",
  ylab = "Pearson's correlation"
)
axis(
  1,
  k.best,
  paste("optimum", k.best, sep = "\n"),
  col = "red",
  font = 2,
  col.axis = "red"
)
points(k.best,
       max(kt$r),
       pch = 16,
       col = "red",
       cex = 1.5)



# Final dendrogram with the selected clusters =====================

#UPGMA
# Choose the number of clusters
#k <- 10
k<- 9 #after comparing, 9 has the longest width 
#k<-8
# Silhouette plot of the final partition
tonga.k <- cutree(tonga.UPGMA, k = k)
sil <- silhouette(tonga.k, tonga.dc.d)
rownames(sil) <- row.names(tonga.d)
dev.new(title = "Silhouette plot - UPGMA - k=9", noRStudioGD = TRUE)
plot(
  sil,
  main = "Silhouette plot - Chord - UPGMA",
  cex.names = 0.6,
  col = 2:(k + 1),
  nmax = 100
)
mfrow(par = c(1,1))

#############
#Ward
k <- 8
#k<- 9
#k<-10
# Silhouette plot of the final partition
tonga.k <- cutree(tonga.ward, k = k)
sil <- silhouette(tonga.k, tonga.dc.d)
rownames(sil) <- row.names(tonga.d)
dev.new(title = "Silhouette plot - Ward - k=8", noRStudioGD = TRUE)
plot(
  sil,
  main = "Silhouette plot - Chord - Ward",
  cex.names = 0.6,
  col = 2:(k + 1),
  nmax = 100
)
mfrow(par = c(1,1))

#---> 8 seems to be best for Ward


# Plot the Clusters on the map
#UPGMA
k<-10
tonga.k <- cutree(tonga.UPGMA, k = k)
dev.new(title = "UPGMA mapped clusters",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = tonga.xy,
        clusters = tonga.k,
        main = "UPGMA mapped clusters")
drawmap(xy = tonga.xy,
        clusters = tonga.k,
        main = "UPGMA mapped clusters"
)

#Ward
k<-10
tonga.k <- cutree(tonga.ward, k = k)
dev.new(title = "Ward's mapped clusters",
        width = 9,
        noRStudioGD = TRUE)
drawmap(xy = tonga.xy,
        clusters = tonga.k,
        main = "Ward's mapped clusters")
drawmap(xy = tonga.xy,
        clusters = tonga.k,
        main = "Ward's mapped clusters"
)
