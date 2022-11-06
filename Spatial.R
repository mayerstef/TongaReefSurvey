# Load packages, functions and data ===============================
library(data.table)
library(SoDA)
library(reshape)
library(ape)
library(spdep)
library(ade4)
library(adegraphics)
library(adespatial)
library(vegan)
library(readxl)

source("plot.links.R")
source("sr.value.R")
source("quickMEM.R")
source("scalog.R")

tongu_data <- read_excel("tongu_data.xlsx", 
                         sheet = "tongu", col_types = c("date", "skip", "skip", "numeric", "numeric", 
                                                        "numeric", "text", "text", "numeric", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "text", "numeric", "numeric", "numeric", 
                                                        "numeric", "text", "text", "text", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric"))
tonga<-tongu_data %>% clean_names(.,case ="snake")
tonga<-tongu_data
tonga$species<-spCodes(tonga$species, nchar.gen = 3, nchar.sp = 9, nchar.ssp = 0,
                       sep.species = " ", sep.spcode = ".", verbosity = 2)
#tonga<-tonga%>% group_by(species) %>% filter(n()>= 5) %>% ungroup()
tonga$site<-abbreviate(tonga$site)

#Density per km
tonga.d<-cast(tonga, formula = site~species, sum, value = "density", fill = 0) #
tonga.d<-column_to_rownames(tonga.d, var = "site")

#chord using library(vegan)
tonga.dc <- decostand(tonga.d, "nor")
tonga.dc.d <- dist(tonga.dc)
tonga.dc.pca <- rda(tonga.dc)


#Extract coordinates --> spatial matrix
#longitude values are considered the x-coordinate, while latitude values are the y-coordinate
mlat<-cast(tonga, formula = site~., mean, value = "lat", fill = 0) 
mlong<-cast(tonga, formula = site~., mean, value = "long", fill = 0)
tonga.gps<-data.frame(mlong, mlat)
ugh<-data.frame(tonga.gps$site, tonga.gps$X.all., tonga.gps$X.all..1)
setnames(ugh, old = c("tonga.gps.site", "tonga.gps.X.all.", "tonga.gps.X.all..1"), 
         new = c('site','lon','lat'))
tonga.gps<-ugh
tonga.gps<-column_to_rownames(tonga.gps, var="site")
tonga.gps

tonga.xy<- geoXY(tonga.gps$lat, tonga.gps$lon, unit = 1000)
tonga.xy<-as.data.frame(tonga.xy, row.names=ugh$site)
View(tonga.xy)
plot(tonga.xy$X~tonga.xy$Y)
plot(tonga.gps$lon~tonga.gps$lat)


#mlat<-cast(tonga30, formula = site~., mean, value = "lat", fill = 0) 
#mlong<-cast(tonga30, formula = site~., mean, value = "long", fill = 0)
#tonga30.gps<-data.frame(mlong, mlat)
#ugh30<-data.frame(tonga30.gps$site, tonga30.gps$X.all., tonga30.gps$X.all..1)
#setnames(ugh30, old = c("tonga30.gps.site", "tonga30.gps.X.all.", "tonga30.gps.X.all..1"), 
#         new = c('site','lon','lat'))
#t30.gps<-ugh30
#t30.gps<-column_to_rownames(t30.gps, var="site")
#t30.gps
#t30.xy <- geoXY(t30.gps$lat, t30.gps$lon, unit = 1000)
#t30.xy<-as.data.frame(t30.xy, row.names = ugh30$site) #wow it was that easy to not have to use excel 
#View(t30.xy)

## Spatial data: linear trends
#Can we explan anything with just using the *xy* coordinates of the stations ? 
rda.xy <- rda(tonga.dc, tonga.xy)
summary(rda.xy) # Big output, but DO do look at it.
RsquareAdj(rda.xy) 
anova(rda.xy)
# 0.12 explained with linear coordinates, so the residuals contain...
var.res=1-0.1198403 
var.res

# Transform the data
#tonga.dc <- we will be using tonga.dc (tonga density chord )
#tonga.xy.c <- scale(tonga.xy, center = TRUE, scale = FALSE)
tonga.xy.c<-scale(tonga.xy, center = TRUE, scale = FALSE)


# Mantel correlogram of the reef fish tonga data ====================

# The species data are first detrended; see Sect. 7.3
tonga.dc.det <- resid(lm(as.matrix(tonga.dc) ~ ., data = tonga.xy))
RsquareAdj(rda(tonga.dc, tonga.xy)) 
tonga.dc.D1 <- dist(tonga.dc.det)
(tonga.correlog <- 
    mantel.correlog(tonga.dc.D1, 
                    XY = tonga.xy, 
                    nperm = 999))
summary(tonga.correlog)

# Number of classes
tonga.correlog$n.class # or: tonga.correlog[2]
# Break points
tonga.correlog$break.pts # or: tonga.correlog[3]

# Plot the Mantel correlogram
dev.new(
  title = "Mantel correlogram of tonga data", 
  width = 9, 
  height = 5, 
  noRStudioGD = TRUE
)
plot(tonga.correlog)


# Trend-surface analysis ==========================================
# Computation of a raw (non-orthogonal) third-degree polynomial 
# function on the previously centred X-Y coordinates
tonga.poly <- poly(as.matrix(tonga.xy.c), degree = 3, raw = TRUE)
colnames(tonga.poly) <- 
  c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")

# RDA with all 9 polynomial terms
(tonga.trend.rda <- rda(tonga.dc ~ ., 
                        data = as.data.frame(tonga.poly)))

# Computation of the adjusted R^2
(R2adj.poly <- RsquareAdj(tonga.trend.rda)$adj.r.squared)

# RDA using a third-degree orthogonal polynomial of the geographic 
# coordinates
tonga.poly.ortho <- poly(as.matrix(tonga.xy), degree = 3)
colnames(tonga.poly.ortho) <- 
  c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")
(tonga.trend.rda.ortho <- 
    rda(tonga.dc ~ ., 
        data = as.data.frame(tonga.poly.ortho)))
(R2adj.poly2 <- RsquareAdj(tonga.trend.rda.ortho)$adj.r.squared)

# Forward selection using Blanchet et al. (2008a) double stopping 
# criterion
(tonga.trend.fwd <- 
    forward.sel(tonga.dc, tonga.poly.ortho, adjR2thresh = R2adj.poly2))

# New RDA using the 8 terms retained
(tonga.trend.rda2 <- rda(tonga.dc ~ ., 
                         data = as.data.frame(tonga.poly)[ ,tonga.trend.fwd[ ,2]]))

# Overall test and test of the canonical axes
anova(tonga.trend.rda2)
anova(tonga.trend.rda2, by = "axis")

# Plot of the 4 independent significant spatial structures (canonical axes) 
tonga.trend.fit <- 
  scores(tonga.trend.rda2, 
         choices = 1:4, 
         display = "lc", 
         scaling = 1)
dev.new(
  title = "tonga Trend Surface Analysis", 
  noRStudioGD = TRUE
)
s.value(tonga.xy, tonga.trend.fit, symbol = "circle")

# Distance-based Moran eigenvector maps (dbMEM) ===================
## dbMEM analysis of the reef fish tonga data

# Is there a linear trend in the tonga data?
anova(rda(tonga.dc, tonga.xy))
# Result: significant trend

# Computation of linearly detrended tonga data
tonga.dc.det <- resid(lm(as.matrix(tonga.dc) ~ ., data = tonga.xy))

## Step 1. Construct the matrix of dbMEM variables
tonga.dbmem.tmp <- dbmem(tonga.xy, silent = FALSE)
# Argument silent = FALSE allows the function to display 
# the truncation level.
(tonga.dbmem <- as.data.frame(tonga.dbmem.tmp))
RsquareAdj(rda(tonga.dc, tonga.dbmem))

tonga.dbmem.test <- dbmem(tonga.xy, silent = F, MEM.autocor=c("negative"))
(tonga.dbmem.neg <- as.data.frame(tonga.dbmem.test))
RsquareAdj(rda(tonga.dc, tonga.dbmem.neg))

tonga.dbmem.test2 <- dbmem(tonga.xy, silent = F, MEM.autocor=c("non-null"))
(tonga.dbmem.null <- as.data.frame(tonga.dbmem.test2))
RsquareAdj(rda(tonga.dc, tonga.dbmem.null))



# Truncation distance used above:
(thr <- give.thresh(dist(tonga.xy)))

# Display and count the eigenvalues
attributes(tonga.dbmem.tmp)$values
length(attributes(tonga.dbmem.tmp)$values)

## Step 2. Run the global dbMEM analysis on the *detrended*
##    Chord-transformed tonga data
(tonga.dbmem.rda <- rda(tonga.dc.det ~ ., tonga.dbmem))
# with tonga.dc.det <- resid(lm(as.matrix(tonga.dc) ~ ., data = tonga.xy))
anova(tonga.dbmem.rda)

## Step 3. Since the R-square is significant, compute the adjusted
##    R2 and run a forward selection of the dbmem variables
(tonga.R2a <- RsquareAdj(tonga.dbmem.rda)$adj.r.squared)
(tonga.dbmem.fwd <- forward.sel(tonga.dc.det, as.matrix(tonga.dbmem), 
                                adjR2thresh = tonga.R2a))
(nb.sig.dbmem <- nrow(tonga.dbmem.fwd))    # Number of signif. dbMEM
# Identity of the significant dbMEM in increasing order
(dbmem.sign <- sort(tonga.dbmem.fwd[ ,2]))
# Write the significant dbMEM to a new object
dbmem.red <- tonga.dbmem[ ,c(dbmem.sign)]

## Step 4. New dbMEM analysis with 8 significant dbMEM variables
##    Adjusted R-square after forward selection: R2adj = 0.2255962 --> 0.1924879
(tonga.dbmem.rda2 <- rda(tonga.dc.det ~ ., data = dbmem.red))
(tonga.fwd.R2a <- RsquareAdj(tonga.dbmem.rda2)$adj.r.squared)
anova(tonga.dbmem.rda2)
(axes.test <- anova(tonga.dbmem.rda2, by = "axis"))
# Number of significant axes
(nb.ax <- length(which(axes.test[ ,ncol(axes.test)] <=  0.05)))

## Step 5. Plot the significant canonical axes
tonga.rda2.axes <- 
  scores(tonga.dbmem.rda2, 
         choices = c(1:nb.ax), 
         display = "lc", 
         scaling = 1)
dev.new(
  title = "dbMEM analysis of tonga data", 
  width = 8, 
  height = 6, 
  noRStudioGD = TRUE
)
par(mfrow = c(1,nb.ax))
for(i in 1:nb.ax){
  sr.value(tonga.xy, tonga.rda2.axes[ ,i], 
           sub = paste("RDA",i), 
           csub = 2)
}

# Interpreting the spatial variation: regression of the significant
# canonical axes on the environmental variables, with Shapiro-Wilk 
# normality tests of residuals
tonga.rda2.axis1.env <- lm(tonga.rda2.axes[ ,1] ~ ., data = tonga.env)
shapiro.test(resid(tonga.rda2.axis1.env))
summary(tonga.rda2.axis1.env)

tonga.rda2.axis2.env <- lm(tonga.rda2.axes[ ,2] ~ ., data = tonga.env)
shapiro.test(resid(tonga.rda2.axis2.env))
summary(tonga.rda2.axis2.env)

# Depending on the permutation-based p-value the third axis may 
# not be significant. In that case the 3 next lines of code will
# return error messages.
tonga.rda2.axis3.env <- lm(tonga.rda2.axes[ ,3] ~ ., data = tonga.env)
shapiro.test(resid(tonga.rda2.axis3.env))
summary(tonga.rda2.axis3.env)

# Scalogram of the variance explained by all dbMEM eigenfunctions, 
# computed with our homemade function scalog()
dev.new(
  title = "scalogram",
  width = 10,
  height = 5,
  noRStudioGD = TRUE)
scalog(tonga.dbmem.rda)

# Maps of the 8 significant dbMEM variables with the homemade
# function sr.value()
dev.new(
  title = "8 dbMEM variables - tongas", 
  width = 10, 
  height = 4,
  noRStudioGD = TRUE
)
par(mfrow = c(2, 4))
for(i in 1 : ncol(dbmem.red))
{
  sr.value(tonga.xy, 
           dbmem.red[ ,i], 
           #             sub = paste("dbMEM",i), 
           sub = paste("dbMEM",dbmem.sign[i]), 
           csub = 2, clegend=1.5, grid=FALSE)
}


# Maps with s.value()
dev.new(
  title = "8 dbMEM variables - tongas", 
  noRStudioGD = TRUE
)
par(mfrow = c(1,1))
s.value(tonga.xy, dbmem.red, method = "size",symbol = "circle")



## dbMEM analysis of the tonga data - broad scale
(tonga.dbmem.broad <- 
    rda(tonga.dc.det ~ ., data = tonga.dbmem[ ,c(1,3,4)]))
anova(tonga.dbmem.broad)
(axes.broad <- anova(tonga.dbmem.broad, by = "axis"))
# Number of significant axes
(nb.ax.broad <- 
    length(which(axes.broad[ , ncol(axes.broad)] <=  0.05)))

# Plot of the two significant canonical axes
tonga.dbmembroad.axes <- 
  scores(tonga.dbmem.broad, 
         choices = c(1,2), 
         display = "lc", 
         scaling = 1)
dev.new(
  title = "dbMEM analysis of tonga data - broad scale", 
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
sr.value(tonga.xy, tonga.dbmembroad.axes[ ,1])
sr.value(tonga.xy, tonga.dbmembroad.axes[ ,2])

# Interpreting the broad-scaled spatial variation: regression of
# the two significant spatial canonical axes on the environmental
# variables
tonga.dbmembroad.ax1.env <- 
  lm(tonga.dbmembroad.axes[ ,1] ~ ., data = tonga.env)
summary(tonga.dbmembroad.ax1.env)
tonga.dbmembroad.ax2.env <- 
  lm(tonga.dbmembroad.axes[ ,2] ~ ., data = tonga.env)
summary(tonga.dbmembroad.ax2.env)


## dbMEM analysis of the tonga data - medium scale

(tonga.dbmem.med <- 
    rda(tonga.dc.det ~ ., data = tonga.dbmem[ ,c(5,7,9,11)]))
anova(tonga.dbmem.med)
(axes.med <- anova(tonga.dbmem.med, by = "axis"))
# Number of significant axes
(nb.ax.med <- length(which(axes.med[ ,ncol(axes.med)] <=  0.05)))

# Plot of the significant canonical axes
tonga.dbmemmed.axes <- 
  scores(tonga.dbmem.med, choices = c(1,2), 
         display = "lc", 
         scaling = 1)
dev.new(
  title = "dbMEM analysis of tonga data - medium scale", 
  noRStudioGD = TRUE
)
par(mfrow = c(1, 2))
sr.value(tonga.xy, tonga.dbmemmed.axes[ ,1])
sr.value(tonga.xy, tonga.dbmemmed.axes[ ,2])

# Interpreting the medium-scaled spatial variation: regression of
# the two significant spatial canonical axes on the environmental
# variables
tonga.dbmemmed.ax1.env <- 
  lm(tonga.dbmemmed.axes[ ,1] ~ ., data = tonga.env)
summary(tonga.dbmemmed.ax1.env)
tonga.dbmemmed.ax2.env <- 
  lm(tonga.dbmemmed.axes[ ,2] ~ ., data = tonga.env)
summary(tonga.dbmemmed.ax2.env)


## dbMEM analysis of the tonga data - fine scale

(tonga.dbmem.fine <- 
    rda(tonga.dc.det ~ ., data = as.data.frame(tonga.dbmem[ ,14])))
anova(tonga.dbmem.fine)
# Analysis stops here, since the RDA is not significant.



## tonga - trend - environment - dbMEM variation partitioning

# 1. Test trend
tonga.XY.rda <- rda(tonga.dc, tonga.xy)
anova(tonga.XY.rda)

# 2. Test and forward selection of the environmental variables