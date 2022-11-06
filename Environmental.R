library(readxl)
env <- read_excel("tongu_data.xlsx", 
                  sheet = "env", col_types = c("text", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", "numeric", 
                                               "text", "text"))



env1<-env[1:8]%>%
  group_by(site) %>%
  summarize(across(.fns = mean))

#env1[7:8] <- lapply(env1[7:8], factor)
#sapply(env1, class)
#View(env1)

#env1.30<-subset(env1,site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))


#Feeding type by biomass 
env2<-food.b <- read_excel("tongu_data.xlsx", 
                           sheet = "food.b")
env2<-env2%>%
  group_by(site) %>%
  summarize(across(.fns = sum))

#env2.30<-subset(env2, site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))

#Feeding type by density
env3<-food.d <- read_excel("tongu_data.xlsx", 
                           sheet = "food.d")
env3<-env3%>%
  group_by(site) %>%
  summarize(across(.fns = sum))

#env3.30<-subset(env3, site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))

#Feeding type by richness
env4<-food.r <- read_excel("tongu_data.xlsx", 
                           sheet = "food.r")
env4<-env4%>%
  group_by(site) %>%
  summarize(across(.fns = sum))


#env4.30<-subset(env4, site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))

#Distances to location
env5<-distance <- read_excel("tongu_data.xlsx", 
                             sheet = "distance")
env5<-env5%>%
  group_by(site) %>%
  summarize(across(.fns = mean))

#env5.30<-subset(env5, site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))

#Benthic %cover
env6<-benthic <- read_excel("tongu_data.xlsx", 
                            sheet = "benthic")
env6<-env6%>%
  group_by(site) %>%
  summarize(across(.fns = mean))

# env6.30<-subset(env6, site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))


#%Cover by total "groups
env7<-cover<- read_excel("tongu_data.xlsx", 
                         sheet = "cover")
env7<-env7%>%
  group_by(site) %>%
  summarize(across(.fns = mean))

#env7.30<-subset(env7, site %in%  c("Afa",
#                                   "American warf",
#                                   "Ata prison close",
#                                   "Ata prison north",
#                                   "Atata FHR 1",
#                                   "Atata SMA 3",
#                                   "Eueiki SMA 3",
#                                   "Euiki FHR 1",
#                                   "Fafa 3",
#                                   "Fafa control close",
#                                  "Fafa Far control 1",
#                                  "Haatafu FHR Resort",
#                                  "Hakau Manu 1",
#                                  "Kolonga FHR",
#                                  "Kolonga SMA",
#                                  "Lagoon Entrance 2",
#                                  "Malinoa 1",
#                                  "Malinoa 2",
#                                   "Middle reefs close",
#                                   "Military Island 1",
#                                   "Nuku bommie 1",
#                                   "Nuku Island close",
#                                   "Nuku Outside 2",
#                                   "Palace",
#                                   "Pangaimotu control",
#                                   "Sopu Mid",
#                                   "Tao Mid",
#                                   "Tao North",
#                                   "Tokatoka 1",
#                                   "Tokatoka Far bommie"))

#env1.30$site<-abbreviate(env1.30$site)
#env2.30$site<-abbreviate(env2.30$site)
#env3.30$site<-abbreviate(env3.30$site)
#env4.30$site<-abbreviate(env4.30$site)
#env5.30$site<-abbreviate(env5.30$site)
#env6.30$site<-abbreviate(env6.30$site)
#env7.30$site<-abbreviate(env7.30$site)

#e31<-column_to_rownames(env1.30, var = "site")
#e32<-column_to_rownames(env2.30, var = "site")
#e33<-column_to_rownames(env3.30, var = "site")
#e34<-column_to_rownames(env4.30, var = "site")
#e35<-column_to_rownames(env5.30, var = "site")
#e36<-column_to_rownames(env6.30, var = "site")
#e37<-column_to_rownames(env7.30, var = "site")

env1$site<-abbreviate(env1$site)
env2$site<-abbreviate(env2$site) #will not be used, possibly for functional diversity
env3$site<-abbreviate(env3$site) #will not be used, possibly for functional diversity
env4$site<-abbreviate(env4$site) #will not be used, possibly for functional diversity
env5$site<-abbreviate(env5$site)
env6$site<-abbreviate(env6$site)
env7$site<-abbreviate(env7$site)

e1.5<-merge(env1, env5, by="site") #combine e1 and e5
e1.5.7<-merge(e1.5, env7, by="site") #combine e1, e5, and e7

e1<-column_to_rownames(env1, var = "site")
e2<-column_to_rownames(env2, var = "site") #will not be used, possibly for functional diversity
e3<-column_to_rownames(env3, var = "site") #will not be used, possibly for functional diversity
e4<-column_to_rownames(env4, var = "site") #will not be used, possibly for functional diversity
e5<-column_to_rownames(env5, var = "site")
e6<-column_to_rownames(env6, var = "site")
e7<-column_to_rownames(env7, var = "site")
e1.5<-column_to_rownames(e1.5, var = "site")
e1.5.7<-column_to_rownames(e1.5.7, var="site")

########
#Explore Data, check for multicolinearity 
par(mfrow=c(1,1))
for (i in 1:ncol(e1)) {
  cur_var <- colnames(e1)[i]
  hist(e1[,i], xlab = cur_var, main = cur_var)
} 
par(mfrow=c(1,2))
for (i in 1:ncol(e1)) {
  cur_var <- colnames(e1)[i]
  hist(e1[,i], xlab = cur_var, main = cur_var)
  cur_var <- paste("log(", cur_var, ")", sep = "")
  hist(log(e1[,i]), xlab = cur_var, main = cur_var)
} 

#############################
par(mfrow=c(1,1))
for (i in 1:ncol(e5)) {
  cur_var <- colnames(e5)[i]
  hist(e5[,i], xlab = cur_var, main = cur_var)
} 
par(mfrow=c(1,2))
for (i in 1:ncol(e5)) {
  cur_var <- colnames(e5)[i]
  hist(e5[,i], xlab = cur_var, main = cur_var)
  cur_var <- paste("log(", cur_var, ")", sep = "")
  hist(log(e5[,i]), xlab = cur_var, main = cur_var)
} 
########################
par(mfrow=c(1,1))
for (i in 1:ncol(e6)) {
  cur_var <- colnames(e6)[i]
  hist(e6[,i], xlab = cur_var, main = cur_var)
} 
par(mfrow=c(1,2))
for (i in 1:ncol(e6)) {
  cur_var <- colnames(e6)[i]
  hist(e6[,i], xlab = cur_var, main = cur_var)
  cur_var <- paste("log(", cur_var, ")", sep = "")
  hist(log(e6[,i]), xlab = cur_var, main = cur_var)
} 
########################
par(mfrow=c(1,1))
for (i in 1:ncol(e7)) {
  cur_var <- colnames(e7)[i]
  hist(e7[,i], xlab = cur_var, main = cur_var)
} 
par(mfrow=c(1,2))
for (i in 1:ncol(e7)) {
  cur_var <- colnames(e7)[i]
  hist(e7[,i], xlab = cur_var, main = cur_var)
  cur_var <- paste("log(", cur_var, ")", sep = "")
  hist(log(e7[,i]), xlab = cur_var, main = cur_var)
} 
par(mfrow=c(1,1))
###########################
library(car)
mod_full<-lm(depth~., data=e1)
vif(mod_full)

mod_full<-lm(market~., data=e5)
vif(mod_full)

mod_full<-lm(Turf~., data=e6)
vif(mod_full)

mod_full<-lm(hard~., data=e7)
vif(mod_full)
#--> all below 5, no multicolinearity detected

#kobe's function:
VIF_analysis <- function(x){
  x <- as.data.frame(x)
  
  varname <- vector()
  Rsquared <- vector()
  VIF <- vector()
  
  for(i in 1:ncol(x)){
    varname <- c(varname, colnames(x)[i])
    mod <- lm(data=x[,-i], x[,i]~.)
    
    R2 <- summary(mod)$r.squared
    Rsquared <- c(Rsquared,R2)
    
    VIF <- c(VIF,1/(1-R2))
  }
  output <- data.frame(variable=varname, Rsquared=Rsquared, VIF=VIF)
}

(VIF_analysis(e1)) #none over 5
(VIF_analysis(e5)) #none over 5
(VIF_analysis(e6)) #essentially perfect fit: summary may be unreliable
e6<-e6[,-1] #new env matrix
(VIF_analysis(e7))
(VIF_analysis(e7[,-1])) #none over 5
e7<-e7[,-1] #new env matrix
(VIF_analysis(e1.5)) #market VIF > 5 
(VIF_analysis(e1.5[-11]))
e1.5<-e1.5[,11] #new env matrix
(VIF_analysis(e1.5.7)) #market VIF > 5 
(VIF_analysis(e1.5.7[,-14, -3]))
(VIF_analysis(dplyr::select(e1.5.7, -3, -14))) #remove algae and sst
e.1.5.7<-dplyr::select(e1.5.7, -3, -14) #new env matrix

#################################

### PCA of environmental variables (reduced) 
#e31.stand <- scale(e31)
#env.pca <- rda(e31.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

#feeding biomass
#e32.stand <- e32
#env.pca <- rda(e32.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

#feeding density
#e33.stand <- e33
#env.pca <- rda(e33.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

#feeding richness
#e34.stand <- scale(e34)
#env.pca <- rda(e34.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

#distance to things
#e35.stand <- scale(e35)
#env.pca <- rda(e35.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

#benthic
#e36.stand <- e36
#env.pca <- rda(e36.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

#cover
#e37.stand <- e37 #scale or no
#env.pca <- rda(e37.stand)
#screeplot(env.pca, bstick=TRUE)
#summary(env.pca)
#cleanplot.pca(env.pca, scaling = 1)

### PCA of environmental variables 
e1.stand <- scale(e1)
env.pca <- rda(e1.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.3741 0.5951
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#feeding biomass
e2.stand <- scale(e2)
env.pca <- rda(e2.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.3477 0.5173
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#feeding density
e3.stand <- scale(e3)
env.pca <- rda(e3.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.2994 0.5023
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#feeding richness
e4.stand <- scale(e4)
env.pca <- rda(e4.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.3659 0.5463
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#distance to things
e5.stand <- scale(e5)
env.pca <- rda(e5.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.3598 0.5987
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#benthic
e6.stand <- e6
env.pca <- rda(e6.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion   0.6473   0.8390 
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)
#!!!Acr_br, #CCA, #turf

View(e7)
#cover
e7.stand <- scale(e7) 
env.pca <- rda(e7.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.348 0.5487
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#general + distance
e1.5.stand <- scale(e1.5) 
env.pca <- rda(e1.5.stand)
par(mfrow = c(1,1))
screeplot(env.pca, bstick=TRUE)
summary(env.pca) #Cumulative Proportion 0.348 0.5487
par(mfrow = c(1,2))
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

#=====================================================================================
#Environmental data: 
#after data visualization, we are most interested in e1 (general), e5 (distances), e7 (benthic cover)
#tonga.env = e1 + e5 + e7
#e1.5 = e1 + e5 
#we will see what should be tested separately, together, and finish with a clean tonga.env

site.scores <- as.data.frame(scores(tonga.dc.pca, display = "sites", scaling = 1))

#general env
mod.pc1.e1 <- lm(site.scores$PC1~., e1)
summary(mod.pc1.e1) #sst, rugosity
mod.pc2.e1 <- lm(site.scores$PC2~., e1)
summary(mod.pc2.e1) #sst

#distances
mod.pc1.e5 <- lm(site.scores$PC1~., e5)
summary(mod.pc1.e5) #20m, market, village
mod.pc2.e5 <- lm(site.scores$PC2~., e5)
summary(mod.pc2.e5) #20m, lagoon, village

#benthic cover
mod.pc1.e7 <- lm(site.scores$PC1~., e7)
summary(mod.pc1.e7) #invert
mod.pc2.e7 <- lm(site.scores$PC2~., e7)
summary(mod.pc2.e7) #none? 


#Stepwise selection 
#PC1
zero.pc1.e1<- lm(site.scores$PC1~1, e1)
mod.pc1.e1.red <- step(zero.pc1.e1, formula(mod.pc1.e1), direction="both")
summary(mod.pc1.e1.red) 
mod.pc1.e1.red$anova
# AIC=-250.15 : PC1 ~ sst +  rugosity 

#PC2
zero.pc2.e1 <- lm(site.scores$PC2~1, e1)
mod.pc2.e1.red <- step(zero.pc2.e1, formula(mod.pc2.e1), direction="both")
summary(mod.pc2.e1.red)
# AIC=-254.87 : PC2 ~depth 


#PC1
zero.pc1.e5<- lm(site.scores$PC1~1, e5)
mod.pc1.e5.red <- step(zero.pc1.e5, formula(mod.pc1.e5), direction="both")
summary(mod.pc1.e5.red)
#AIC=-261.39: PC1 ~ market + `20m` + village 

#PC2
zero.pc2.e5 <- lm(site.scores$PC2~1, e5)
mod.pc2.e5.red <- step(zero.pc2.e5, formula(mod.pc2.e5), direction="both")
summary(mod.pc2.e5.red)
#AIC=-273.89: PC1 ~ `20m` + market + village + lagoon

#PC1
zero.pc1.e7<- lm(site.scores$PC1~1, e7)
mod.pc1.e7.red <- step(zero.pc1.e7, formula(mod.pc1.e7), direction="both")
summary(mod.pc1.e7.red)
#AIC=-235.76 - PC1 ~ invert 

#PC2
zero.pc2.e7 <- lm(site.scores$PC2~1, e7)
mod.pc2.e7.red <- step(zero.pc2.e7, formula(mod.pc2.e7), direction="both")
summary(mod.pc2.e7.red)
#AIC=-239.89 - site.scores$PC2 ~ hard + soft

######################
rda.e1 <- rda(tonga.dc, e1)
RsquareAdj(rda.e1) # this provides an unbiased estimate of explained variance (0.1332588)
anova(rda.e1) # this performs a permutation test where the Null Hypothesis is that the models does not explain a 
# significant proportion of the variance of Y.

rda.e5 <- rda(tonga.dc, e5)
RsquareAdj(rda.e5) # 0.1573721
anova(rda.e5)  #sign

rda.e7 <- rda(tonga.dc, e7)
RsquareAdj(rda.e7) #very low..., let's try e6?
anova(rda.e7) #sign

rda.e6 <- rda(tonga.dc, e6)
RsquareAdj(rda.e6) #0.1567958
anova(rda.e6) #sign
#########################

## Direct gradient analysis : redundancy analysis (RDA)
mod0.e1<-rda(tonga.dc~1, e1)
mod1.e1<-rda(tonga.dc~., e1)
step.e1<-ordiR2step(mod0.e1, mod1.e1)
mod1.e1 
step.e1$call #rda(formula = tonga.dc ~ depth + rugosity + sst + wave, data = e1)
step.e1$anova
e1.rda<-rda(tonga.dc ~ depth + rugosity + sst + wave, data = e1) 
RsquareAdj(e1.rda) # 0.1200259

## Direct gradient analysis : redundancy analysis (RDA)
mod0.e5<-rda(tonga.dc~1, e5)
mod1.e5<-rda(tonga.dc~., e5)
step.e5<-ordiR2step(mod0.e5, mod1.e5)
step.e5$call #rda(formula = tonga.dc ~ market + `20m` + village, data = e5)
step.e5$anova
e5.rda<-rda(tonga.dc ~ market + `20m` + village, data = e5) 
RsquareAdj(e5.rda)


## Direct gradient analysis : redundancy analysis (RDA)
mod0.e7<-rda(tonga.dc~1, e7[,-1])
mod1.e7<-rda(tonga.dc~., e7[,-1])
step.e7<-ordiR2step(mod0.e7, mod1.e7)
step.e7
step.e7$anova


## Direct gradient analysis : redundancy analysis (RDA)
mod0.e6<-rda(tonga.dc~1, e6)
mod1.e6<-rda(tonga.dc~., e6)
step.e6<-ordiR2step(mod0.e6, mod1.e6, method="forward")
step.e6$call
step.e6$anova

## Direct gradient analysis : redundancy analysis (RDA)
mod0.e1.5<-rda(tonga.dc~1, e1.5)
mod1.e1.5<-rda(tonga.dc~., e1.5)
step.e1.5<-ordiR2step(mod0.e1.5, mod1.e1.5)
step.e1.5$call
step.e1.5$anova

## Direct gradient analysis : redundancy analysis (RDA)
mod0.e1.5.7<-rda(tonga.dc~1, e1.5.7)
mod1.e1.5.7<-rda(tonga.dc~., e1.5.7)
tonga.env<-ordiR2step(mod0.e1.5.7, mod1.e1.5.7)
tonga.env$call
mod1.e1.5.7$call
tonga.env$anova


#Plot 
source("triplot.rda.R")
par(mfrow = c(1,2))

# PRETTY MUCH THE SAME, COMPARE TRIPLOTS
#e1.sel <- forward.sel(tonga.dc, e1)
#rda.e1.sel <- rda(tonga.dc, e1[e1.sel$order])
#summary(rda.e1.sel) # Big output, but DO do look at it.
#RsquareAdj(rda.e1.sel)
#anova(rda.e1.sel)
#par(mfrow = c(1,1))
#triplot.rda(rda.e1.sel, site.sc = "wa")
#triplot.rda(step.e1, site.sc = "wa")
#triplot.rda(rda.e1.sel)
#triplot.rda(step.e1)



e1.sp <- goodness(step.e1)
e1.sp<-which((e1.sp[,2] >= 0.3)==TRUE)
triplot.rda(step.e1, select.spe = e1.sp, site.sc="wa")
triplot.rda(step.e1, select.spe = e1.sp, site.sc="lc")

##########################################################
##Plot
## extract % explained by the first 2 axes
perc.e1 <- round(100*(summary(step.e1)$cont$importance[2, 1:2]), 2)
## extract scores - these are coordinates in the RDA space
step.e1.si <- scores(step.e1, display="sites", choices=c(1,2), scaling=1)
step.e1.si<- as.data.frame(step.e1.si)
step.e1.si <- tibble::rownames_to_column(step.e1.si, "sites")# add rownames as column
step.e1.plot<-merge(step.e1.si, ward.f, by = "sites")
step.e1.plot<-column_to_rownames(step.e1.plot,var="sites")
step.e1.plot


e1.ward1<-step.e1.plot[step.e1.plot$ward.k9 == "1",]
e1.ward2<-step.e1.plot[step.e1.plot$ward.k9 == "2",]
e1.ward3<-step.e1.plot[step.e1.plot$ward.k9 == "3",]
e1.ward4<-step.e1.plot[step.e1.plot$ward.k9 == "4",]
e1.ward5<-step.e1.plot[step.e1.plot$ward.k9 == "5",]
e1.ward6<-step.e1.plot[step.e1.plot$ward.k9 == "6",]
e1.ward7<-step.e1.plot[step.e1.plot$ward.k9 == "7",]
e1.ward8<-step.e1.plot[step.e1.plot$ward.k9 == "8",]
e1.ward9<-step.e1.plot[step.e1.plot$ward.k9 == "9",]

step.e1.sp <- scores(step.e1, display="species", choices=c(1,2), scaling=1)
which((step.e1.sp[,2] >= 0.4)==TRUE)
step.e1.sp<- step.e1.sp[c(118, 233, 235, 240, 303 ),]
const=0.15
step.e1.sp.R2<-step.e1.sp*const


step.e1.bp <- scores(step.e1, display="bp", choices=c(1,2), scaling=1)

# Set up a blank plot with scaling, axes, and labels
palette(hcl.colors(10, palette = "Dynamic"))
par(mfrow=c(1,1))
plot(step.e1,
     scaling = 1, # set scaling type 
     type = "none", # this excludes the plotting of any points from the results
     frame = TRUE,
     # set axis limits
     ylim = c(-.25,.25),
     xlim = c(-.25,.3),
     # label the plot (title, and axes)
     main = "Triplot RDA - scaling 1",
     xlab = paste0("RDA1 (", perc.e1[1], "%)"), 
     ylab = paste0("RDA2 (", perc.e1[2], "%)") 
)
points(data = e1.ward1, RDA2~RDA1, pch = 19, col = "2")+
  points(data = e1.ward2, RDA2~RDA1, pch = 19, col = "3")+
  points(data = e1.ward3, RDA2~RDA1, pch = 19, col = "4")+
  points(data = e1.ward4, RDA2~RDA1, pch = 19, col = "5")+
  points(data = e1.ward5, RDA2~RDA1, pch = 19, col = "6")+
  points(data = e1.ward6, RDA2~RDA1, pch = 19, col = "7")+
  points(data = e1.ward7, RDA2~RDA1, pch = 19, col = "8")+
  points(data = e1.ward8, RDA2~RDA1, pch = 19, col = "9")+
  points(data = e1.ward9, RDA2~RDA1, pch = 19, col = "10")+
  text(x = step.e1.plot[,1]+0.02, # adjust text coordinate to avoid overlap with arrow tip
       y = step.e1.plot[,2], 
       labels = rownames(step.e1.plot), 
       col = "black", 
       cex = .5, 
       font = 1)
# add legend 
legend("bottomright",
       legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9"),
       col = c("2", "3", "4", "5", "6", "7", "8", "9", "10"),
       pch=19,
       cex=1,
       bty="n")
# add points for species scores
arrows(0,0,
       step.e1.sp.R2[,1],step.e1.sp.R2[,2], 
       col = "red",
       lwd = 1,
       length = .1)
# add text labels for species abbreviations
text(step.e1.sp.R2,#+ c(-0.09, -0.09), # adjust text coordinates to avoid overlap with points 
     labels = rownames(step.e1.sp.R2), 
     col = "red", 
     font = 1, # bold
     cex = 0.6)
# add arrows for effects of the expanatory variables
arrows(0,0, # start them from (0,0)
       step.e1.bp[,1], step.e1.bp[,2], # end them at the score value
       col = "blue", 
       lwd = 1,
       length = .1)
# add text labels for arrows
text(x = step.e1.bp[,1] , # adjust text coordinate to avoid overlap with arrow tip
     y = step.e1.bp[,2] , 
     labels = rownames(step.e1.bp), 
     col = "blue", 
     cex = .7, 
     font = 1)

#All these analyses could be performed on presence-absence data. What results would you expect by doing this ? 
#Will patterns be more strongly expressed or not ? To transform  community data to presence absence you can use:
tonga.pa <- decostand(tonga.a, method="pa")





