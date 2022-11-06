my_libraries <- c("tidyverse", "skimr", "sf", "readxl", "kableExtra",
                  "lubridate", "janitor", "caper", "RCurl", "XML", "dplyr", "reshape", "writexl", "robis", "rfishbase",
                  "adespatial", "vegan", "SoDA", "janitor", "kableExtra", "readxl", "fuzzySim")
sapply(my_libraries, library, character.only = TRUE)

tongu_data <- read_excel("tongu_data.xlsx", 
                         sheet = "tongu", col_types = c("date", "skip", "skip", "numeric", "numeric", 
                                                        "numeric", "text", "text", "numeric", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "text", "numeric", "numeric", "numeric", 
                                                        "numeric", "text", "text", "text", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric"))


#Change default palette to colour blind friendly
n <- 10
h1 <- hcl.colors(10, palette = "Dynamic")
palette(h1)
h1

#Cast data to make community matrix Y 
#using density --> we are not going to use transects, make dataset smaller
#Instead, using library(janitor) and library(fuzzySim)
tonga<-tongu_data %>% clean_names(.,case ="snake")
tonga<-tongu_data
specieslist<-(as.data.frame(unique(tonga$species)))
tonga$species<-spCodes(tonga$species, nchar.gen = 3, nchar.sp = 9, nchar.ssp = 0,
                       sep.species = " ", sep.spcode = ".", verbosity = 2)
specieslist<- cbind(specieslist, (unique(tonga$species)))
#tonga<-tonga%>% group_by(species) %>% filter(n()>= 5) %>% ungroup()
sitelist<-(as.data.frame(unique(tonga$site)))
tonga$site<-abbreviate(tonga$site)
sitelist<- cbind(sitelist, (unique(tonga$site)))


#Raw Abundance 
tonga.a<-cast(tonga, formula = site~species, sum, value = "abundance", fill = 0) #abundance
tonga.a<-column_to_rownames(tonga.a, var ="site")

#Density per km
tonga.d<-cast(tonga, formula = site~species, sum, value = "density", fill = 0) #
tonga.d<-column_to_rownames(tonga.d, var = "site")

#chord using library(vegan)
tonga.dc <- decostand(tonga.d, "nor")
tonga.dc.d <- dist(tonga.dc)
tonga.dc.pca <- rda(tonga.dc)
summary(tonga.dc.pca)
screeplot(tonga.dc.pca, bstick=TRUE)  

#Biomass kg/km
tonga.bkm<-cast(tonga, formula = site~species, sum, value  = "biomass_1000mkg", fill = 0) #biomass
tonga.bkm<-column_to_rownames(tonga.bkm, var ="site")

#chord using library(vegan)
tonga.bkmc <- decostand(tonga.bkm, "nor")
tonga.bkmc.d <- dist(tonga.bkmc)
tonga.bkmc.pca <- rda(tonga.bkmc)
summary(tonga.bkmc.pca)
screeplot(tonga.bkmc.pca, bstick=TRUE)  


#make nicer plots with cleanplot() function
source("cleanplot.pca.R")
palette("default")
cleanplot.pca(tonga.dc.pca, scaling=2)
cleanplot.pca(tonga.dc.pca, scaling=1) #we are interested in this one this one since we want to look at association between objects not species
#which species contribute most in the differences seen in sites, not correlation between species and stuff
species.R2 <- goodness(tonga.dc.pca, display="species", model="CA")
par(mfrow = c(1, 2))
cleanplot.pca(tonga.dc.pca, scaling = 1, select.spe = species.R2[,2] >= 0.4) # with 0.45 all species outside circle
cleanplot.pca(tonga.dc.pca, scaling = 1, ax2=3, select.spe = species.R2[,2] >= 0.4) 
#What if we look at consider the size/weight of the fish? with biomass per km 
species.R2 <- goodness(tonga.bkmc.pca, display="species", model="CA")
cleanplot.pca(tonga.bkmc.pca, scaling = 1, select.spe = species.R2[,2] >= 0.4) # with 0.45 all species outside circle
cleanplot.pca(tonga.bkmc.pca, scaling = 1, ax2=3, select.spe = species.R2[,2] >= 0.4) 
#we get different results --> large schools of small fish, vs.medium sch
#On axis 3 Amblyglyphidodon melanopterus
#If we take off rare species with:
#tonga<-tonga%>% group_by(species) %>% filter(n()>= 5) %>% ungroup()
#we get a slight decrease in overall variance explained, and also nothing changes in the species responsible for most variation
#looking at both PC1~PC2, and PC1~PC3
#we will use complete data set 
#If we look at abundances, without the correction for transect size, we see similar to density, with some resemblance to biomass/km
#In particular, we see scarus flavipectoralis, Yellowfin Parrotfish which has super high abundances, which are reduced when you 
#divide by 5 instead of 2, 
#PROBLEM
#The abundance and size of all large mobile fish were recorded to species level within a 5-m belt.
#All small, site-attached reef fish species were recorded along a 2-m belt.
#except species, such as the yellowfin parrotfish, range in size from lenghts of 3cm to 27cm 
#this likely includes juveniles, which have different behaviors and schooling patterns to adults (and are found in larger abundances)
#biomasses are also based on species level --> 
#The length and abundance of reef fish were converted to biomass following published length–weight relationships for each species



#extract sites/species scores 
## extract % explained by the first 2 axes
perc <- round(100*(summary(tonga.dc.pca)$cont$importance[2, 1:2]), 2)
site.scores <- as.data.frame(scores(tonga.dc.pca, display = "sites", scaling = 1))
site.scores<-tibble::rownames_to_column(site.scores, "sites")
site.scores
spec.scores <- as.data.frame(scores(tonga.dc.pca, display=c("species"), scaling=1, center=TRUE))
spec.scores
species.R2 <- goodness(tonga.dc.pca, display="species", model="CA")
which((species.R2[,2] >= 0.45)==TRUE)
spec.scores.45 <- spec.scores[c(18,94,98,102,233,235),]
spec.scores.45
const=0.19
spec.scores.45<-spec.scores.45*const
#scale.fac <- attributes(spec.scores.45)$const
#scale.fac
#rad <- sqrt(2 /60)
#rad <- scale.fac * rad

## Hierarchical clustering
#Once you have an adequat distance matrix you can compute hierarchichal clustering using different --> see "clustering"
#ward
tonga.ward <- hclust(tonga.dc.d, method = "ward.D2") # ward clustering
plot(tonga.ward, main = "Ward")

#You can use the *cutree* function to tranform a grouping level to a (unordered) factor, for example: 
ward.k9 <- cutree(tonga.ward, k=9)
ward.f<-as.data.frame(ward.k9)
ward.f
ward.f <- tibble::rownames_to_column(ward.f, "sites")
ward.plot <- merge(site.scores, ward.f, by="sites")
ward.plot<-column_to_rownames(ward.plot,var="sites")
ward.plot

ward1<-ward.plot[ward.plot$ward.k9 == "1",]
ward2<-ward.plot[ward.plot$ward.k9 == "2",]
ward3<-ward.plot[ward.plot$ward.k9 == "3",]
ward4<-ward.plot[ward.plot$ward.k9 == "4",]
ward5<-ward.plot[ward.plot$ward.k9 == "5",]
ward6<-ward.plot[ward.plot$ward.k9 == "6",]
ward7<-ward.plot[ward.plot$ward.k9 == "7",]
ward8<-ward.plot[ward.plot$ward.k9 == "8",]
ward9<-ward.plot[ward.plot$ward.k9 == "9",]

#Set up a blank plot with scaling, axes, and labels
par(mfrow = c(1,1))
plot(tonga.dc.pca,
     scaling = 1, # set scaling type 
     type = "none", # this excludes the plotting of any points from the results
     frame = TRUE,
     # set axis limits
     ylim = c(-0.2,0.35),
     xlim = c(-0.2,0.3),
     # label the plot (title, and axes)
     main = "Biplot PCA with Ward Clustering - scaling 1",
     xlab = paste0("PCA1 (", perc[1], "%)"), 
     ylab = paste0("PCA2 (", perc[2], "%)") 
)
palette(hcl.colors(10, palette = "Dynamic"))
points(data = ward1, PC2~PC1, pch = 19, col = "2")+
  points(data = ward2, PC2~PC1, pch = 19, col = "3")+
  points(data = ward3, PC2~PC1, pch = 19, col = "4")+
  points(data = ward4, PC2~PC1, pch = 19, col = "5")+
  points(data = ward5, PC2~PC1, pch = 19, col = "6")+
  points(data = ward6, PC2~PC1, pch = 19, col = "7")+
  points(data = ward7, PC2~PC1, pch = 19, col = "8")+
  points(data = ward8, PC2~PC1, pch = 19, col = "9")+
  points(data = ward9, PC2~PC1, pch = 19, col = "10")+
  text(x = ward.plot[,1]+0.02, # adjust text coordinate to avoid overlap with arrow tip
       y = ward.plot[,2], 
       labels = rownames(ward.plot), 
       col = "black", 
       cex = .5, 
       font = 1)
arrows(0,0, spec.scores.45[,1], spec.scores.45[,2], col = "1", length = 0.05)
# add text labels for arrows
text(x = spec.scores.45[,1]+0.02 , # adjust text coordinate to avoid overlap with arrow tip
     y = spec.scores.45[,2], 
     labels = rownames(spec.scores.45), 
     col = "1", 
     cex = .7, 
     font = 1)
# add legend 
legend("topright",
       legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9"),
       col = c("2", "3", "4", "5", "6", "7", "8", "9", "10"),
       pch=19,
       cex=1,
       bty="n")

#"FAILED" ATTEMPT BELOW,
#library(viridis)
#ugh<-ggplot(ward.plot, aes(PC1, PC2, col=as.factor(ward.k9), type="p")) + 
#  scale_color_viridis(discrete = TRUE, option = "H") +
#  labs(title="Principal Component Analysis of Sites ",
#       x ="PC1 (18.75%)", y = "PC2 (17.21%)")+
#  geom_point(size=2) +  
#  theme(legend.position="none")+
#  geom_hline(yintercept = 0, lty = 2) +
#  geom_vline(xintercept = 0, lty = 2) +
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#  geom_text(aes(label = rownames(ward.plot), color=NULL), size = 3, nudge_x = 0.012)

###########################################
#UPGMA
tonga.UPGMA <- hclust(tonga.dc.d, method = "average") # UPGMA clustering
plot(tonga.UPGMA, main = "UPGMA")
#You can use the *cutree* function to tranform a grouping level to a (unordered) factor, for example: 
UPGMA.k9 <- cutree(tonga.UPGMA, k=9)
UPGMA.f<-as.data.frame(UPGMA.k9)
UPGMA.f

UPGMA.f <- tibble::rownames_to_column(UPGMA.f, "sites")
UPGMA.plot <- merge(site.scores, UPGMA.f, by="sites")
UPGMA.plot<-column_to_rownames(UPGMA.plot,var="sites")
UPGMA.plot

UPGMA1<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "1",]
UPGMA2<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "2",]
UPGMA3<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "3",]
UPGMA4<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "4",]
UPGMA5<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "5",]
UPGMA6<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "6",]
UPGMA7<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "7",]
UPGMA8<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "8",]
UPGMA9<-UPGMA.plot[UPGMA.plot$UPGMA.k9 == "9",]

#Set up a blank plot with scaling, axes, and labels
par(mfrow = c(1,1))
plot(tonga.dc.pca,
     scaling = 1, # set scaling type 
     type = "none", # this excludes the plotting of any points from the results
     frame = TRUE,
     # set axis limits
     ylim = c(-0.2,0.35),
     xlim = c(-0.2,0.3),
     # label the plot (title, and axes)
     main = "Biplot PCA with UPGMA Clustering- scaling 1",
     xlab = paste0("PCA1 (", perc[1], "%)"), 
     ylab = paste0("PCA2 (", perc[2], "%)") 
)
points(data = UPGMA1, PC2~PC1, pch = 19, col = "2")+
  points(data = UPGMA2, PC2~PC1, pch = 19, col = "3")+
  points(data = UPGMA3, PC2~PC1, pch = 19, col =  "4")+
  points(data = UPGMA4, PC2~PC1, pch = 19, col = "5")+
  points(data = UPGMA5, PC2~PC1, pch = 19, col = "6")+
  points(data = UPGMA6, PC2~PC1, pch = 19, col = "7")+
  points(data = UPGMA7, PC2~PC1, pch = 19, col = "8")+
  points(data = UPGMA8, PC2~PC1, pch = 19, col = "9")+
  points(data = UPGMA9, PC2~PC1, pch = 19, col = "10")+
  text(x = UPGMA.plot[,1]+0.02, # adjust text coordinate to avoid overlap with arrow tip
       y = UPGMA.plot[,2], 
       labels = rownames(UPGMA.plot), 
       col = "black", 
       cex = .5, 
       font = 1)
arrows(0,0, spec.scores.45[,1], spec.scores.45[,2], col = "1", length = 0.05)
# add text labels for arrows
text(x = spec.scores.45[,1]+0.02 , # adjust text coordinate to avoid overlap with arrow tip
     y = spec.scores.45[,2], 
     labels = rownames(spec.scores.45), 
     col = "1", 
     cex = .7, 
     font = 1)
# add legend 
legend("topright",
       legend=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9"),
       col = c("2", "3", "4", "5", "6", "7", "8", "9", "10"),
       pch=19,
       cex=1,
       bty="n")