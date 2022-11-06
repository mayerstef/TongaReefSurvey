my_libraries <- c("tidyverse", "skimr", "sf", "readxl", "kableExtra",
                  "lubridate", "janitor", "caper", "RCurl", "XML", "dplyr", "reshape2", "writexl", "robis", "rfishbase",
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
##########################

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


#Cast data using library(reshape) abundance, density, biomass
#Abundance
tonga.a<-cast(tonga, formula = site~species, sum, value = "abundance", fill = 0) #abundance
tonga.a<-column_to_rownames(tonga.a, var ="site")

#raw abundance data pca
tonga.a.pca<-rda(tonga.a, scale =TRUE)
summary(tonga.a.pca)
screeplot(tonga.a.pca, bstick=TRUE)  
plot(tonga.a.pca, scaling = 1)

#hellinger
tonga.ah <- decostand(tonga.a, method="hellinger") 
tonga.ah.pca <- rda(tonga.ah)
summary(tonga.ah.pca)
screeplot(tonga.ah.pca, bstick=TRUE)  
plot(tonga.ah.pca, scaling = 1)

#chord using library(vegan)
tonga.ac <- decostand(tonga.a, "nor")
tonga.ac.d <- dist(tonga.ac)
tonga.ac.pca <- rda(tonga.ac)
summary(tonga.ac.pca)
screeplot(tonga.ac.pca, bstick=TRUE)  
plot(tonga.ac.pca, scaling = 1)

#log-chord
tonga.aln <- log1p(tonga.a)
tonga.alogc <- decostand(tonga.aln, "nor")
tonga.alogc.d <- dist(tonga.alogc)
tonga.alogc.pca <- rda(tonga.alogc)
summary(tonga.alogc.pca)
screeplot(tonga.alogc.pca, bstick=TRUE)  
plot(tonga.alogc.pca, scaling = 1)

#box-cox-chord with custom exponent 
box.cox.chord <- 
  function(mat, 
           bc.exp=0.1) { #test with varying exponents (note: bc.exp = 0.5 is the same as hellinger transform)
    # Internal function
    vec.norm <- function(vec)  sqrt(sum(vec^2))
    #
    chck <- apply(mat, 1, sum)
    if(any(chck == 0)) stop("Rows",which(chck==0)," of the data matrix sum to 0")
    #
    # Apply the user-selected Box-Cox exponent (bc.exp) to the frequency data
    if(bc.exp==0) {
      tmp <- log(mat+1) 
    } else { 
      tmp <- mat^bc.exp 
    }
    row.norms <- apply(tmp, 1, vec.norm)
    #
    # Apply the chord transformation to matrix "tmp" before returning it
    res <- sweep(tmp, 1, row.norms, "/")
  }

tonga.abox<-box.cox.chord(tonga.a)
tonga.abox.pca <- rda(tonga.abox)
summary(tonga.abox.pca)
screeplot(tonga.abox.pca, bstick=TRUE)  
plot(tonga.abox.pca, scaling = 1)

#############################################################################################
#Density per 1000m^2
tonga.dkm<-cast(tonga, formula = site~species, sum, value = "density1000", fill = 0)
tonga.dkm<-column_to_rownames(tonga.dkm, var = "site")

tonga.dkmh <- decostand(tonga.dkm, method="hellinger") 
tonga.dkmh.pca <- rda(tonga.dkmh)
summary(tonga.dkmh.pca)
screeplot(tonga.dkmh.pca, bstick=TRUE)  
plot(tonga.dkmh.pca, scaling = 1)

#chord using library(vegan)
tonga.dkmc <- decostand(tonga.dkm, "nor")
tonga.dkmc.d <- dist(tonga.dkmc)
tonga.dkmc.pca <- rda(tonga.dkmc)
summary(tonga.dkmc.pca)
screeplot(tonga.dkmc.pca, bstick=TRUE)  
plot(tonga.dkmc.pca, scaling = 1)

#log-chord
tonga.dkmln <- log1p(tonga.dkm)
tonga.dkmlogc <- decostand(tonga.dkmln, "nor")
tonga.dkmlogc.d <- dist(tonga.dkmlogc)
tonga.dkmlogc.pca <- rda(tonga.dkmlogc)
summary(tonga.dkmlogc.pca)
screeplot(tonga.dkmlogc.pca, bstick=TRUE)  
plot(tonga.dkmlogc.pca, scaling = 1)

#box-cox-chord with custom exponent 
box.cox.chord <- 
  function(mat, 
           bc.exp=0.1) { 
    # Internal function
    vec.norm <- function(vec)  sqrt(sum(vec^2))
    #
    chck <- apply(mat, 1, sum)
    if(any(chck == 0)) stop("Rows",which(chck==0)," of the data matrix sum to 0")
    #
    # Apply the user-selected Box-Cox exponent (bc.exp) to the frequency data
    if(bc.exp==0) {
      tmp <- log(mat+1) 
    } else { 
      tmp <- mat^bc.exp 
    }
    row.norms <- apply(tmp, 1, vec.norm)
    #
    # Apply the chord transformation to matrix "tmp" before returning it
    res <- sweep(tmp, 1, row.norms, "/")
  }

tonga.dkmbox<-box.cox.chord(tonga.dkm)
tonga.dkmbox.pca <- rda(tonga.dkmbox)
summary(tonga.dkmbox.pca)
screeplot(tonga.dkmbox.pca, bstick=TRUE)  
plot(tonga.dkmbox.pca, scaling = 1)
############################################################################################

#Density per m^2
tonga.d<-cast(tonga, formula = site~species, sum, value = "density", fill = 0) #
tonga.d<-column_to_rownames(tonga.d, var = "site")

tonga.dh <- decostand(tonga.d, method="hellinger") 
tonga.dh.pca <- rda(tonga.dh)
summary(tonga.dh.pca)
screeplot(tonga.dh.pca, bstick=TRUE)  
plot(tonga.dh.pca, scaling = 1)

#chord using library(vegan)
tonga.dc <- decostand(tonga.d, "nor")
tonga.dc.d <- dist(tonga.dc)
tonga.dc.pca <- rda(tonga.dc)
summary(tonga.dc.pca)
screeplot(tonga.dc.pca, bstick=TRUE)  
plot(tonga.dc.pca, scaling = 1)
View(tonga.dc)

#log-chord
tonga.dln <- log1p(tonga.d)
tonga.dlogc <- decostand(tonga.dln, "nor")
tonga.dlogc.d <- dist(tonga.dlogc)
tonga.dlogc.pca <- rda(tonga.dlogc)
summary(tonga.dlogc.pca)
screeplot(tonga.dlogc.pca, bstick=TRUE)  
plot(tonga.dlogc.pca, scaling = 1)

#box-cox-chord with custom exponent 
box.cox.chord <- 
  function(mat, 
           bc.exp=0.1) { 
    # Internal function
    vec.norm <- function(vec)  sqrt(sum(vec^2))
    #
    chck <- apply(mat, 1, sum)
    if(any(chck == 0)) stop("Rows",which(chck==0)," of the data matrix sum to 0")
    #
    # Apply the user-selected Box-Cox exponent (bc.exp) to the frequency data
    if(bc.exp==0) {
      tmp <- log(mat+1) 
    } else { 
      tmp <- mat^bc.exp 
    }
    row.norms <- apply(tmp, 1, vec.norm)
    #
    # Apply the chord transformation to matrix "tmp" before returning it
    res <- sweep(tmp, 1, row.norms, "/")
  }

tonga.dbox<-box.cox.chord(tonga.d)
tonga.dbox.pca <- rda(tonga.dbox)
summary(tonga.dbox.pca)
screeplot(tonga.dbox.pca, bstick=TRUE)  
plot(tonga.dbox.pca, scaling = 1)
#############################################################################################
#Biomass kg
tonga.b<-cast(tonga, formula = site~species, sum, value  = "biomass_kg", fill = 0) #biomass
tonga.b<-column_to_rownames(tonga.b, var ="site")

tonga.bh <- decostand(tonga.b, method="hellinger") 
tonga.bh.pca <- rda(tonga.bh)
summary(tonga.bh.pca)
screeplot(tonga.bh.pca, bstick=TRUE)  
plot(tonga.bh.pca, scaling = 1)

#chord using library(vegan)
tonga.bc <- decostand(tonga.b, "nor")
tonga.bc.d <- dist(tonga.bc)
tonga.bc.pca <- rda(tonga.bc)
summary(tonga.bc.pca)
screeplot(tonga.bc.pca, bstick=TRUE)  
plot(tonga.bpca, scaling = 1)

#log-chord
tonga.bln <- log1p(tonga.b)
tonga.blogc <- decostand(tonga.bln, "nor")
tonga.blogc.d <- dist(tonga.blogc)
tonga.blogc.pca <- rda(tonga.blogc)
summary(tonga.blogc.pca)
screeplot(tonga.blogc.pca, bstick=TRUE)  
plot(tonga.blogc.pca, scaling = 1)

#box-cox-chord with custom exponent 
box.cox.chord <- 
  function(mat, 
           bc.exp=0.1) { 
    # Internal function
    vec.norm <- function(vec)  sqrt(sum(vec^2))
    #
    chck <- apply(mat, 1, sum)
    if(any(chck == 0)) stop("Rows",which(chck==0)," of the data matrix sum to 0")
    #
    # Apply the user-selected Box-Cox exponent (bc.exp) to the frequency data
    if(bc.exp==0) {
      tmp <- log(mat+1) 
    } else { 
      tmp <- mat^bc.exp 
    }
    row.norms <- apply(tmp, 1, vec.norm)
    #
    # Apply the chord transformation to matrix "tmp" before returning it
    res <- sweep(tmp, 1, row.norms, "/")
  }

tonga.bbox<-box.cox.chord(tonga.b)
tonga.bbox.pca <- rda(tonga.bbox)
summary(tonga.bbox.pca)
screeplot(tonga.bbox.pca, bstick=TRUE)  
plot(tonga.bbox.pca, scaling = 1)

#############################################################################################
#Biomass kg/km
tonga.bkm<-cast(tonga, formula = site~species, sum, value  = "biomass_1000mkg", fill = 0) #biomass
tonga.bkm<-column_to_rownames(tonga.bkm, var ="site")

tonga.bkmh <- decostand(tonga.bkm, method="hellinger") 
tonga.bkmh.pca <- rda(tonga.bkmh)
summary(tonga.bkmh.pca)
screeplot(tonga.bkmh.pca, bstick=TRUE)  
plot(tonga.bkmh.pca, scaling = 1)

#chord using library(vegan)
tonga.bkmc <- decostand(tonga.bkm, "nor")
tonga.bkmc.d <- dist(tonga.bkmc)
tonga.bkmc.pca <- rda(tonga.bkmc)
summary(tonga.bkmc.pca)
screeplot(tonga.bkmc.pca, bstick=TRUE)  
plot(tonga.bkmc.pca, scaling = 1)

#log-chord
tonga.bkmln <- log1p(tonga.bkm)
tonga.bkmlogc <- decostand(tonga.bkmln, "nor")
tonga.bkmlogc.d <- dist(tonga.bkmlogc)
tonga.bkmlogc.pca <- rda(tonga.bkmlogc)
summary(tonga.bkmlogc.pca)
screeplot(tonga.bkmlogc.pca, bstick=TRUE)  
plot(tonga.bkmlogc.pca, scaling = 1)

#box-cox-chord with custom exponent 
box.cox.chord <- 
  function(mat, 
           bc.exp=0.1) { #test with varying exponents (note: bc.exp = 0.5 is the same as hellinger transform)
    # Internal function
    vec.norm <- function(vec)  sqrt(sum(vec^2))
    #
    chck <- apply(mat, 1, sum)
    if(any(chck == 0)) stop("Rows",which(chck==0)," of the data matrix sum to 0")
    #
    # Apply the user-selected Box-Cox exponent (bc.exp) to the frequency data
    if(bc.exp==0) {
      tmp <- log(mat+1) 
    } else { 
      tmp <- mat^bc.exp 
    }
    row.norms <- apply(tmp, 1, vec.norm)
    #
    # Apply the chord transformation to matrix "tmp" before returning it
    res <- sweep(tmp, 1, row.norms, "/")
  }

tonga.bkmbox<-box.cox.chord(tonga.bkm)
tonga.bkmbox.pca <- rda(tonga.bkmbox)
summary(tonga.bkmbox.pca)
screeplot(tonga.bkmbox.pca, bstick=TRUE)  
plot(tonga.bkmbox.pca, scaling = 1)

###############################################################################################################
