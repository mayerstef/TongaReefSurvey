my_libraries <- c("tidyverse", "skimr", "sf", "readxl", "kableExtra",
                  "lubridate", "janitor", "caper", "RCurl", "XML", "dplyr", "reshape", "writexl", "robis", "rfishbase",
                  "adespatial", "vegan", "SoDA", "janitor", "kableExtra", "readxl")

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

library(readxl)
specieslist <- read_excel("specieslist.xlsx")
View(specieslist)

#Cast data to make community matrix Y 
#using density --> we are not going to use transects, make dataset smaller
#Instead, using library(janitor) and library(fuzzySim)
tonga<-tongu_data %>% clean_names(.,case ="snake")
tonga<-tongu_data
specieslist<-(as.data.frame(unique(tonga$species)))
names(specieslist)[1] <- "list"

library(rfishbase)
fish<-rfishbase::validate_names(species_list = specieslist$list)


library(fishtree)
#tree1<-fishtree_phylogeny(species = "specieslist")#forget this
tips<-gsub(" ", "_", fish, fixed=TRUE)
tips


library(ape)
tree<-read.tree(file = "./full.trees")
#tips<-fish ## my tips to keep
pruned<-lapply(tree,function(tree,tips)
  drop.tip(tree,setdiff(tree$tip.label,tips)),tips=tips)
class(pruned)<-"multiPhylo"
write.tree(pruned)
write.tree(pruned, file = "pruned_tree", append = FALSE,
           digits = 10, tree.names = FALSE)

#This function scans a list of trees, and returns a list with the duplicate trees removed. By default the
#labelled topologies are compared.
unique_trees<-unique(pruned, incomparables = FALSE,
                     use.edge.length = FALSE,
                     use.tip.label = TRUE)

Ntip(unique_trees) #number of unique tips
Ntip(pruned) #they are the same, was just checking 


library(TreeTools)
pruned_fish<-AllTipLabels(pruned)
as.data.frame(pruned_fish)
write.csv(pruned_fish, file="specieslist_new302")
fish2<-gsub(" ", "_", fish, fixed=TRUE)
fish3<-setdiff(fish2, pruned_fish)
fish3 #species that don't have a tree