################################################################################
############################Biodiversity Analysis###############################
################################################################################

library(readxl)
library(tidyverse)
library(vegan)
library(betapart)
library(dplyr)
library(colorblindr)

################################################################################
#Birds analysis#
vert1.dat <- read_excel("data/Arran_data1.xlsx", sheet = "Vertebrates")

for (i in 1:nrow(vert1.dat)) {
  if (vert1.dat[i, "site"] == "North") {
    vert1.dat[i, "site"] <- "A"
  } else if (vert1.dat[i, "site"] == "South") {
    vert1.dat[i, "site"] <- "B"
  }
}

unique(vert1.dat$class) # In the excel sheet there are recordings of amphibians, reptiles, birds and mammals
length(which(vert1.dat$class!="Aves")) # However, only 3 recordings (including both study sites) are not of bird species

# Therefore, we'll focus on birds for now:
bird.dat <- vert1.dat[vert1.dat$class == "Aves", ]

# To compute community patterns, we need site by species data frames for each study site:
#bird.dat <- bird.dat.A[!duplicated(bird.dat.A$scientificName), ]
bird.dat.red.prov <- bird.dat[,c("scientificName","site")]
bird.dat.red <- unique(bird.dat.red.prov)
bird.dat.red$presence <- 1
bird.dat.pa <- bird.dat.red %>%  
  pivot_wider(names_from=scientificName,values_from=c(presence)) 
list0 <- as.list(rep(0,ncol(bird.dat.pa)))
names(list0) <- names(bird.dat.pa)
bird.dat.pa <- as.data.frame(bird.dat.pa %>% replace_na(list0))
row.names(bird.dat.pa) <- bird.dat.pa$site
bird.dat.pa <- bird.dat.pa[,-1]
