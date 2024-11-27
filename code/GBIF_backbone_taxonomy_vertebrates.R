##########################################################
###GBIF Backbone Taxonomy Comprobation for Bird Species###
##########################################################

library(readxl)
library(rgbif)
bird.vert.north.south <- read_excel("data/EIA_data.xlsx", sheet = "Birds")
View(bird.vert.north.south)

table <- data.frame(scientificName=as.character(unique(bird.vert.north.south$scientificName)))
write.csv(table, 'data/Birds_North_South.csv', row.names = F)
spp <- unique(bird.vert.north.south$scientificName)
n_spp <- length(spp)

# Initialize the data frames for a species that is sure to be in the taxonomy
spp.check.ok <- name_backbone("Ardea cinerea", verbose = TRUE, strict = TRUE)
spp.check.ok <- spp.check.ok[-c(1:nrow(spp.check.ok)), ]  # Remove all the rows
spp.check.ok <- spp.check.ok[,-which(names(spp.check.ok) %in% c("acceptedUsageKey","family","genus","familyKey","genusKey","class","classKey"))]

# Initialize a data frame for a bad word (not a species name)
spp.check.bad <- name_backbone("xxx", verbose = TRUE, strict = TRUE)
spp.check.bad <- spp.check.bad[-1, ]  # Remove this row
