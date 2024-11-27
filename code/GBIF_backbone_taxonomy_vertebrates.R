##########################################################
###GBIF Backbone Taxonomy Comprobation for Bird Species###
##########################################################

library(readxl)
library(rgbif)
bird.vert.north.south <- read_excel("data/EIA_data.xlsx", sheet = "Birds")
View(bird.vert.north.south)
