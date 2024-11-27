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
#Terrestrial invertebrates analysis#

north_ti <- read_excel("data/Arran_data1.xlsx", sheet = "North side invert transects")
south_ti <- read_excel("data/Arran_data1.xlsx", sheet = "South side invert transects")
ti.raw.data <- rbind(north_ti,south_ti) # combining site A and site B data into a single data frame for easier analysis
str(ti.raw.data)
head(ti.raw.data) # previewing the first few rows

length(which(ti.raw.data$site=="North")) # 26 taxa recordings in site A, classified as North
length(which(ti.raw.data$site=="South")) # 80 taxa recordings in site B, classified as South

for (i in 1:nrow(ti.raw.data)) {
  if (ti.raw.data[i, "site"] == "North") {
    ti.raw.data[i, "site"] <- "A"
  } else if (ti.raw.data[i, "site"] == "South") {
    ti.raw.data[i, "site"] <- "B"
  }
} # Modyfing data frame so "North" site is considered site A and "South" site is considered site B

length(which(ti.raw.data$site=="A"))
length(which(ti.raw.data$site=="B")) #comprobation
