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
#Freshwater invertebrates analysis#

fi.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "N+S Stream inverts")

for (i in 1:nrow(fi.raw.data)) {
  if (fi.raw.data[i, "site"] == "North") {
    fi.raw.data[i, "site"] <- "A"
  } else if (fi.raw.data[i, "site"] == "South") {
    fi.raw.data[i, "site"] <- "B"
  }
} # Modyfing data frame so "North" site is considered site A and "South" site is considered site B
