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
