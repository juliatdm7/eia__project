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
#Bats analysis#

vert2.dat <- read_excel("data/Arran_data1.xlsx", sheet = "Vertebrates (tech)")

for (i in 1:nrow(vert2.dat)) {
  if (vert2.dat[i, "site"] == "North") {
    vert2.dat[i, "site"] <- "A"
  } else if (vert2.dat[i, "site"] == "South") {
    vert2.dat[i, "site"] <- "B"
  }
}

# There are only two camera recordings for other verts aside from bats, so we'll remove them from the analysis because visualizing them offers no advantage over simply stating the recordings:

bat.dat <- vert2.dat[which(vert2.dat$samplingProtocol=="Audiomoths"),]

