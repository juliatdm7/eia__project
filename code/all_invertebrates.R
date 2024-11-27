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
#All invertebrates analysis#

all.inverts <- raw.data.all[1:235,]

orderNA <- c(which(is.na(all.inverts$order))) ##which species have NA’s
i.data <- all.inverts[-orderNA,]
View(i.data)
