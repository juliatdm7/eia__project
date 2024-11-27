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

#We're gonna perform analysis for Order level, so we'll erase rows with NAs for rows:
orderNA <- c(which(is.na(fi.raw.data$order))) ##which species have NA’s
fi.data <- fi.raw.data[-orderNA,]
View(fi.data)

# Creating one dataframe for each site
dat.A <- fi.data[which(fi.data$site=="A"),]
dat.B <- fi.data[which(fi.data$site=="B"),]


fi.dat.red <- fi.data[,c("order","site","individualCount")]
unique(fi.dat.red$order) # We can see that across both sites, 14 Orders were identified
fi.dat.redagg <- fi.dat.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop") # We aggregate difference samples of the same Orders
fi.dat.ab <- fi.dat.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(fi.dat.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(fi.dat.ab) # we assign the species names to this list of 0s
fi.dat.ab <- as.data.frame(fi.dat.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(fi.dat.ab) <- fi.dat.ab$site # here we give to each row the name of its corresponding island
fi.dat.ab <- fi.dat.ab[,-1] # we remove the island column because we don't need it anymore, as we have given island names to all rows
