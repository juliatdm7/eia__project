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

# Classifiying for spots
ti.raw.data <- ti.raw.data %>%
  mutate(new_locationID = case_when(
    locationID %in% c("TopRight", "TopMiddle", "TopLeft") ~ "Top",
    locationID %in% c("MiddleRight", "MiddleMiddle", "MiddleLeft") ~ "Middle",
    locationID %in% c("BottomRight", "BottomMiddle", "BottomLeft") ~ "Bottom",
  ))

#We're gonna perform analysis for Order level, so we'll erase rows with NAs for rows:
orderNA <- c(which(is.na(ti.raw.data$order))) ##which species have NA’s
ti.data <- ti.raw.data[-orderNA,]
View(ti.data)

# Creating one dataframe for each site
dat.A <- ti.data[which(ti.data$site=="A"),]
dat.B <- ti.data[which(ti.data$site=="B"),]

# To compute community patterns, we need site by species data frames for each study site
# In this case, we want to compare diversity not only between study sites but also between "heights" (Top, Middle, Bottom), so we'll consider those as "site" in our site by species matrix

##SiteA##
dat.A.red <- dat.A[,c("order","new_locationID","individualCount")]
dat.A.redagg <- dat.A.red %>%
  group_by(order, new_locationID) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.A.ab <- dat.A.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.A.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.A.ab) # we assign the species names to this list of 0s
dat.A.ab <- as.data.frame(dat.A.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.A.ab) <- dat.A.ab$new_locationID # here we give to each row the name of its corresponding island
dat.A.ab <- dat.A.ab[,-1] # we remove the island column because we don't need it anymore, as we have given island names to all rows

##SiteB##
dat.B.red <- dat.B[,c("order","new_locationID","individualCount")]
dat.B.redagg <- dat.B.red %>%
  group_by(order, new_locationID) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.B.ab <- dat.B.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.B.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.B.ab) # we assign the species names to this list of 0s
dat.B.ab <- as.data.frame(dat.B.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.B.ab) <- dat.B.ab$new_locationID # here we give to each row the name of its corresponding island
dat.B.ab <- dat.B.ab[,-1]

# Computing Shannon's and Simpson's diversity indices:
shannon.alpha.dat.A <- diversity(dat.A.ab, index = "shannon") # site A
shannon.alpha.dat.B <- diversity(dat.B.ab, index = "shannon") # site B
simpson.alpha.dat.A <- diversity(dat.A.ab, index = "simpson") # site A
simpson.alpha.dat.B <- diversity(dat.B.ab, index = "simpson") # site B

# Converting abundance data to presence/absence data in order to calculate richness and Soerensen and Jaccard indices:
dat.A.pa <- ifelse(dat.A.ab > 0, 1, 0) # site A
dat.B.pa <- ifelse(dat.B.ab > 0, 1, 0) # site B

#Computing order richness
richness_ti_A <- ncol(dat.A.pa) 
richness_ti_B <- ncol(dat.B.pa)

richness_ti <- data.frame(
  Site = c("A", "B"),
  Richness = c(richness_ti_A, richness_ti_B)
)

# Gamma Diversity for site A
combined.dat.A.ab <- colSums(dat.A.ab) 
gamma.richness.dat.A <- specnumber(combined.dat.A.ab)


##########################INCLUDING BOG TERRESTRIAL SAMPLING#############################################
#Run this to check for bog instead of coding lines between 15 and 79

bog <- read_excel("data/Arran_data1.xlsx", sheet = "BOG")
ti.bog <- bog[which(bog$samplingProtocol=="Sweep netting"),]
orderNA <- c(which(is.na(ti.bog$order))) ## no rows have NA’s for Order
ti.bog.dat <- ti.bog
for (i in 1:nrow(ti.bog.dat)) {
  if (ti.bog.dat[i, "site"] == "North") {
    ti.bog.dat[i, "site"] <- "A"
  } else if (ti.bog.dat[i, "site"] == "South") {
    ti.bog.dat[i, "site"] <- "B"
  }
}
ti.bog.dat$new_locationID <- "Bog"
dat.A.prov <- ti.data[which(ti.data$site=="A"),]
dat.B.prov <- ti.data[which(ti.data$site=="B"),]
dat.it <- rbind(dat.A.prov, dat.B.prov, ti.bog.dat)
dat.A <- dat.it[which(dat.it$site=="A"),]
dat.B <- dat.it[which(dat.it$site=="B"),]

#Site A
dat.it.A.red <- dat.A[,c("order","new_locationID","individualCount")]
dat.A.redagg <- dat.it.A.red %>%
  group_by(order, new_locationID) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.A.ab <- dat.A.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.A.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.A.ab) # we assign the species names to this list of 0s
dat.A.ab <- as.data.frame(dat.A.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.A.ab) <- dat.A.ab$new_locationID # here we give to each row the name of its corresponding island
dat.A.ab <- dat.A.ab[,-1]

##SiteB##
dat.B.red <- dat.B[,c("order","new_locationID","individualCount")]
dat.B.redagg <- dat.B.red %>%
  group_by(order, new_locationID) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.B.ab <- dat.B.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.B.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.B.ab) # we assign the species names to this list of 0s
dat.B.ab <- as.data.frame(dat.B.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.B.ab) <- dat.B.ab$new_locationID # here we give to each row the name of its corresponding island
dat.B.ab <- dat.B.ab[,-1]
###########################################################################################################################################################################################################################################################################


