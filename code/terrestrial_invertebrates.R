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
richness_ti_A <- ncol(dat.A.pa)  # 6 orders present in Site A
richness_ti_B <- ncol(dat.B.pa)  # 9 orders present in Site B

#richness_ti <- data.frame(
#  Site = c("A", "B"),
#  Richness = c(richness_ti_A, richness_ti_B)
#)

# Gamma Diversity for site A
combined.dat.A.ab <- colSums(dat.A.ab) 
gamma.richness.dat.A <- specnumber(combined.dat.A.ab)

###########
###Plots###
###########

# Orders per site per "height" category:
dat.A.redagg$site <- "A"
dat.B.redagg$site <- "B"
dat.it.redagg <- rbind(dat.A.redagg, dat.B.redagg)

orders__height_plot <- ggplot(dat.it.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Abundance of orders per site (no bog, no moths)", x = "Orders", y = "Abundance") +
  facet_wrap(~new_locationID) + 
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Orders_comparison_between_heights_WITHOUT_BOG_WITHOUT_MOTHS.png")

# Orders per site (ditching faceting by "height")

dat.A.h.red <- dat.A[,c("order","site","individualCount")]
dat.A.h.redagg <- dat.A.h.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.B.h.red <- dat.B[,c("order","site","individualCount")]
dat.B.h.redagg <- dat.B.h.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")

dat.A.h.redagg$site <- "A"
dat.B.h.redagg$site <- "B"
dat.it.h.redagg <- rbind(dat.A.h.redagg, dat.B.h.redagg)

#Small pause in the graphs to calculate Shannon's and Simpson's indices for whole sites (A and B)
dat.it.h.ab <- dat.it.h.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.it.h.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.it.h.ab) # we assign the species names to this list of 0s
dat.it.h.ab <- as.data.frame(dat.it.h.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.it.h.ab) <- dat.it.h.ab$site # here we give to each row the name of its corresponding island
dat.it.h.ab <- dat.it.h.ab[,-1]
shannon.tot <- diversity(dat.it.h.ab, index = "shannon")
simpson.tot <- diversity(dat.it.h.ab, index = "simpson")
ncol(dat.it.h.ab) # Total number of orders sampled

#Back to the graph of orders per site (ditching faceting by "height")

orders_plot <- ggplot(dat.it.h.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Abundance of orders per site (no bog, no moths)", x = "Orders", y = "Abundance") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Orders_comparison_WITHOUT_BOG_WITHOUT_MOTHS.png")

# Graphic representation of indices

# Including these in a data frame (WITHOUT BOG): might be wrong,
alpha_div <- data.frame(
  Site = rep(c("A", "B"), 6),
  Index = rep(c("Shannon", "Simpson"), each = 6),
  Height = rep(c("Bottom", "Bottom", "Middle", "Middle", "Top", "Top"), 2),
  Value = c(shannon.alpha.dat.A, shannon.alpha.dat.B, simpson.alpha.dat.A, simpson.alpha.dat.B) # Replace with actual results
)

# preparing a data frame for Shannon index
shannon <- alpha_div[which(alpha_div$Index=="Shannon"),]

# Plotting the differences in Shannon index by site and height:
shannon_plot <- ggplot(shannon, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Shannon Index Comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Height) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Shannon_indices_WITH_BOG.png")
cvd_grid(shannon_plot) # checking that the chosen colour palette is suitable for colourblindness

# preparing a data frame for Simpson index
simpson <- alpha_div[which(alpha_div$Index=="Simpson"),]
# Plotting the differences in Simpson index by site and height:
simpson_plot <- ggplot(simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Simpson Index Comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Height) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Simpson_indices_WITH_BOG.png")
cvd_grid(simpson_plot) # checking that the chosen colour palette is suitable for colourblindness (although we're using the same colours and we've seen it's colourblindness safe)


##########################INCLUDING BOG TERRESTRIAL SAMPLING#############################################

########################WARNING#########################
#Run at least until line 50 before running this section#
########################WARNING#########################


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


# Including these in a data frame (WITH BOG):
alpha_div <- data.frame(
  Site = rep(c("A", "A", "A", "B", "B", "B", "B"), 2),
  Index = rep(c("Shannon", "Simpson"), each = 7),
  Height = rep(c("Bottom", "Middle", "Top", "Bog", "Bottom", "Middle", "Top"), 2),
  Value = c(shannon.alpha.dat.A, shannon.alpha.dat.B, simpson.alpha.dat.A, simpson.alpha.dat.B) # Replace with actual results
)
# preparing a data frame for Shannon index
shannon <- alpha_div[which(alpha_div$Index=="Shannon"),]
# Plotting the differences in Shannon index by site and height:
shannon_plot <- ggplot(shannon, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Shannon Index Comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Height) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Shannon_indices_WITH_BOG.png")
cvd_grid(shannon_plot) # checking that the chosen colour palette is suitable for colourblindness

# preparing a data frame for Simpson index
simpson <- alpha_div[which(alpha_div$Index=="Simpson"),]
# Plotting the differences in Simpson index by site and height:
simpson_plot <- ggplot(simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Simpson Index Comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Height) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Simpson_indices_WITH_BOG.png")
cvd_grid(simpson_plot) # checking that the chosen colour palette is suitable for colourblindness (although we're using the same colours and we've seen it's colourblindness safe)

# Orders accross habitats and sites

dat.A.redagg$site <- "A"
dat.B.redagg$site <- "B"
dat.redagg <- rbind(dat.A.redagg,dat.B.redagg)

orders_plot <- ggplot(dat.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Abundance of orders per site", x = "Orders", y = "Abundance") +
  facet_wrap(~new_locationID) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 20), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Orders_comparison_WITH_BOG_WITHOUT_MOTHS.png")

# Orders per site (ditching faceting by "height")

dat.A.h.red <- dat.A[,c("order","site","individualCount")]
dat.A.h.redagg <- dat.A.h.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.B.h.red <- dat.B[,c("order","site","individualCount")]
dat.B.h.redagg <- dat.B.h.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")

dat.A.h.redagg$site <- "A"
dat.B.h.redagg$site <- "B"
dat.it.h.redagg <- rbind(dat.A.h.redagg, dat.B.h.redagg)

#Small pause in the graphs to calculate Shannon's and Simpson's indices for whole sites (A and B)
dat.it.h.ab <- dat.it.h.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.it.h.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.it.h.ab) # we assign the species names to this list of 0s
dat.it.h.ab <- as.data.frame(dat.it.h.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.it.h.ab) <- dat.it.h.ab$site # here we give to each row the name of its corresponding island
dat.it.h.ab <- dat.it.h.ab[,-1]
shannon.tot <- diversity(dat.it.h.ab, index = "shannon")
simpson.tot <- diversity(dat.it.h.ab, index = "simpson")

#Back to the graph of orders per site (ditching faceting by "height")
orders_plot <- ggplot(dat.it.h.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Abundance of orders per site (no bog, no moths)", x = "Orders", y = "Abundance") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Orders_comparison_WITH_BOG_WITHOUT_MOTHS.png")


###########################################################################################################################################################################################################################################################################

###########################################################################################################################################################################################################################################################################
###########################################################################################################################################################################################################################################################################
rm(list=ls())
###WITH MOTH DATA###

##########
###NOTE###
##########

#Including moth data involves ditching "top", "middle", "bottom", and "bog" classification.

north_ti <- read_excel("data/Arran_data1.xlsx", sheet = "North side invert transects")
south_ti <- read_excel("data/Arran_data1.xlsx", sheet = "South side invert transects")
moth_ti <- read_excel("data/Arran_data1.xlsx", sheet = "N+S Moth traps")
ti.raw.data <- rbind(north_ti,south_ti,moth_ti) # combining site A and site B data into a single data frame for easier analysis
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

#We're gonna perform analysis for Order level, so we'll erase rows with NAs for rows:
orderNA <- c(which(is.na(ti.raw.data$order))) ##which species have NA’s
ti.data <- ti.raw.data[-orderNA,]
View(ti.data)

# To compute community patterns, we need site by species data frames for each study site
# In this case, we want to compare diversity not only between study sites but also between "heights" (Top, Middle, Bottom), so we'll consider those as "site" in our site by species matrix

##SiteA##
dat.red <- ti.data[,c("order","site","individualCount")]
dat.redagg <- dat.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.ti.ab <- dat.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.ti.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.ti.ab) # we assign the species names to this list of 0s
dat.ti.ab <- as.data.frame(dat.ti.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.ti.ab) <- dat.ti.ab$site # here we give to each row the name of its corresponding island
dat.ti.ab <- dat.ti.ab[,-1] # we remove the island column because we don't need it anymore, as we have given island names to all rows

# Computing Shannon's and Simpson's diversity indices:
shannon.alpha.dat <- diversity(dat.ti.ab, index = "shannon") 
simpson.alpha.dat <- diversity(dat.ti.ab, index = "simpson") 

# Converting abundance data to presence/absence data in order to calculate richness and Soerensen and Jaccard indices:
dat.ti.ab <- ifelse(dat.ti.ab > 0, 1, 0) 

#Computing order richness
richness <- rowSums(dat.ti.ab)

richness_ti <- data.frame(
  Site = c("A", "B"),
  Richness = c(8, 9)
)

# Gamma Diversity for site A
#combined.dat.A.ab <- colSums(dat.A.ab) 
#gamma.richness.dat.A <- specnumber(combined.dat.A.ab)

###########
###Plots###
###########

orders_plot <- ggplot(dat.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Abundance of orders per site (with moths, no bog)", x = "Orders", y = "Abundance") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Orders_comparison_WITHOUT_BOG_WITH_MOTHS.png")

# Graphic representation of indices

# Including these in a data frame (WITHOUT BOG): might be wrong,
alpha_div <- data.frame(
  Site = rep(c("A", "B"), 2),
  Index = rep(c("Shannon", "Simpson"), each = 2),
  Value = c(shannon.alpha.dat[1], shannon.alpha.dat[2], simpson.alpha.dat[1], simpson.alpha.dat[2]) # Replace with actual results
)

# preparing a data frame for Shannon index
shannon <- alpha_div[which(alpha_div$Index=="Shannon"),]

# Plotting the differences in Shannon index by site and height:
shannon_plot <- ggplot(shannon, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Shannon Index Comparison", x = "Site", y = "Diversity Index") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Shannon_indices_WITH_BOG_WITH_MOTHS.png")
cvd_grid(shannon_plot) # checking that the chosen colour palette is suitable for colourblindness

# preparing a data frame for Simpson index
simpson <- alpha_div[which(alpha_div$Index=="Simpson"),]
# Plotting the differences in Simpson index by site and height:
simpson_plot <- ggplot(simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Simpson Index Comparison", x = "Site", y = "Diversity Index") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Simpson_indices_WITH_BOG_WITH_MOTH.png")
cvd_grid(simpson_plot) # checking that the chosen colour palette is suitable for colourblindness (although we're using the same colours and we've seen it's colourblindness safe)

###########################################################################################################################################################################################################################################################################
###########################################################################################################################################################################################################################################################################
rm(list=ls())
###WITH BOG AND MOTHS###

north_ti <- read_excel("data/Arran_data1.xlsx", sheet = "North side invert transects")
south_ti <- read_excel("data/Arran_data1.xlsx", sheet = "South side invert transects")
moth_ti <- read_excel("data/Arran_data1.xlsx", sheet = "N+S Moth traps")
bog <- read_excel("data/Arran_data1.xlsx", sheet = "BOG")
bog_ti <- bog[which(bog$samplingProtocol=="Sweep netting"),]
ti.raw.data <- rbind(north_ti,south_ti,moth_ti,bog_ti) # combining site A and site B data into a single data frame for easier analysis
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

#We're gonna perform analysis for Order level, so we'll erase rows with NAs for rows:
orderNA <- c(which(is.na(ti.raw.data$order))) ##which species have NA’s
ti.data <- ti.raw.data[-orderNA,]
View(ti.data)

# To compute community patterns, we need site by species data frames for each study site
# In this case, we want to compare diversity not only between study sites but also between "heights" (Top, Middle, Bottom), so we'll consider those as "site" in our site by species matrix


dat.red <- ti.data[,c("order","site","individualCount")]
dat.redagg <- dat.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
dat.ti.ab <- dat.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=order,values_from=c(total_individuals)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(dat.ti.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(dat.ti.ab) # we assign the species names to this list of 0s
dat.ti.ab <- as.data.frame(dat.ti.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(dat.ti.ab) <- dat.ti.ab$site # here we give to each row the name of its corresponding island
dat.ti.ab <- dat.ti.ab[,-1] # we remove the island column because we don't need it anymore, as we have given island names to all rows

# Computing Shannon's and Simpson's diversity indices:
shannon.alpha.dat <- diversity(dat.ti.ab, index = "shannon") 
simpson.alpha.dat <- diversity(dat.ti.ab, index = "simpson") 

# Converting abundance data to presence/absence data in order to calculate richness and Soerensen and Jaccard indices:
dat.ti.ab <- ifelse(dat.ti.ab > 0, 1, 0) 

#Computing order richness
richness <- rowSums(dat.ti.ab)
ncol(dat.ti.ab) # total number of orders detected

richness_ti <- data.frame(
  Site = c("A", "B"),
  Richness = c(8, 9)
)

# Gamma Diversity for site A
#combined.dat.A.ab <- colSums(dat.A.ab) 
#gamma.richness.dat.A <- specnumber(combined.dat.A.ab)

###########
###Plots###
###########

orders_plot <- ggplot(dat.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Abundance of orders per site (with moths, with bog)", x = "Orders", y = "Abundance") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Orders_comparison_WITH_BOG_WITH_MOTHS.png")

# Graphic representation of indices

# Including these in a data frame (WITHOUT BOG): might be wrong,
alpha_div <- data.frame(
  Site = rep(c("A", "B"), 2),
  Index = rep(c("Shannon", "Simpson"), each = 2),
  Value = c(shannon.alpha.dat[1], shannon.alpha.dat[2], simpson.alpha.dat[1], simpson.alpha.dat[2]) # Replace with actual results
)

# preparing a data frame for Shannon index
shannon <- alpha_div[which(alpha_div$Index=="Shannon"),]

# Plotting the differences in Shannon index by site and height:
shannon_plot <- ggplot(shannon, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Shannon Index Comparison", x = "Site", y = "Diversity Index") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Shannon_indices_WITH_BOG_WITH_MOTHS.png")
cvd_grid(shannon_plot) # checking that the chosen colour palette is suitable for colourblindness

# preparing a data frame for Simpson index
simpson <- alpha_div[which(alpha_div$Index=="Simpson"),]
# Plotting the differences in Simpson index by site and height:
simpson_plot <- ggplot(simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  labs(title = "Simpson Index Comparison", x = "Site", y = "Diversity Index") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12))
ggsave("figures/terrestrial_invertebrates/Simpson_indices_WITH_BOG_WITH_MOTH.png")
cvd_grid(simpson_plot) # checking that the chosen colour palette is suitable for colourblindness (although we're using the same colours and we've seen it's colourblindness safe)
