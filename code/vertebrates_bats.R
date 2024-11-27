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

bat.dat.red <- bat.dat[,c("vernacularName","site","individualCount")]
unique(bat.dat.red$vernacularName) # Five bats species (or genus) are shared among sites
bat.dat.redagg <- bat.dat.red %>%
  group_by(vernacularName, site) %>%
  summarise(total_calls = sum(individualCount), .groups = "drop") # We aggregate difference samples of the same Orders
bat.dat.ab <- bat.dat.redagg %>%  #  here we're passing the dat.soc.red data frame into the next function using the operator pipe (%>%)
  pivot_wider(names_from=vernacularName,values_from=c(total_calls)) # the pivot_wider() function of tidyverse converts data from a long format to a wide format, creating a new dataframe in the values under "species" become columns, the "islands" become rows and the cells are filled with the corresponding "presence" values 
list0 <- as.list(rep(0,ncol(bat.dat.ab))) # here we create a list with as many 0s as there are columns in dat.soc.pa
names(list0) <- names(bat.dat.ab) # we assign the species names to this list of 0s
bat.dat.ab <- as.data.frame(bat.dat.ab %>% replace_na(list0)) # here we replace all NAs with 0s for all species in dat.soc.pa
row.names(bat.dat.ab) <- bat.dat.ab$site # here we give to each row the name of its corresponding island
bat.dat.ab <- bat.dat.ab[,-1]

# Computing Shannon's and Simpson's diversity indices:
shannon.alpha.bat.dat <- diversity(bat.dat.ab, index = "shannon")
simpson.alpha.bat.dat <- diversity(bat.dat.ab, index = "simpson")
#WARNING! By calculating Shannon's and Simpson's index we're assuming that each voice call accounts for an individual. Might me a wrong assumption.

# To compute order richness is better to work with presence/absence data.
# We transform our site-by-order matrix (data frame) into presence/absence:
bat.dat.pa <- ifelse(bat.dat.ab > 0, 1, 0)
richness_A <- rowSums(bat.dat.pa)[1] # Order richness in site A is 13
richness_B <- rowSums(bat.dat.pa)[2] # Order richness in site B is 9

# Plotting number of calls per bat taxon per site
calls_bats <- ggplot(bat.dat.redagg, aes(x = vernacularName, y = total_calls, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = total_calls), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  labs(title = "Bat calls", x = "Bat taxon", y = "Number of calls") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/vertebrates_bats/Nr_calls_bat.png") # Need to refer to Brown long-eared bat and Soprano pipistrelle being only in site A, cause colours are not quite distinguisable

# Building a data frame including all three metrics:
metrics <- data.frame(
  Site = rep(c("A", "B"), 3),
  Metric = rep(c("Shannon", "Simpson", "Richness"), each = 2),
  Value = c(shannon.alpha.bat.dat[1], shannon.alpha.bat.dat[2], simpson.alpha.bat.dat[1], simpson.alpha.bat.dat[2], richness_A, richness_B)
)

# Building a data frame only for richness:
richness_bats <- data.frame(
  Site = c("A", "B"),
  Richness = c(richness_A, richness_B)
)
