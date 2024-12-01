################################################################################
############################Biodiversity Analysis###############################
################################################################################

library(readxl)
library(tidyverse)
library(vegan)
library(betapart)
library(dplyr)
library(colorblindr)
library(ggplot2)

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
  summarise(total_calls = sum(individualCount), .groups = "drop") 
bat.dat.ab <- bat.dat.redagg %>%  
  pivot_wider(names_from=vernacularName,values_from=c(total_calls)) 
list0 <- as.list(rep(0,ncol(bat.dat.ab)))
names(list0) <- names(bat.dat.ab) 
bat.dat.ab <- as.data.frame(bat.dat.ab %>% replace_na(list0)) 
row.names(bat.dat.ab) <- bat.dat.ab$site 
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
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = total_calls), position = position_dodge(width = 0.8), vjust = -0.5, size = 5) +
  labs(title = "Bat calls", x = "Bat taxon", y = "Number of calls") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  
    axis.text.x = element_text(hjust = 0.5, vjust = 1),
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

# Plotting richness
richness_plot <- ggplot(richness_bats, aes(x = Site, y = Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = Richness), vjust = -0.5, size = 5) +
  labs(title = "Richness comparison", x = "Site", y = "Nr of species/genus") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    strip.text = element_text(size = 12))
ggsave("figures/vertebrates_bats/Richnesss_bat.png")
cvd_grid(shannon_plot)

# Building a data frame only for indices:
shannon_simpson <- metrics[metrics$Metric != "Richness", ]

# Plotting Shannon's and Simpson's indices
shannon_simpson_plot <- ggplot(shannon_simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Diversity indices comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Metric) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  # Axis text size
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/vertebrates_bats/Shannon_simpson_bat.png")
cvd_grid(shannon_plot)

# Removing Common Pipistrelle calls to better compare number of calls across sites:
bat.dat.redagg2 <- bat.dat.redagg[which(bat.dat.redagg$vernacularName!="Common pipistrelle"),]
calls_bats <- ggplot(bat.dat.redagg2, aes(x = vernacularName, y = total_calls, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = total_calls), position = position_dodge(width = 0.5), vjust = -0.5, size = 5) +
  labs(title = "Bat calls", x = "Bat taxon", y = "Number of calls") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 20), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  
    axis.text.x = element_text(hjust = 0.5, vjust = 1),
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/vertebrates_bats/Nr_calls_bat_NoCommonPipistrelle.png")
