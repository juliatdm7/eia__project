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

unique(vert1.dat$class) # In the excel sheet there are recordings of amphibians, reptiles, birds and mammals
length(which(vert1.dat$class!="Aves")) # However, only 3 recordings (including both study sites) are not of bird species

# Therefore, we'll focus on birds for now:
bird.dat <- vert1.dat[vert1.dat$class == "Aves", ]

# To compute community patterns, we need site by species data frames for each study site:
#bird.dat <- bird.dat.A[!duplicated(bird.dat.A$scientificName), ]
bird.dat.red.prov <- bird.dat[,c("scientificName","site")]
bird.dat.red <- unique(bird.dat.red.prov)
bird.dat.red$presence <- 1
bird.dat.pa <- bird.dat.red %>%  
  pivot_wider(names_from=scientificName,values_from=c(presence)) 
list0 <- as.list(rep(0,ncol(bird.dat.pa)))
names(list0) <- names(bird.dat.pa)
bird.dat.pa <- as.data.frame(bird.dat.pa %>% replace_na(list0))
row.names(bird.dat.pa) <- bird.dat.pa$site
bird.dat.pa <- bird.dat.pa[,-1]
ncol(bird.dat.pa) # The total number of species identified across sites is 21

#Calculating species richness between sites

richness_bird_A <- rowSums(bird.dat.pa)[2] # 19 out of 21 species found in site A
richness_bird_B <- rowSums(bird.dat.pa)[1] # 12 out of 21 species found in site B

richness_bird <- data.frame(
  Site = c("A", "B"),
  Richness = c(richness_bird_B, richness_bird_A)
)

richness_bird_plot <- ggplot(richness_bird, aes(x = Site, y = Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = Richness), vjust = -0.5, size = 5) +
  labs(title = "Species richness", x = "Site", y = "Nr of Species") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 20), size = 16),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    strip.text = element_text(size = 12))
ggsave("figures/vertebrates_birds/Birds_richness.png")
cvd_grid(richness_bird_plot)

# Maybe I should consider differentiating between top, middle, and bottom?

#Option  1 of sections
bird.dat$new_locationID <- "0"
bird.dat[1:8,32] <- "Bog"
bird.dat[9:15,32] <- "Middle"
bird.dat[16:19,32] <- "Top"
bird.dat[20:31,32] <- "Bottom"
bird.dat[32:38,32] <- "Middle"
bird.dat[c(39,40),32] <- "Top"
