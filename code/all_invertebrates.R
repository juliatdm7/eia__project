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

i.data.red <- i.data[,c("order","site","individualCount")]
i.data.red$individualCount <- as.numeric(i.data.red$individualCount)
i.data.red.redagg <- i.data.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")

all_inverts <- ggplot(i.data.red.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Invertebrates diversity between sites", x = "Invertebrates orders", y = "Abundance") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # Facet label size (if you have facets)
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/all_invertebrates/Inverts_all.png")  
