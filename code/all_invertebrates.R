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
#Please run the file all_groups.R before running this one#
all.inverts <- raw.data.all[1:235,] # Loading all invertebrate data

orderNA <- c(which(is.na(all.inverts$order))) ##which rows have NA’s
i.data <- all.inverts[-orderNA,]

i.data.red <- i.data[,c("order","site","individualCount")]
i.data.red$individualCount <- as.numeric(i.data.red$individualCount)

# Rearranging data to create site by class dataframes
i.data.red.redagg <- i.data.red %>%
  group_by(order, site) %>%
  summarise(total_individuals = sum(individualCount), .groups = "drop")
i.data.ab <- i.data.red.redagg %>%  
  pivot_wider(names_from=order,values_from=c(total_individuals)) 
list0 <- as.list(rep(0,ncol(i.data.ab))) 
names(list0) <- names(i.data.ab) 
i.data.ab <- as.data.frame(i.data.ab %>% replace_na(list0))
row.names(i.data.ab) <- i.data.ab$site 
i.data.ab <- i.data.ab[,-1]
ncol(i.data.ab)

# To compute order richness is better to work with presence/absence data.
# We transform our site-by-order matrix (data frame) into presence/absence:
i.data.pa <- ifelse(i.data.ab > 0, 1, 0)
richness_A <- rowSums(i.data.pa)[1] # Order richness in site A is 13
richness_B <- rowSums(i.data.pa)[2] # Order richness in site B is 9
which((i.data.pa)[1,] != 0)
which((i.data.pa)[2,] != 0)

#Bar plot for orders comparison between sites
all_inverts <- ggplot(i.data.red.redagg, aes(x = order, y = total_individuals, fill = site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = total_individuals), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Invertebrates diversity between sites", x = "Invertebrates orders", y = "Abundance") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.text = element_text(size = 12))
ggsave("figures/all_invertebrates/Inverts_all.png")  

