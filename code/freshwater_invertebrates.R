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
  summarise(total_individuals = sum(individualCount), .groups = "drop") 
fi.dat.ab <- fi.dat.redagg %>%  
  pivot_wider(names_from=order,values_from=c(total_individuals)) 
list0 <- as.list(rep(0,ncol(fi.dat.ab))) 
names(list0) <- names(fi.dat.ab) 
fi.dat.ab <- as.data.frame(fi.dat.ab %>% replace_na(list0)) 
row.names(fi.dat.ab) <- fi.dat.ab$site 
fi.dat.ab <- fi.dat.ab[,-1] 

ncol(fi.dat.ab) # Total number of orders sampled

# Computing Shannon's and Simpson's diversity indices:
shannon.alpha.fi.dat <- diversity(fi.dat.ab, index = "shannon")
simpson.alpha.fi.dat <- diversity(fi.dat.ab, index = "simpson")

# To compute order richness is better to work with presence/absence data.
# We transform our site-by-order matrix (data frame) into presence/absence:
fi.dat.pa <- ifelse(fi.dat.ab > 0, 1, 0)
richness_A <- rowSums(fi.dat.pa)[1] # Order richness in site A is 13
richness_B <- rowSums(fi.dat.pa)[2] # Order richness in site B is 9

# Building a data frame including all three metrics:
metrics <- data.frame(
  Site = rep(c("A", "B"), 3),
  Metric = rep(c("Shannon", "Simpson", "Richness"), each = 2),
  Value = c(shannon.alpha.fi.dat[1], shannon.alpha.fi.dat[2], simpson.alpha.fi.dat[1], simpson.alpha.fi.dat[2], richness_A, richness_B)
)

# Plotting all three metrics
metrics_plot <- ggplot(metrics, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Metrics comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Metric) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    strip.text = element_text(size = 12))
ggsave("figures/freshwater_invertebrates/All_metrics.png")
cvd_grid(shannon_plot)

# Maybe Richness plot is making it difficult to see differences in Shannon's and Simpson's indices.
shannon_simpson <- metrics[metrics$Metric != "Richness", ]
metrics_plot <- ggplot(shannon_simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Metrics comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Metric) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  # Axis text size
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/freshwater_invertebrates/Shannon_Simpson_Only.png")
cvd_grid(shannon_plot)

# Richness plot
richness_fi <- metrics[metrics$Metric == "Richness", ]
richness_plot_fi <- ggplot(richness_fi, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = Value), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Freshwater invertebrates richness", x = "Site", y = "Order richness") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  # Axis text size
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/freshwater_invertebrates/Freshwater_Invertebrates_richness.png")

################################################
###Freshwater Invertebrates Analysis With Bog###
################################################

fi.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "N+S Stream inverts")
bog <- read_excel("data/Arran_data1.xlsx", sheet = "BOG")
fi.bog <- bog[which(bog$samplingProtocol=="Kick net sampling"),]
orderNA <- c(which(is.na(fi.bog$order))) ## no rows have NA’s for Order
dat.fi <- rbind(fi.raw.data, fi.bog)

for (i in 1:nrow(dat.fi)) {
  if (dat.fi[i, "site"] == "North") {
    dat.fi[i, "site"] <- "A"
  } else if (dat.fi[i, "site"] == "South") {
    dat.fi[i, "site"] <- "B"
  }
} # Modyfing data frame so "North" site is considered site A and "South" site is considered site B

#We're gonna perform analysis for Order level, so we'll erase rows with NAs for rows:
orderNA <- c(which(is.na(dat.fi$order))) ##which species have NA’s
fi.data <- dat.fi[-orderNA,]

# Creating one dataframe for each site
dat.A <- fi.data[which(fi.data$site=="A"),]
dat.B <- fi.data[which(fi.data$site=="B"),]


fi.dat.red <- fi.data[,c("order","site","individualCount")]
unique(fi.dat.red$order) # We can see that across both sites, 15 Orders were identified
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

# Computing Shannon's and Simpson's diversity indices:
shannon.alpha.fi.dat <- diversity(fi.dat.ab, index = "shannon")
simpson.alpha.fi.dat <- diversity(fi.dat.ab, index = "simpson")

# To compute order richness is better to work with presence/absence data.
# We transform our site-by-order matrix (data frame) into presence/absence:
fi.dat.pa <- ifelse(fi.dat.ab > 0, 1, 0)
richness_A <- rowSums(fi.dat.pa)[2] # Order richness in site A is 13
richness_B <- rowSums(fi.dat.pa)[1] # Order richness in site B is 12 when we add the bog data!

# Building a data frame including all three metrics:
metrics <- data.frame(
  Site = rep(c("A", "B"), 3),
  Metric = rep(c("Shannon", "Simpson", "Richness"), each = 2),
  Value = c(shannon.alpha.fi.dat[1], shannon.alpha.fi.dat[2], simpson.alpha.fi.dat[1], simpson.alpha.fi.dat[2], richness_A, richness_B)
)

# Plotting all three metrics
metrics_plot <- ggplot(metrics, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Metrics comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Metric) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    strip.text = element_text(size = 12))
ggsave("figures/freshwater_invertebrates/All_metrics_WITH_BOG.png")
cvd_grid(shannon_plot)

# Maybe Richness plot is making it difficult to see differences in Shannon's and Simpson's indices.
shannon_simpson <- metrics[metrics$Metric != "Richness", ]
metrics_plot <- ggplot(shannon_simpson, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(title = "Metrics comparison", x = "Site", y = "Diversity Index") +
  facet_wrap(~Metric) +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  # Axis text size
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/freshwater_invertebrates/Shannon_Simpson_Only_WITH_BOG.png")
cvd_grid(shannon_plot)

# Richness plot
richness_fi <- metrics[metrics$Metric == "Richness", ]
richness_plot_fi <- ggplot(richness_fi, aes(x = Site, y = Value, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = Value), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Freshwater invertebrates richness", x = "Site", y = "Order richness") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  # Main title size
    axis.title = element_text(size = 14),  # Axis titles size
    axis.text = element_text(size = 12),  # Axis text size
    strip.text = element_text(size = 12)   # Facet label size (if you have facets)
  )
ggsave("figures/freshwater_invertebrates/Freshwater_Invertebrates_richness_WITH_BOG.png")

fi_orders <- ggplot(fi.dat.redagg, aes(x = order, y = total_individuals, fill = site)) +
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
ggsave("figures/freshwater_invertebrates/Freshwater_Invertebrates_orders_WITH_BOG.png")  
