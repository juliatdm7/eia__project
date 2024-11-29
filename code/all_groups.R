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
#All groups analysis#
#Run this file between running all_invertebrates.R

fi.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "N+S Stream inverts")
A.ti.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "North side invert transects")
B.ti.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "South side invert transects")
ti.raw.data <- rbind(A.ti.raw.data,B.ti.raw.data)
bog.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "BOG")
bog.raw.data <- bog.raw.data[1:22,]
vert1.raw.dat <- read_excel("data/Arran_data1.xlsx", sheet = "Vertebrates")
vert2.raw.dat <- read_excel("data/Arran_data1.xlsx", sheet = "Vertebrates (tech)")
moth.raw.data <- read_excel("data/Arran_data1.xlsx", sheet = "N+S Moth traps")

str(fi.raw.data)
str(ti.raw.data)
str(bog.raw.data)
str(moth.raw.data)
str(vert1.raw.dat)
str(vert2.raw.dat)


fi.raw.data$eventDate <- as.character(fi.raw.data$eventDate)
fi.raw.data$eventTime <- as.character(fi.raw.data$eventTime)

ti.raw.data$eventDate <- as.character(ti.raw.data$eventDate)
ti.raw.data$eventTime <- as.character(ti.raw.data$eventTime)

bog.raw.data$eventDate <- as.character(bog.raw.data$eventDate)
bog.raw.data$eventTime <- as.character(bog.raw.data$eventTime)

moth.raw.data$eventDate <- as.character(moth.raw.data$eventDate)
moth.raw.data$eventTime <- as.character(moth.raw.data$eventTime)

vert1.raw.dat$eventDate <- as.character(vert1.raw.dat$eventDate)
vert1.raw.dat$eventTime <- as.character(vert1.raw.dat$eventTime)

vert2.raw.dat$eventDate <- as.character(vert2.raw.dat$eventDate)
vert2.raw.dat$eventTime <- as.character(vert2.raw.dat$eventTime)

raw.data.all <- rbind(fi.raw.data,ti.raw.data,bog.raw.data,moth.raw.data,vert1.raw.dat,vert2.raw.dat)

for (i in 1:nrow(raw.data.all)) {
  if (raw.data.all[i, "site"] == "North") {
    raw.data.all[i, "site"] <- "A"
  } else if (raw.data.all[i, "site"] == "South") {
    raw.data.all[i, "site"] <- "B"
  }
}

class_counts <- raw.data.all[,c("class","site","occurrenceStatus")]

orderNA <- c(which(is.na(class_counts$class))) ##which species have NA’s
class.data <- class_counts[-orderNA,]

class.data[170,1] <- "Insecta"
class.data[,3] <- 1

class.data.aggreg <- class.data %>%
  group_by(class, site) %>%
  summarise(presence = 1, .groups = "drop")
class.dat.pa <- class.data.aggreg %>%  
  pivot_wider(names_from=class,values_from=c(presence)) 
list0 <- as.list(rep(0,ncol(class.dat.pa))) 
names(list0) <- names(class.dat.pa) 
class.dat.pa <- as.data.frame(class.dat.pa %>% replace_na(list0))
row.names(class.dat.pa) <- class.dat.pa$site 
class.dat.pa <- class.dat.pa[,-1]

classes_richness_A <- rowSums(class.dat.pa)[2]
classes_richness_B <- rowSums(class.dat.pa)[1]

A <- class.data.aggreg[which(class.data.aggreg[,2]=="A"),]
B <- class.data.aggreg[which(class.data.aggreg[,2]=="B"),]


classes <- data.frame(
  Site = c("A", "B"),
  Classes = c(nrow(A),nrow(B))
)

all.dat.class <- ggplot(classes, aes(x = Site, y = Classes, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", width = 0.5) +
  geom_text(aes(label = Classes), vjust = -0.5, size = 5) +
  labs(title = "Number of classes between sites", x = "Site", y = "Nr of classes") +
  scale_fill_manual(values = c("A" = "#0073e6", "B" = "#f194b8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 5, b = 40), size = 16),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    strip.text = element_text(size = 12))
ggsave("figures/all_groups/Classes_all.png")
cvd_grid(all.dat.class) # Making sure colours are colourblind friendly
