rm(list=ls())

###### Use the right 
setwd("C:/Users/u0175011/OneDrive - KU Leuven/PhD jaar 1/summer of science/practical")

install.packages("vegan")
install.packages("tidyverse")
###### Load libraries ######
library(vegan)
library(tidyverse)

###### Load your data ######
# Example: read CSV generated earlier
data <- read.csv("ButterflyForestvsGrass.csv")

###### Prepare community matrix ######
comm_matrix <- data %>%
  pivot_wider(names_from = Species, values_from = Count, values_fill = 0) %>%
  column_to_rownames("Site_ID") %>%
  select(-Habitat)

habitat <- data %>%
  distinct(Site_ID, Habitat) %>%
  arrange(match(Site_ID, rownames(comm_matrix)))

###### Calculate Bray-Curtis distance and PCoA ######
bray_dist <- vegdist(comm_matrix, method = "bray")
pcoa_result <- cmdscale(bray_dist, k = 2, eig = TRUE)

pcoa_df <- as.data.frame(pcoa_result$points)
colnames(pcoa_df) <- c("PCoA1", "PCoA2")
pcoa_df$Site_ID <- rownames(pcoa_df)
pcoa_df$Habitat <- habitat$Habitat

###### Plot with ggplot2 ######
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "PCoA of Butterfly Communities (Bray-Curtis)",
       x = "PCoA Axis 1", y = "PCoA Axis 2")

ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = Habitat)) +
  geom_point(size = 4) +
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = Habitat), color = NA) +
  scale_color_manual(values = c("Forest" = "#1b9e77", "Grassland" = "#d95f02")) +
  scale_fill_manual(values = c("Forest" = "#1b9e77", "Grassland" = "#d95f02")) +
  theme_minimal() +
  labs(title = "PCoA of Butterfly Communities (Bray-Curtis)",
       x = "PCoA Axis 1", y = "PCoA Axis 2")


########################################
###### PCA Biplot with Species Arrows #
########################################

# PCA via rda()
pca <- rda(comm_matrix, scale = TRUE)

# Scores
site_scores <- scores(pca, display = "sites") %>% as.data.frame()
species_scores <- scores(pca, display = "species") %>% as.data.frame()
site_scores$Site_ID <- rownames(site_scores)
site_scores <- left_join(site_scores, habitat, by = "Site_ID")

# Plot
ggplot() +
  geom_point(data = site_scores, aes(x = PC1, y = PC2, color = Habitat), size = 4) +
  geom_segment(data = species_scores,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
  geom_text(data = species_scores,
            aes(x = PC1, y = PC2, label = rownames(species_scores)),
            size = 3, hjust = 0.5, vjust = -0.5) +
  scale_color_manual(values = c("Forest" = "#1b9e77", "Grassland" = "#d95f02")) +
  coord_equal() +
  theme_minimal() +
  labs(title = "PCA Biplot of Butterfly Communities",
       x = "PC1", y = "PC2")

