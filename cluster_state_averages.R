# AVERAGE PER STATE CLUSTERS

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(cluster)      # For silhouette score
library(factoextra)   # For dendrograms & cluster plots
library(reshape2)

# 1. Load the dataset
setwd("/Users/beatricesoresina/Desktop/mva_project/")
state_data <- read_csv("state_level_beauty_wellness_econ_means.csv", show_col_types = FALSE)

# 2. Drop the 'state' column for clustering input
clustering_input <- state_data %>% select(-state)

# 3. Scale the data
scaled_data <- scale(clustering_input)

# 4. Compute distance matrix
dist_matrix <- dist(scaled_data)

# 5. Hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# 6. Plot dendrogram
fviz_dend(hc,
          k = 4,  # Change this if you want to test other cluster sizes
          rect = TRUE,
          rect_fill = TRUE,
          k_colors = c("#00AFBB", "#E7B800", "#FC4E07", "#999999"),
          main = "Clusters of States Based on Beauty, Wellness, and Econ Indicators")

# 7. Cut tree into clusters
state_data$cluster <- cutree(hc, k = 4)


# 8. Visualize the cluster on a map
library(maps)
library(ggplot2)

state_data$region <- tolower(state.name[match(state_data$state, state.abb)])
states_map <- map_data("state")
map_clustered <- left_join(states_map, state_data, by = "region")

ggplot(map_clustered, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "State Clusters: Beauty, Wellness, and Economic Profiles")

state_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

write_csv(state_data, "clusters_state_averages.csv")



