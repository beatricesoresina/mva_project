# Load required libraries
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

# Set working directory (adjust if needed)
setwd("/Users/beatricesoresina/Desktop/mva_project")

# Load your dataset
state_data <- read_csv("state_level_beauty_wellness_econ_means.csv", show_col_types = FALSE)

# Define variable groups
beauty_vars <- c("beauty_rev", "beauty_est", "beauty_ent", "beauty_emp", "beauty_wag")
wellness_vars <- c("wellness_rev", "wellness_est", "wellness_ent", "wellness_emp", "wellness_wag")
econ_vars <- c("r_gdp", "r_persinc", "r_pce", "disp_persinc", "rpc_persinc", "rpc_pce")

# Create all elasticity combinations
for (b in c(beauty_vars, wellness_vars)) {
  for (e in econ_vars) {
    state_data[[paste0(b, "_", e, "_elasticity")]] <- state_data[[b]] / state_data[[e]]
  }
}
# Summarize average elasticities across states
elasticity_summary <- state_data %>%
  summarise(across(ends_with("_elasticity"), ~ mean(.x, na.rm = TRUE)))

# Save to CSV
write_csv(elasticity_summary, "elasticity_summary_state_averages.csv")
# Melt to long format
elasticity_long <- melt(elasticity_summary)

# Add Sector column
elasticity_long <- elasticity_long %>%
  mutate(
    Sector = case_when(
      grepl("^beauty", variable) ~ "Beauty",
      grepl("^wellness", variable) ~ "Wellness",
      TRUE ~ "Other"
    )
  )
# Plot Beauty Heatmap
ggplot(filter(elasticity_long, Sector == "Beauty"), aes(x = variable, y = "Elasticity")) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Elasticities of Beauty Variables to Economic Indicators",
       x = "Beauty Elasticity Metric",
       y = "",
       fill = "Elasticity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Wellness Heatmap
ggplot(filter(elasticity_long, Sector == "Wellness"), aes(x = variable, y = "Elasticity")) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Elasticities of Wellness Variables to Economic Indicators",
       x = "Wellness Elasticity Metric",
       y = "",
       fill = "Elasticity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save melted (long-format) elasticity data
write_csv(elasticity_long, "elasticity_summary_long_state_averages.csv")

