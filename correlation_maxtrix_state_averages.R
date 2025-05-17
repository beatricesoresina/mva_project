#Correlation Matrix per state average
# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

# 1. Set your working directory
setwd("/Users/beatricesoresina/Desktop/mva_project")

# 2. Load the dataset
state_averages <- read_csv("state_level_beauty_wellness_econ_means.csv", show_col_types = FALSE)

# 3. Remove the 'state' column (non-numeric)
state_numeric <- state_averages %>% select(-state)

# 4. Compute correlation matrix
cor_matrix <- cor(state_numeric, use = "complete.obs")

# 5. View the correlation matrix
print(round(cor_matrix, 2))

# 6. Plot correlation heatmap
corrplot(cor_matrix, method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.srt = 45,
         number.cex = 0.7,
         mar = c(0, 0, 1, 0),
         title = "Correlation Matrix: State-level Averages")

# 7. Save correlation matrix to CSV
write.csv(cor_matrix, "correlation_matrix_state_averages.csv", row.names = TRUE)
cat("✅ Correlation matrix saved as 'correlation_matrix_state_averages.csv'\n")
