library(dplyr)
library(readr)

install.packages("reshape2")  # if you haven't already
library(reshape2)

# Set working directory
setwd("/Users/beatricesoresina/Desktop/mva_project")

# Load your full panel dataset
combined_beauty_wellness_econ <- read_csv("combined_beauty_wellness_econ.csv", show_col_types = FALSE)

# Collapse to mean values from 2008–2023 per state
state_averages <- combined_beauty_wellness_econ %>%
  filter(year >= 2008, year <= 2023) %>%
  group_by(state) %>%
  summarise(across(
    c(r_gdp, r_persinc, r_pce, disp_persinc, rpc_persinc, rpc_pce,
      beauty_rev, beauty_est, beauty_ent, beauty_emp, beauty_wag,
      wellness_rev, wellness_est, wellness_ent, wellness_emp, wellness_wag),
    ~ mean(.x, na.rm = TRUE)
  ))

# Preview
print(state_averages)

# Save to CSV
write_csv(state_averages, "state_level_beauty_wellness_econ_means.csv")

cat("✅ Saved: 50-state averages to 'state_level_beauty_wellness_econ_means.csv'\n")

library(readr)
library(dplyr)
install.packages(car)
library(car)     # for VIF
library(ggplot2)

# Set working directory
setwd("/Users/beatricesoresina/Desktop/mva_project")

# Load the state-averaged dataset
state_averages <- read_csv("state_level_beauty_wellness_econ_means.csv", show_col_types = FALSE)

# Beauty Revenue ~ Macro Indicators
beauty_avg_model <- lm(beauty_rev ~ r_gdp + r_persinc + r_pce + disp_persinc + rpc_persinc + rpc_pce, data = state_averages)
summary(beauty_avg_model)

# Wellness Revenue ~ Macro Indicators
wellness_avg_model <- lm(wellness_rev ~ r_gdp + r_persinc + r_pce + disp_persinc + rpc_persinc + rpc_pce, data = state_averages)
summary(wellness_avg_model)

# VIF (Variance Inflation Factor)
vif(beauty_avg_model)
vif(wellness_avg_model)

# GDP vs Beauty Revenue
ggplot(state_averages, aes(x = r_gdp, y = beauty_rev, label = state)) +
  geom_point(color = "blue", size = 3) +
  geom_text(nudge_y = 500, size = 3) +
  labs(
    title = "Beauty Revenue vs GDP (State Averages)",
    x = "Average Real GDP (2008–2023)",
    y = "Average Beauty Revenue"
  ) +
  theme_minimal()

# Define outcome and predictor variables
outcomes <- c("beauty_rev", "beauty_est", "beauty_ent", "beauty_emp", "beauty_wag",
              "wellness_rev", "wellness_est", "wellness_ent", "wellness_emp", "wellness_wag")
predictors <- c("r_gdp", "r_persinc", "r_pce", "disp_persinc", "rpc_persinc", "rpc_pce")

# Initialize dataframe for storing results
regression_results <- data.frame()

# Loop through each dependent variable
for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = state_averages)
  summary_model <- summary(model)
  coefs <- summary_model$coefficients[predictors, "Estimate"]
  
  temp <- data.frame(
    Dependent = outcome,
    Predictor = predictors,
    Coefficient = coefs
  )
  
  regression_results <- rbind(regression_results, temp)
}

# Define outcome and predictor variables
outcomes <- c("beauty_rev", "beauty_est", "beauty_ent", "beauty_emp", "beauty_wag",
              "wellness_rev", "wellness_est", "wellness_ent", "wellness_emp", "wellness_wag")
predictors <- c("r_gdp", "r_persinc", "r_pce", "disp_persinc", "rpc_persinc", "rpc_pce")

# Initialize dataframe for storing results
regression_results <- data.frame()

# Loop through each DEPENDENT variable
for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = state_averages)
  summary_model <- summary(model)
  coefs <- summary_model$coefficients[predictors, "Estimate"]
  
  temp <- data.frame(
    Dependent = outcome,
    Predictor = predictors,
    Coefficient = coefs
  )
  
  regression_results <- rbind(regression_results, temp)
}

# Reshape to wide format
# Reshape to wide format
coef_matrix <- dcast(regression_results, Dependent ~ Predictor, value.var = "Coefficient")
# Convert to matrix format for heatmap
rownames(coef_matrix) <- coef_matrix$Dependent
coef_matrix$Dependent <- NULL
coef_matrix <- as.matrix(coef_matrix)


#STAT DATA 
# New data frame to hold full regression stats
full_regression_results <- data.frame()

for (outcome in outcomes) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = state_averages)
  model_summary <- summary(model)
  
  # Extract coefficients and p-values
  coef_df <- as.data.frame(model_summary$coefficients[predictors, ])
  coef_df$Predictor <- rownames(coef_df)
  coef_df$Dependent <- outcome
  coef_df$R2 <- model_summary$r.squared
  coef_df$AdjR2 <- model_summary$adj.r.squared
  
  full_regression_results <- rbind(full_regression_results, coef_df)
}
print(full_regression_results)


# Filter just Estimate and p-value
sig_matrix_beauty <- full_regression_results %>%
  filter(grepl("beauty", Dependent)) %>%
  select(Dependent, Predictor, Estimate, `Pr(>|t|)`) %>%
  mutate(Significant = `Pr(>|t|)` < 0.05)

sig_matrix_wellness <- full_regression_results %>%
  filter(grepl("wellness", Dependent)) %>%
  select(Dependent, Predictor, Estimate, `Pr(>|t|)`) %>%
  mutate(Significant = `Pr(>|t|)` < 0.05)

# Beauty heatmap with stars for significance
ggplot(sig_matrix_beauty, aes(x = Predictor, y = Dependent, fill = Estimate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Significant, "*", "")), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(title = "Beauty Sector: Coefficients & Significance",
       fill = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sig_matrix_wellness, aes(x = Predictor, y = Dependent, fill = Estimate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Significant, "*", "")), color = "black", size = 5) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Wellness Sector: Regression Coefficients & Significance",
    x = "Macroeconomic Indicator",
    y = "Wellness Outcome Variable",
    fill = "Coefficient"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

r2_summary <- full_regression_results %>%
  group_by(Dependent) %>%
  summarise(R2 = unique(R2))

ggplot(r2_summary, aes(x = reorder(Dependent, -R2), y = R2)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(R2, 2)), vjust = -0.5) +
  labs(title = "Model Fit (R²) Across Dependent Variables",
       x = "Dependent Variable", y = "R²") +
  theme_minimal()

