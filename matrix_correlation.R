# Load libraries
library(dplyr)
library(corrplot)
library(ggplot2)

# Load data
data <- read.csv("D:/Files/Side_Project/preprocess_data/merged_dataset.csv", stringsAsFactors = FALSE)

# Preprocess numerical/ordinal variables
data_processed <- data %>%
  mutate(
    # Convert age to numerical (midpoint of ranges)
    Age = case_when(
      `What_is_your_age` == "Under 20" ~ 18,
      `What_is_your_age` == "20-25" ~ 22.5,
      `What_is_your_age` == "26-30" ~ 28,
      `What_is_your_age` == "Over 30" ~ 35,
      TRUE ~ NA_real_
    ),
    
    # Convert time spent to numerical (hours)
    Time_Spent = case_when(
      `How_much_time_do_you_typically_spend_on_social_media_daily` == "Less than 1 hour" ~ 0.5,
      `How_much_time_do_you_typically_spend_on_social_media_daily` == "1-2 hours" ~ 1.5,
      `How_much_time_do_you_typically_spend_on_social_media_daily` == "2-3 hours" ~ 2.5,
      `How_much_time_do_you_typically_spend_on_social_media_daily` == "3-5 hours" ~ 4,
      `How_much_time_do_you_typically_spend_on_social_media_daily` == "More than 5 hours" ~ 5.5,
      TRUE ~ NA_real_
    ),
    
    # Convert ordinal scales (example: perceived addiction)
    Addiction_Score = case_when(
      `Do_you_think_you_are_addicted_to_social_media` == "Strongly agree" ~ 5,
      `Do_you_think_you_are_addicted_to_social_media` == "Somewhat agree" ~ 4,
      `Do_you_think_you_are_addicted_to_social_media` == "Neither agree nor disagree" ~ 3,
      `Do_you_think_you_are_addicted_to_social_media` == "Somewhat disagree" ~ 2,
      `Do_you_think_you_are_addicted_to_social_media` == "Strongly disagree" ~ 1,
      TRUE ~ NA_real_
    )
  )

# Select numerical/ordinal variables for correlation
numeric_vars <- data_processed %>%
  select(
    Age,
    Time_Spent,
    Addiction_Score,
    # Add other convertible variables here
  )

# Remove rows with missing values
numeric_vars <- na.omit(numeric_vars)

# Calculate correlation matrix (Pearson/Spearman)
cor_matrix <- cor(numeric_vars, method = "spearman")

# Visualize correlations
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black")

# Optional: Heatmap for significant correlations
ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal()

# Extract significant correlations (p-values)
library(Hmisc)
cor_test <- rcorr(as.matrix(numeric_vars), type = "spearman")

# Print significant correlations (p < 0.05)
print(cor_test$r)  # Correlation coefficients
print(cor_test$P)  # P-values
