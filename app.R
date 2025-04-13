# Load libraries
library(tidyverse)  # Includes dplyr, ggplot2, tidyr
library(psych)      # For correlation tests

# Read data
data <- read_csv("socialmedia_v2.csv")

# Data cleaning and transformation --------------------------------------------
clean_data <- data %>%
  # Remove unnecessary columns
  select(-ID, -Start.time, -Completion.time, -Email, -Name, -Last.modified.time) %>%
  # Transform variables
  mutate(
    # Age to numerical groups
    AgeGroup = case_when(
      Q1 == "Under 20" ~ 1,
      Q1 == "20-25" ~ 2,
      Q1 == "26-30" ~ 3,
      Q1 == "Over 30" ~ 4,
      TRUE ~ NA_real_
    ),
    # Time spent to hourly midpoints
    TimeSpent = case_when(
      Q7 == "Less than 1 hour" ~ 0.5,
      Q7 == "1-2 hours" ~ 1.5,
      Q7 == "2-3 hours" ~ 2.5,
      Q7 == "3-5 hours" ~ 4,
      Q7 == "More than 5 hours" ~ 6,
      TRUE ~ NA_real_
    ),
    # Mental health impact to 1-5 scale
    MentalHealth = case_when(
      Q31 == "Strongly disagree" ~ 1,
      Q31 == "Somewhat disagree" ~ 2,
      Q31 == "Neither agree nor disagree" ~ 3,
      Q31 == "Somewhat agree" ~ 4,
      Q31 == "Strongly agree" ~ 5,
      TRUE ~ NA_real_
    ),
    # Anxiety frequency to 1-5 scale
    Anxiety = case_when(
      Q32 == "Never" ~ 1,
      Q32 == "Rarely" ~ 2,
      Q32 == "Sometimes" ~ 3,
      Q32 == "Often" ~ 4,
      Q32 == "Very often (daily)" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  # Select relevant columns and remove missing values
  select(AgeGroup, TimeSpent, MentalHealth, Anxiety) %>%
  drop_na()

# Correlation analysis -------------------------------------------------------
# Function to compute and print Spearman's correlation
run_cor_test <- function(x, y, label) {
  result <- cor.test(x, y, method = "spearman")
  cat("\nCorrelation:", label, "\n")
  cat("Spearman's rho =", round(result$estimate, 3), "\n")
  cat("p-value =", format.pval(result$p.value, digits = 3), "\n")
}

# Run correlations
run_cor_test(clean_data$AgeGroup, clean_data$TimeSpent, "Age vs Time Spent")
run_cor_test(clean_data$TimeSpent, clean_data$Anxiety, "Time Spent vs Anxiety")
run_cor_test(clean_data$TimeSpent, clean_data$MentalHealth, "Time Spent vs Mental Health Impact")

# Descriptive statistics -----------------------------------------------------
# Custom summary table
summary_stats <- clean_data %>%
  summarise(
    across(
      everything(),
      list(
        Mean = ~mean(., na.rm = TRUE) %>% round(2),
        SD = ~sd(., na.rm = TRUE) %>% round(2),
        Min = ~min(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      ),
      .names = "{col}_{fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = c("Variable", "Statistic"), names_sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = value)

# Print summary
cat("\nDescriptive Statistics:\n")
print(summary_stats, n = Inf)

# Visualizations -------------------------------------------------------------
# Theme for minimalist plots
minimal_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#333333"),
    axis.title = element_text(size = 12, color = "#555555"),
    axis.text = element_text(size = 10, color = "#666666"),
    panel.grid.major = element_line(color = "#E5E5E5", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F9F9F9", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

# Age group vs Time Spent (Boxplot)
ggplot(clean_data, aes(x = factor(AgeGroup, labels = c("<20", "20-25", "26-30", ">30")), y = TimeSpent)) +
  geom_boxplot(fill = "#A3BFFA", color = "#3C366B", width = 0.45) +
  labs(title = "Time Spent by Age Group", x = "Age Group", y = "Time Spent (hours/day)") +
  minimal_theme

# Time Spent vs Anxiety (Scatter with smooth curve)
ggplot(clean_data, aes(x = TimeSpent, y = Anxiety)) +
  geom_point(color = "#3C366B", alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", color = "#A3BFFA", fill = "#D6BCFA", alpha = 0.2) +
  labs(title = "Time Spent vs Anxiety", x = "Time Spent (hours/day)", y = "Anxiety (1-5)") +
  minimal_theme