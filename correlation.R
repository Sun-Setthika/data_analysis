# Load required libraries
library(dplyr)
library(readr)
library(corrplot)
library(ggplot2)

# Load and preprocess data
data <- read_csv("D:/Files/Side_Project/preprocess_data/merged_dataset.csv")
colnames(data) <- gsub("[ ?]", "_", colnames(data))

# Clean frequency column and enforce ordinality
data <- data %>%
  mutate(
    How_often_do_you_check_social_media = case_when(
      How_often_do_you_check_social_media == "Moins d'une heure" ~ "Less than 1 hour",
      TRUE ~ How_often_do_you_check_social_media
    ),
    Check_Frequency = factor(
      How_often_do_you_check_social_media,
      levels = c(
        "Less than 1 hour",          # Least frequent
        "Several times a day", 
        "Every hour",
        "Multiple times per hour"     # Most frequent
      ),
      ordered = TRUE
    )
  ) %>%
  filter(!is.na(Check_Frequency))  # Remove invalid entries

# Handle multilingual feelings
feelings_col <- "Do_you_experience_any_of_these_feelings_when_unable_to_access_social_media"
feelings <- c(
  "Boredom|Ennui",
  "Relief",
  "Anxious|Anxiété",
  "Isolated|Isolement",
  "No particular feelings|Aucun sentiment particulier",
  "None of these feelings|Aucun de ces sentiments",
  "Fear of missing out|FOMO",
  "Irritability",
  "Restlessness",
  "Loneliness|Solitude"
)

for (pattern in feelings) {
  name <- gsub("\\|.*", "", pattern)
  data[[paste0("Feeling_", gsub(" ", "_", name))]] <- as.numeric(
    grepl(paste0("\\b(", pattern, ")\\b"), data[[feelings_col]], ignore.case = TRUE)
  )
}

# Prepare correlation data
correlation_data <- data %>%
  select(Check_Frequency, starts_with("Feeling_")) %>%
  mutate(
    Check_Frequency_Numeric = as.numeric(Check_Frequency),
    across(-Check_Frequency, as.numeric)
  )

# Remove zero-variance columns
correlation_data_filtered <- correlation_data %>%
  select(-where(~var(., na.rm = TRUE) %in% c(0, NA))
         
         # Calculate Spearman correlations
         correlation_matrix <- cor(
           correlation_data_filtered[, -1, drop = FALSE],
           method = "spearman",
           use = "pairwise.complete.obs"
         ))
         
         # ------------------------------------------
         # Visualization 1: Bar Chart (Recommended)
         # ------------------------------------------
         frequency_correlations <- correlation_matrix["Check_Frequency_Numeric", , drop = FALSE] %>% 
           as.data.frame() %>%
           tibble::rownames_to_column(var = "Variable") %>%
           filter(Variable != "Check_Frequency_Numeric") %>%  # Fixed syntax
           rename(Correlation = Check_Frequency_Numeric)
         
         ggplot(frequency_correlations, aes(x = reorder(Variable, Correlation), y = Correlation)) +
           geom_col(fill = "#69b3a2", width = 0.7) +
           geom_hline(yintercept = 0, color = "gray40", linewidth = 0.8) +
           labs(
             title = "Correlation: Check Frequency vs Feelings",
             x = "Feeling When Unable to Access Social Media",
             y = "Spearman Correlation Coefficient"
           ) +
           coord_flip() +
           theme_minimal(base_size = 14)
         
         # ------------------------------------------
         # Visualization 2: Heatmap (Optional)
         # ------------------------------------------
         corrplot(
           correlation_matrix,
           method = "color",
           type = "upper",
           tl.col = "black",
           addCoef.col = "black",
           number.cex = 0.7
         )
         