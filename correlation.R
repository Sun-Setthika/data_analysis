# Load required libraries
library(dplyr)
library(readr)
library(corrplot)

# Load data
data <- read_csv("D:/Files/Side_Project/preprocess_data/merged_dataset.csv")

# Standardize column names
colnames(data) <- gsub("[ ?]", "_", colnames(data))

# Convert Check_Frequency to ordered factor
data$Check_Frequency <- factor(
  data$How_often_do_you_check_social_media,
  levels = c(
    "Once a day", 
    "Several times a day", 
    "Multiple times per hour",
    "Every hour",
    ""
  ),
  ordered = TRUE
)

# Verify feelings column exists
feelings_col <- "Do_you_experience_any_of_these_feelings_when_unable_to_access_social_media"
if (!feelings_col %in% colnames(data)) stop("Column not found: ", feelings_col)

# Create binary indicators for feelings (ensure numeric)
feelings <- c("Boredom", "Relief", "Anxious", "Isolated", "No particular feelings / None of these feelings", "Fear of missing out", "Irritability", "Restlessness", "Loneliness")
for (feeling in feelings) {
  data[[paste0("Feeling_", feeling)]] <- as.numeric(
    grepl(paste0("\\b", feeling, "\\b"), 
          data[[feelings_col]], 
          ignore.case = TRUE
    )
  )
}

# Prepare correlation data
correlation_data <- data %>%
  select(Check_Frequency, starts_with("Feeling_")) %>%
  mutate(
    Check_Frequency_Numeric = as.numeric(Check_Frequency)
  )

# Ensure ALL columns are numeric (critical fix)
correlation_data <- correlation_data %>%
  mutate(across(-Check_Frequency, as.numeric))

# Remove zero-variance columns (safely)
zero_var_cols <- sapply(correlation_data[, -1], function(x) {
  var_val <- var(x, na.rm = TRUE)
  if (is.na(var_val)) FALSE else var_val == 0
})
zero_var_cols <- names(zero_var_cols[zero_var_cols])

if (length(zero_var_cols) > 0) {
  correlation_data_filtered <- correlation_data %>% select(-all_of(zero_var_cols))
} else {
  correlation_data_filtered <- correlation_data
}

# Verify numeric types (add this check)
cat("\nColumn types in filtered data:\n")
print(sapply(correlation_data_filtered[, -1], class))

# Calculate correlations
correlation_matrix <- cor(
  correlation_data_filtered[, -1, drop = FALSE],  # Exclude factor column
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Subset to frequency vs feelings
frequency_correlations <- correlation_matrix["Check_Frequency_Numeric", ]
frequency_correlations <- frequency_correlations[
  names(frequency_correlations) != "Check_Frequency_Numeric"
]

# Visualize
corrplot(
  as.matrix(frequency_correlations),
  method = "color",
  tl.col = "black",
  tl.srt = 45,
  title = "Check Frequency vs. Feelings",
  mar = c(0, 0, 2, 0),
  cl.pos = "r"
)
