# Load necessary libraries (if not already loaded)
library(NHANES)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import the NHANES dataset
data("NHANES")

# Choose two continuous and two categorical variables
continuous_vars <- c("Age", "BMI")
categorical_vars <- c("Gender", "Race1")

# Create a subset of 1000 individuals
nhanes_subset <- NHANES %>% sample_n(1000)

# Create a separate dataframe with ID, age, and chosen variables
subset_df <- nhanes_subset %>%
  select(ID, Age, all_of(continuous_vars), all_of(categorical_vars))

# Depict continuous variables as a histogram
histograms <- lapply(continuous_vars, function(var) {
  ggplot(subset_df, aes(x = !!as.name(var))) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black") +
    labs(title = var, x = var, y = "Frequency")
})

# Depict categorical variables as a barplot
barplots <- lapply(categorical_vars, function(var) {
  ggplot(subset_df, aes(x = !!as.name(var))) +
    geom_bar(fill = "green") +
    labs(title = var, x = var, y = "Count")
})

# Show histograms and barplots
gridExtra::grid.arrange(grobs = c(histograms, barplots), ncol = 2)

# Show all data in Table 1
table1 <- summary(subset_df)
print(table1)

# Group into age groups in 10-year increments and aggregate variables
age_groups <- nhanes_subset %>%
  mutate(Age_Group = cut(Age, breaks = seq(0, max(Age), by = 10))) %>%
  group_by(Age_Group) %>%
  summarise_at(vars(continuous_vars), mean, na.rm = TRUE) %>%
  ungroup()

# Calculate proportions of categorical variables within each age group
age_groups <- age_groups %>%
  group_by(Age_Group) %>%
  ungroup()

# Print the age_groups dataframe
print(age_groups)

# Show the central tendency and variability of continuous variables as a box plot
boxplot_plot <- subset_df %>%
  mutate(Age_Group = cut(Age, breaks = seq(0, max(Age), by = 10))) %>%
  ggplot(aes(x = Age_Group, y = Age, fill = Age_Group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Age Groups", x = "Age Group", y = "Age")

print(boxplot_plot)

