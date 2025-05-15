# install.packages(NHANES)
library(NHANES)

# Load necessary library
# install.packages("dplyr")
library(dplyr)

data("NHANES")
ls(NHANES)
NHANES<-NHANES[!duplicated(NHANES$ID),]

library(nhanesA)
#https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
nhanesTables('DEMOGRAPHICS', 2010)
nhanesTables('DIETARY', 2010)
nhanesTables('EXAMINATION', 2010)
nhanesTables('LABORATORY', 2010)
nhanesTables('QUESTIONNAIRE', 2010)

# Create a new object with selected variables
selected_data <- NHANES[c("Age", "Gender", "Height", "Weight", "BPSysAve", "BPDiaAve", "Diabetes","PhysActive","BMI")]

# checking type of categorical variables and others

class(selected_data$BPSysAve)
class(selected_data$BPDiaAve)
class(selected_data$Age)
class(selected_data$Height)
class(selected_data$Weight)
class(selected_data$BMI)
class(selected_data$Gender)
class(selected_data$Diabetes)
class(selected_data$PhysActive)

# categorical variables are already in factors so no need to recode them 

# Function to replace NA values in categorical columns with mode
replace_na_with_mode <- function(data) {
  for (col in names(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      mode_value <- tryCatch({
        names(sort(table(data[[col]]), decreasing = TRUE))[1]
      }, error = function(e) {
        NA
      })
      data[[col]][is.na(data[[col]])] <- mode_value
    }
  }
  return(data)
}

# Replace NA values in all categorical columns of your dataframe
selected_data <- replace_na_with_mode(selected_data)


# Replace NA values with column averages
selected_data$BPSysAve[is.na(selected_data$BPSysAve)] <- mean(selected_data$BPSysAve, na.rm = TRUE)
selected_data$BPDiaAve[is.na(selected_data$BPDiaAve)] <- mean(selected_data$BPDiaAve, na.rm = TRUE)
selected_data$Age[is.na(selected_data$Age)] <- mean(selected_data$Age, na.rm = TRUE)
selected_data$Height[is.na(selected_data$Height)] <- mean(selected_data$Height, na.rm = TRUE)
selected_data$Weight[is.na(selected_data$Weight)] <- mean(selected_data$Weight, na.rm = TRUE)
selected_data$BMI[is.na(selected_data$BMI)] <- mean(selected_data$BMI, na.rm = TRUE)



# Check the structure of the new object
str(selected_data)
print(n = 100,selected_data)

table1 <- summary(selected_data)
print("Table 1: Descriptive Statistics for All Included Variables")
print(table1)

# Define functions for z-tests
z_test <- function(sample_mean, population_mean, population_sd, sample_size) {
  z_value <- (sample_mean - population_mean) / (population_sd / sqrt(sample_size))
  return(z_value)
}

# Function to calculate statistics
calculate_stats <- function(data) {
  mean_val <- mean(data)
  sd_val <- sd(data)
  min_val <- min(data)
  max_val <- max(data)
  return(c(Mean = mean_val, SD = sd_val, Min = min_val, Max = max_val))
}

# Descriptive statistics for systolic and diastolic blood pressure for High and Low physical activity levels
high_activity_systolic_stats <- calculate_stats(selected_data[selected_data$PhysActive == "Yes", ]$BPSysAve)
low_activity_systolic_stats <- calculate_stats(selected_data[selected_data$PhysActive == "No", ]$BPSysAve)
high_activity_diastolic_stats <- calculate_stats(selected_data[selected_data$PhysActive == "Yes", ]$BPDiaAve)
low_activity_diastolic_stats <- calculate_stats(selected_data[selected_data$PhysActive == "No", ]$BPDiaAve)

# Descriptive statistics for systolic and diastolic blood pressure for High and Normal BMI categories
high_bmi_systolic_stats <- calculate_stats(selected_data[selected_data$BMI > 30, ]$BPSysAve)
normal_bmi_systolic_stats <- calculate_stats(selected_data[selected_data$BMI >= 18.5 & selected_data$BMI <= 25, ]$BPSysAve)
high_bmi_diastolic_stats <- calculate_stats(selected_data[selected_data$BMI > 30, ]$BPDiaAve)
normal_bmi_diastolic_stats <- calculate_stats(selected_data[selected_data$BMI >= 18.5 & selected_data$BMI <= 25, ]$BPDiaAve)

# Create Table 2a: Descriptive Statistics for Systolic Blood Pressure
table_2a <- data.frame(
  Group = c("High Activity", "Low Activity", "High BMI", "Normal BMI"),
  Mean_Systolic_BP = c(high_activity_systolic_stats["Mean"], low_activity_systolic_stats["Mean"],
                       high_bmi_systolic_stats["Mean"], normal_bmi_systolic_stats["Mean"]),
  SD_Systolic_BP = c(high_activity_systolic_stats["SD"], low_activity_systolic_stats["SD"],
                     high_bmi_systolic_stats["SD"], normal_bmi_systolic_stats["SD"]),
  Min_Systolic_BP = c(high_activity_systolic_stats["Min"], low_activity_systolic_stats["Min"],
                      high_bmi_systolic_stats["Min"], normal_bmi_systolic_stats["Min"]),
  Max_Systolic_BP = c(high_activity_systolic_stats["Max"], low_activity_systolic_stats["Max"],
                      high_bmi_systolic_stats["Max"], normal_bmi_systolic_stats["Max"])
)
print("Table 2a: Descriptive Statistics for Systolic Blood Pressure")
print(table_2a)

# Create Table 2b: Descriptive Statistics for Diastolic Blood Pressure
table_2b <- data.frame(
  Group = c("High Activity", "Low Activity", "High BMI", "Normal BMI"),
  Mean_Diastolic_BP = c(high_activity_diastolic_stats["Mean"], low_activity_diastolic_stats["Mean"],
                        high_bmi_diastolic_stats["Mean"], normal_bmi_diastolic_stats["Mean"]),
  SD_Diastolic_BP = c(high_activity_diastolic_stats["SD"], low_activity_diastolic_stats["SD"],
                      high_bmi_diastolic_stats["SD"], normal_bmi_diastolic_stats["SD"]),
  Min_Diastolic_BP = c(high_activity_diastolic_stats["Min"], low_activity_diastolic_stats["Min"],
                       high_bmi_diastolic_stats["Min"], normal_bmi_diastolic_stats["Min"]),
  Max_Diastolic_BP = c(high_activity_diastolic_stats["Max"], low_activity_diastolic_stats["Max"],
                       high_bmi_diastolic_stats["Max"], normal_bmi_diastolic_stats["Max"])
)
print("Table 2b: Descriptive Statistics for Diastolic Blood Pressure")
print(table_2b)



# Generate random samples for systolic and diastolic blood pressure within specified groups
set.seed(123)  # Set seed for reproducibility
high_activity_systolic <- sample(selected_data[selected_data$PhysActive == "Yes", ]$BPSysAve, 250,replace = TRUE)
low_activity_systolic <- sample(selected_data[selected_data$PhysActive == "No", ]$BPSysAve, 250,replace = TRUE)
high_activity_diastolic <- sample(selected_data[selected_data$PhysActive == "Yes", ]$BPDiaAve, 250,replace = TRUE)
low_activity_diastolic <- sample(selected_data[selected_data$PhysActive == "No", ]$BPDiaAve, 250,replace = TRUE)

high_bmi_systolic <- sample(selected_data[selected_data$BMI > 30, ]$BPSysAve, 250)
normal_bmi_systolic <- sample(selected_data[selected_data$BMI >= 18.5 & selected_data$BMI <= 25, ]$BPSysAve, 250)
high_bmi_diastolic <- sample(selected_data[selected_data$BMI > 30, ]$BPDiaAve, 250)
normal_bmi_diastolic <- sample(selected_data[selected_data$BMI >= 18.5 & selected_data$BMI <= 25, ]$BPDiaAve, 250)

# Compute z-scores for systolic and diastolic blood pressure samples
z_high_activity_systolic <- z_test(mean(high_activity_systolic), mean(selected_data$BPSysAve), sd(selected_data$BPSysAve), 250)
z_low_activity_systolic <- z_test(mean(low_activity_systolic), mean(selected_data$BPSysAve), sd(selected_data$BPSysAve), 250)
z_high_activity_diastolic <- z_test(mean(high_activity_diastolic), mean(selected_data$BPDiaAve), sd(selected_data$BPDiaAve), 250)
z_low_activity_diastolic <- z_test(mean(low_activity_diastolic), mean(selected_data$BPDiaAve), sd(selected_data$BPDiaAve), 250)

z_high_bmi_systolic <- z_test(mean(high_bmi_systolic), mean(selected_data$BPSysAve), sd(selected_data$BPSysAve), 250)
z_normal_bmi_systolic <- z_test(mean(normal_bmi_systolic), mean(selected_data$BPSysAve), sd(selected_data$BPSysAve), 250)
z_high_bmi_diastolic <- z_test(mean(high_bmi_diastolic), mean(selected_data$BPDiaAve), sd(selected_data$BPDiaAve), 250)
z_normal_bmi_diastolic <- z_test(mean(normal_bmi_diastolic), mean(selected_data$BPDiaAve), sd(selected_data$BPDiaAve), 250)

#Perform hypothesis testing using z-tests for systolic and diastolic blood pressure
alpha <- 0.05

# Two-sided tests
p_high_activity_systolic_two_sided <- 2 * (1 - pnorm(abs(z_high_activity_systolic)))
p_low_activity_systolic_two_sided <- 2 * (1 - pnorm(abs(z_low_activity_systolic)))
p_high_activity_diastolic_two_sided <- 2 * (1 - pnorm(abs(z_high_activity_diastolic)))
p_low_activity_diastolic_two_sided <- 2 * (1 - pnorm(abs(z_low_activity_diastolic)))

p_high_bmi_systolic_two_sided <- 2 * (1 - pnorm(abs(z_high_bmi_systolic)))
p_normal_bmi_systolic_two_sided <- 2 * (1 - pnorm(abs(z_normal_bmi_systolic)))
p_high_bmi_diastolic_two_sided <- 2 * (1 - pnorm(abs(z_high_bmi_diastolic)))
p_normal_bmi_diastolic_two_sided <- 2 * (1 - pnorm(abs(z_normal_bmi_diastolic)))


# One-sided tests
p_high_activity_systolic_one_sided <- 1 - pnorm(z_high_activity_systolic)
p_low_activity_systolic_one_sided <- pnorm(z_low_activity_systolic)
p_high_activity_diastolic_one_sided <- 1 - pnorm(z_high_activity_diastolic)
p_low_activity_diastolic_one_sided <- pnorm(z_low_activity_diastolic)

p_high_bmi_systolic_one_sided <- 1 - 
  (z_high_bmi_systolic)
p_normal_bmi_systolic_one_sided <- 1 - pnorm(z_normal_bmi_systolic)
p_high_bmi_diastolic_one_sided <- 1 - pnorm(z_high_bmi_diastolic)
p_normal_bmi_diastolic_one_sided <- 1 - pnorm(z_normal_bmi_diastolic)

# Output results
print("Hypothesis Testing Results for Systolic Blood Pressure (Two-sided tests)")
results_systolic_two_sided <- data.frame(
  Group = c("High Activity", "Low Activity", "High BMI", "Normal BMI"),
  P_Value_Systolic_Two_Sided = c(p_high_activity_systolic_two_sided, p_low_activity_systolic_two_sided,
                                 p_high_bmi_systolic_two_sided, p_normal_bmi_systolic_two_sided)
)
print(results_systolic_two_sided)

print("Hypothesis Testing Results for Diastolic Blood Pressure (Two-sided tests)")
results_diastolic_two_sided <- data.frame(
  Group = c("High Activity", "Low Activity", "High BMI", "Normal BMI"),
  P_Value_Diastolic_Two_Sided = c(p_high_activity_diastolic_two_sided, p_low_activity_diastolic_two_sided,
                                  p_high_bmi_diastolic_two_sided, p_normal_bmi_diastolic_two_sided)
)
print(results_diastolic_two_sided)

print("Hypothesis Testing Results for Systolic Blood Pressure (One-sided tests)")
results_systolic_one_sided <- data.frame(
  Group = c("High Activity", "Low Activity", "High BMI", "Normal BMI"),
  P_Value_Systolic_One_Sided = c(p_high_activity_systolic_one_sided, p_low_activity_systolic_one_sided,
                                 p_high_bmi_systolic_one_sided, p_normal_bmi_systolic_one_sided)
)
print(results_systolic_one_sided)

print("Hypothesis Testing Results for Diastolic Blood Pressure (One-sided tests)")
results_diastolic_one_sided <- data.frame(
  Group = c("High Activity", "Low Activity", "High BMI", "Normal BMI"),
  P_Value_Diastolic_One_Sided = c(p_high_activity_diastolic_one_sided, p_low_activity_diastolic_one_sided,
                                  p_high_bmi_diastolic_one_sided, p_normal_bmi_diastolic_one_sided)
)
print(results_diastolic_one_sided)

par(mar=c(3, 3, 2, 1))
# Plot histograms for systolic and diastolic blood pressure within specified groups

par(mfrow=c(2, 2))
hist(high_activity_systolic, main="High Activity - Systolic BP", xlab="Systolic BP")
hist(low_activity_systolic, main="Low Activity - Systolic BP", xlab="Systolic BP")
hist(high_activity_diastolic, main="High Activity - Diastolic BP", xlab="Diastolic BP")
hist(low_activity_diastolic, main="Low Activity - Diastolic BP", xlab="Diastolic BP")

# Plot histograms for systolic and diastolic blood pressure 
par(mfrow=c(2, 2))
hist(high_bmi_systolic, main="High BMI - Systolic BP", xlab="Systolic BP")
hist(normal_bmi_systolic, main="Normal BMI - Systolic BP", xlab="Systolic BP")
hist(high_bmi_diastolic, main="High BMI - Diastolic BP", xlab="Diastolic BP")
hist(normal_bmi_diastolic, main="Normal BMI - Diastolic BP", xlab="Diastolic BP")

