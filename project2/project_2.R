#install.packages("zoo")
#library(zoo)
# Load necessary library
#install.packages("dplyr")
library(dplyr)

library(NHANES)
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


# Scatter plot
plot(selected_data$BMI, selected_data$BPSysAve,
     xlab = "Body Mass Index (BMI)", ylab = "Systolic Blood Pressure",
     main = "Scatter Plot: BMI vs Systolic Blood Pressure")

# Test for correlation
correlation <- cor(selected_data$BMI, selected_data$BPSysAve, use = "complete.obs")
print(paste("Correlation between BMI and Systolic Blood Pressure:", correlation))


# Build a regression model with BMI and Diastolic Blood Pressure as predictors
# Convert Height, Weight, and Diastolic BP to numeric
selected_data$Height <- as.numeric(as.character(selected_data$Height))
selected_data$Weight <- as.numeric(as.character(selected_data$Weight))
selected_data$BPDiaAve <- as.numeric(as.character(selected_data$BPDiaAve))


# Build a regression model with BMI and Diastolic Blood Pressure as predictors
regression_model <- lm(formula = BPSysAve ~ BMI, data = selected_data)
summary(regression_model)

# association between  

plot(c(min(selected_data$BMI),max(selected_data$BMI)),c(min(selected_data$BPSysAve),max(selected_data$BPSysAve)),col="white",
     xlab="BMI [yrs]",ylab="Avg Systolic blood pressure [mmHg]")
points(selected_data$BMI,selected_data$BPSysAve,pch=2,cex=0.6,col="red")
lm<-lm(selected_data$BPSysAve~selected_data$BMI)
abline(lm,col="blue")
text(x=25,y=210,labels=paste("R2=",round(summary(regression_model)$adj.r.squared,2),sep=""))

# Hypothesis Test Explanation
cat("### Hypothesis Test: Simple Regression Model ###\n")
cat("Hypothesis: There exists a significant association between BMI and Systolic BP\n\n")

# Formulation of Null and Research Hypotheses
cat("Null Hypothesis (H0): There is no significant association between BMI and Systolic BP. Mathematically, the coefficients for BMI and age in the regression model are equal to zero.\n")
cat("Alternative Hypothesis (H1): There is a significant association between BMI and Systolic BP.At least one of the coefficients for BMI or age is not equal to zero.\n\n")

# Adjust for age in the regression model
adjusted_model <- lm(formula = BPSysAve ~ BMI+Age, data = selected_data)
summary(adjusted_model)
print(summary(adjusted_model))

adjusted_model$residuals
plot(adjusted_model)
summary(adjusted_model)$adj.r.squared
ls(adjusted_model)
ls(summary(adjusted_model))
summary(adjusted_model)$coefficients


