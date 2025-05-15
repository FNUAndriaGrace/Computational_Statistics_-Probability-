#Practice Lab 6 - Topic 5
#Load the NHANES dataset from the Final Project (as per instructions provided). 
#install.packages(NHANES)
library(NHANES)
data("NHANES")
ls(NHANES)
NHANES<-NHANES[!duplicated(NHANES$ID),]

#packages
library(dplyr)
library(ggplot2)

#folder definitions
read.directory<-"C:/Users/acer/Desktop/Assignment/"

#Load functions
source(paste(read.directory,"SD_pop.R",sep=""))

#install.packages(NHANES) 
library(nhanesA)
#https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
nhanesTables('DEMOGRAPHICS', 2010)
nhanesTables('DIETARY', 2010)
nhanesTables('EXAMINATION', 2010)
nhanesTables('LABORATORY', 2010)
nhanesTables('QUESTIONNAIRE', 2010)

#Compute descriptive statistics for “age” and “sex” Of the full dataset.
str(NHANES)
summary(NHANES$Age)
population_mean_age =mean(NHANES$Age)
population_mean_age
median(NHANES$Age)
# Descriptive statistics for "sex" column
table(NHANES$Gender)

length(NHANES$Gender[NHANES$Gender=="male"]) /  length(NHANES$Gender)

#Create a dataset containing “ID”, “age” and “biological sex” of the first 500 participants. 

# Set seed for reproducibility
set.seed(42)

# Number of random participants
n <- 500

# Generate random IDs, ages, and sexes
ID <- 1:n

random_age <- sample(NHANES$Age, n, replace = TRUE)
random_sex <- sample(NHANES$Gender, n, replace = TRUE)

# Create a data frame with ID, age, and sex for the random participants
df <- data.frame(ID, age = random_age, sex = random_sex)

# Descriptive statistics for age of the random sample
summary_age_sample = summary(df$age)
summary_age_sample
# Descriptive statistics for sex of the random sample
table(df$sex)

#Test the representativeness of your sample to the overall population computing the margin of error 
#and the 90, 95 and 99% confidence interval estimates. For the proportion of sex use the normal approximation.


# Calculate proportion of sex  in the full dataset (excluding missing values)
proportion_male_full <- sum(NHANES$Gender == "male" & !is.na(NHANES$Gender)) / sum(!is.na(NHANES$Gender))
proportion_male_sample <- sum(df$sex == "male" & !is.na(df$sex)) / sum(!is.na(df$sex))



# Calculate the standard error for proportion of males in the sample
sample_size <- nrow(df)
SE <- sqrt(proportion_male_sample * (1 - proportion_male_sample) / sample_size)

# Calculate Z-scores for 90%, 95%, and 99% confidence intervals
z_scores <- c(1.645, 1.96, 2.576)

# Calculate margin of error for each confidence level
margin_of_error <- z_scores * SE
margin_of_error

# Compute confidence intervals for different confidence levels
confidence_intervals <- matrix(nrow = 3, ncol = 2)
confidence_intervals[, 1] <- proportion_male_sample - margin_of_error
confidence_intervals[, 2] <- proportion_male_sample + margin_of_error

# Print confidence intervals
rownames(confidence_intervals) <- c("90%", "95%", "99%")
colnames(confidence_intervals) <- c("Lower Bound", "Upper Bound")
print(confidence_intervals)


#Depict the distribution of your sample for the continuous data with the confidence intervals in a histogram. 

# Depict the distribution of age in a histogram with confidence intervals
hist(df$age, col = "skyblue", main = "Age Distribution of Sample (n=500)", xlab = "Age")
abline(v = summary_age_sample[4:5], col = "red", lwd = 2)
legend("topright", legend = c("Confidence Interval", "Sample Mean"), fill = c("red", "green"))


#Depict the data of the continuous variable of the sample in a box whisker plot.

boxplot(df$age, main="Box Plot of Continuous Variable", ylab="age")


#Hypothesis testing
#Test the hypotheses that the sample mean for age is significantly greater, smaller or different (two-sided) from the population mean.

#Null Hypothesis (H0): The sample mean age is equal to the population mean age.
#Alternative Hypothesis (H1): The sample mean age is significantly greater, smaller, or different from the population mean age.

# Perform t-test for age mean
t_test_result <- t.test(df$age, mu = population_mean_age, alternative = "two.sided")
print(t_test_result)

#Test the hypotheses that the proportion of women is significantly greater, smaller or different (two-sided) from the proportion in the overall population. 

#Null Hypothesis (H0): The proportion of women in the sample is equal to the proportion in the overall population.
#Alternative Hypothesis (H1): The proportion of women in the sample is significantly greater, smaller, or different from the proportion in the overall population.
# Calculate sample proportion of women in the sample
sample_proportion_women <- sum(df$sex == "female") / nrow(df)
print(paste("Sample Proportion of Women: ", sample_proportion_women))

# Calculate proportion of women in the overall population
proportion_women_in_population <- sum(NHANES$Gender == "female") / nrow(NHANES)
print(paste("Proportion of Women in Population: ", proportion_women_in_population))

# Perform z-test for proportion of women
z_test_result <- prop.test(sum(sample_proportion_women), length(sample_proportion_women), p = proportion_women_in_population, alternative = "two.sided")
print(z_test_result)


#Distribution
m <- ggplot(df, aes(x=age))
m + geom_histogram(aes(y = ..density..,fill = ..count..))  + geom_density()

#Definitions
mew<-mean(NHANES$Age[NHANES$Age>18])
sigma<-SD_pop(NHANES$Age[NHANES$Age>18],n=length(NHANES$Age[NHANES$Age>18]))
sigma_estimated<-sd(df$age)
xbar<-mean(df$age)
n<-length(df$age)
SEM<-(sigma/sqrt(n))

z<-(xbar-mew)/SEM
z

#2-tailed
p_2tailed<-2 * pnorm( - abs(z))
p_2tailed
#not rejecting the null

#1-tailed 
#Null hypothesis sample is greater/equal than the mew 
#Research hypothesis sample is smaller than the mew
p_1tailed_smaller <- pnorm(z)
p_1tailed_smaller
#not rejecting the null

#1-tailed 
#Null hypothesis sample is smaller/equal than the mew 
#Research hypothesis sample is greater than the mew
p_1tailed_greater <- 1 - pnorm(z)
p_1tailed_greater
#not rejecting the null

