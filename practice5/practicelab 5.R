
download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")

load("ames.RData")



# 1).Focus on the living area size
living_area <- ames$Gr.Liv.Area


#2).Create a sample of 60 houses, describe the distribution of your sample and compare it to
#the overall population. What would you say is the “typical” size within your sample / the
#overall population? 
#Also state precisely what you interpreted “typical” to mean:"Typical" typically means the "central tendency" of the data, often represented by the mean. 


set.seed(123)  # Set a seed for reproducibility
sample_size <- 60
sample_data <- sample(living_area, sample_size)

# Descriptive statistics for the sample
summary(sample_data)

# Descriptive statistics for the population
summary(living_area)


#3). Create a second sample and describe and compare this one

sample_data2 <- sample(living_area, sample_size)

# Describing:
summary(sample_data2)
#comparing:
summary(living_area)


#4).What would be the result to take more samples:  It provides  better understanding of the variability of sample means. 
#What were the result of taking larger samples? 
# Which one would provide the more accurate estimate of the population mean?
#Ans: Larger samples tend to provide a more accurate estimate of the population mean.


#5).Create a dataset with 500 independent sample means from the overall population;
#hOW does the mean distribute? What does the mean of this distribution tell us?
# Create a function to perform sampling and calculate the sample mean

simulate_samples <- function(sample_size, num_samples) {
  sample_means <- replicate(num_samples, mean(sample(living_area, sample_size)))
  return(sample_means)
}

#Creating 500 samples using the function above
num_samples_to_take <- 500
sample_means <- simulate_samples(sample_size, num_samples_to_take)

# Compare the distribution of sample means to the population mean
hist(sample_means, main = "Distribution of Sample Means", xlab = "Sample Mean")
abline(v = mean(living_area), col = "red", lwd = 2)
print(mean(living_area))

#Create a dataset with 500 independent sample means from the overall population:
sample_means_dataset <- simulate_samples(sample_size, 500)

# Calculate the mean of this distribution
mean_of_sample_means <- mean(sample_means_dataset)




#6).Take a random sample of 50 from price. What is the best point estimate of the population
#mean?
# Set a seed for reproducibility
set.seed(123)

#7).Take a random sample of 50 observations from the "price" variable
sample_price <- sample(ames$SalePrice, 50)

# Calculate the sample mean (point estimate of the population mean)
sample_mean_price <- mean(sample_price)




#8).Create a dataframe with 5000 independent sample means and compare the distribution and
#the mean of the means to those of the overall population.
sample_means_dataframe <- simulate_samples(sample_size, 5000)

# Calculate the mean of means
mean_of_means <- mean(sample_means_dataframe)

# Compare the distribution of sample means to the population mean
hist(sample_means_dataframe, main = "Distribution of Sample Means", xlab = "Sample Mean")
abline(v = mean(living_area), col = "green", lwd = 2)


#9).Stratify the overall population by prize (create an additional variable) and compare the
#sample means in the most expensive quartile and the least expensive quartile to the overall
#population mean).
# Create an additional variable for price quartiles


# Stratifying the population by SalePrice
SalePrice_quartiles <- quantile(ames$SalePrice, probs = c(0, 0.25, 0.5, 0.75, 1))
ames$SalePrice_Quartile <- cut(ames$SalePrice, breaks = SalePrice_quartiles, labels = c("Q1", "Q2", "Q3", "Q4"))

# Calculate sample means in the most expensive quartile and the least expensive quartile
expensive_quartile_mean <- mean(ames$Gr.Liv.Area[ames$stratum == "4th Qu."])
least_expensive_quartile_mean <- mean(ames$Gr.Liv.Area[ames$stratum == "1st Qu."])

# Compare the sample means in the most expensive quartile and the least expensive quartile
summary(subset(ames, SalePrice_Quartile == "Q1")$Gr.Liv.Area)
summary(subset(ames, SalePrice_Quartile == "Q4")$Gr.Liv.Area)

# Calculate Z-scores
population_mean <- mean(ames$Gr.Liv.Area)
population_sd <- sd(ames$Gr.Liv.Area)
sample_mean_Q1 <- mean(subset(ames, SalePrice_Quartile == "Q1")$Gr.Liv.Area)
sample_mean_Q4 <- mean(subset(ames, SalePrice_Quartile == "Q4")$Gr.Liv.Area)
z_score_Q1 <- (sample_mean_Q1 - population_mean) / (population_sd / sqrt(60))
z_score_Q4 <- (sample_mean_Q4 - population_mean) / (population_sd / sqrt(60))
# Print the z-scores

cat("Z-score for Q1:", z_score_Q1, "\n")
cat("Z-score for Q4:", z_score_Q4, "\n")


