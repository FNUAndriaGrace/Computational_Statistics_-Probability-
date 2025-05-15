#Load the NHANES test dataset provided in the module. 
#clear the environment
rm(list=ls())
#Reading data
input_folder<-'C:/Users/acer/Desktop/Assignment/'
output_folder<-'C:/Users/acer/Desktop/Assignment/'
#Load the NHANES dataset from the Final Project (as per instructions provided). 
#install.packages(NHANES)
library(NHANES)
data("NHANES")
ls(NHANES)
NHANES<-NHANES[!duplicated(NHANES$ID),]
df_sample<-read.csv(paste(input_folder,"df_sample.csv",sep=""))
#Inspect the full dataset and transform variables into an analyzable format.
#Compute descriptive statistics for all variables of the full dataset.
#Preparing data
df<-NHANES
summary(df)
df$MALE<-ifelse(df$Gender=="male",1,0)
df$MALE_sex_factor<-factor(df$MALE)
df$MaritalStatus_factor<-factor(df$MaritalStatus)
df$Diabetes_factor<-factor(df$Diabetes)
df$PhysActive_factor<-factor(df$PhysActive)
summary(df)
df<-df[df$Age>18,]
summary(df$Height)
summary(df$Weight)
df$BMI<- df$Weight / (df$Height/100)^2
summary(df$BMI)
summary(df$Diabetes_factor)
summary(df_sample)

#Compare the BMI between Diabetic and non-diabetic participants. 
summary(df$BMI[df$Diabetes_factor=="Yes"])
summary(df$BMI[df$Diabetes_factor=="No"])
df$Diabetes_factor<-factor(df$Diabetes_factor,levels=c("Yes","No")) 

#Compare the BMI between diabetics and non-diabetics in the full dataset using an independent sample t-test

test1<-t.test(df$BMI[df$Diabetes_factor=="Yes"],
              df$BMI[df$Diabetes_factor=="No"])
test1

df_sample<-data.frame("BMI"=c(sample(df$BMI[df$Diabetes_factor=="Yes"],25),
                              sample(df$BMI[df$Diabetes_factor=="No"],25)),"Diabetes"=c(rep(1,25),rep(0,25)))
df_sample$BMI2<-df_sample$BMI+as.vector(c(sample(c(0:20),25,replace=TRUE),sample(c(-10:10),25,replace=TRUE)))
write.csv(df_sample,paste(output_folder,"df_sample.csv")) 

summary(df_sample$BMI[df_sample$Diabetes==1])
summary(df_sample$BMI[df_sample$Diabetes==0])
df_sample$Diabetes_factor<-factor(df_sample$Diabetes,levels=c(1,0))
test1<-t.test(df_sample$BMI[df_sample$Diabetes_factor==1],
              df_sample$BMI[df_sample$Diabetes_factor==0])
test1

test2<-t.test(df_sample$BMI[df_sample$Diabetes_factor==1],
              df_sample$BMI2[df_sample$Diabetes_factor==1],paired = TRUE)
test2

#Create a new dataframe of 25 diabetics and 25 non-diabetics and extract the SBP (in-class example BMI)
set.seed(123)  # For reproducibility
n_diabetics <- 25
n_non_diabetics <- 25

# Sample 25 diabetics and 25 non-diabetics separately
sample_diabetics <- NHANES[NHANES$Diabetes == "Yes", ]
sample_non_diabetics <- NHANES[NHANES$Diabetes == "No", ]

# Ensure that both datasets have the same structure and column names
sample_diabetics <- sample_diabetics[1:50, ]  # Limit to the first 50 rows
sample_non_diabetics <- sample_non_diabetics[1:50, ]  # Limit to the first 50 rows

# Combine the samples
df_sample <- rbind(sample_diabetics, sample_non_diabetics)
#Compare now SBP in the full dataset between diabetics and non-diabetics.
# Assuming "SBP" is the variable for systolic blood pressure and "Diabetes" is the variable for diabetes status in NHANES
summary(NHANES$BPSysAve[NHANES$Diabetes == "Yes"])  # Summary for SBP of diabetics
summary(NHANES$BPSysAve[NHANES$Diabetes == "No"])   # Summary for SBP of non-diabetics

#Create a new variable simulating a f/u measurement of SBP
# Assuming "SBP" is the variable for original SBP in your dataset
set.seed(123)  # For reproducibility

# Create an empty column for SBP_follow_up
df_sample$SBP_follow_up <- numeric(nrow(df_sample))

# Generate follow-up SBP values
for (i in 1:nrow(df_sample)) {
  df_sample$SBP_follow_up[i] <- df_sample$BPSysAve[i] + rnorm(1, mean = 0, sd = 5)
}

#Compare now SBP between diabetics and non-diabetics, and individually for both cohorts the SBP for both measurements
# Summary statistics for original SBP by diabetes status
summary(df_sample$BPSysAve[df_sample$Diabetes == "Yes"])  # Summary for SBP of diabetics
summary(df_sample$BPSysAve[df_sample$Diabetes == "No"])   # Summary for SBP of non-diabetics

# Summary statistics for follow-up SBP by diabetes status
summary(df_sample$SBP_follow_up[df_sample$Diabetes == "Yes"])  # Summary for follow-up SBP of diabetics
summary(df_sample$SBP_follow_up[df_sample$Diabetes == "No"])   # Summary for follow-up SBP of non-diabetics

#Depict the data of BMI in a box whisker plot separating both.
boxplot(df$BMI~df$Diabetes_factor)