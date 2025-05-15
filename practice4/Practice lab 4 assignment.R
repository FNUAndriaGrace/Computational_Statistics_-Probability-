#empty environment
rm(list=ls())

#Loading libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(table1)
library(doBy)
install.packages('writexl')
library(writexl)


#folder definitions
read.directory<-"C:/Users/Andria Grace/Desktop/Semester1/Computaional stats and probabaility Module/"
#read.directory<-"C:/Users/3108313/OneDrive - Fresenius Medical Care/Desktop/JGR/Academics/Yeshiva/Fall 2023/Graduates/Module 3/"
write.directory<-"C:/Users/Andria Grace/Desktop/Semester1/Computaional stats and probabaility Module/cdc.xlsx"
file.name<-"Module 4 - df.xlsx"

#Read data
source("http://www.openintro.org/stat/data/cdc.R")
View(cdc)
write_xlsx(cdc,write.directory)
#Functions
SD_pop<-function(x,n){sqrt(sum( (x - mean(x) )^2)/(n))}
#b.	Calculate all index points for the empirical rule


#a.	Calculate all index points for the empirical rule
#calculate BMI
cdc$weight_kg<-cdc$weight/2.2
cdc$height_cm<-cdc$height * 2.5
cdc$height_m <-cdc$height_cm / 100
cdc$BMI <- cdc$weight_kg / cdc$height_m^2
hist(cdc$BMI)
df<-cdc
mean(df$BMI)
c(round(mean(df$BMI)-1*SD_pop(df$BMI,length(df$BMI)),2), "to", mean(df$BMI)+1*SD_pop(df$BMI,length(df$BMI))) #68%
c(mean(df$BMI)-2*SD_pop(df$BMI,length(df$BMI)), "to", mean(df$BMI)+2*SD_pop(df$BMI,length(df$BMI))) #95%
c(mean(df$BMI)-3*SD_pop(df$BMI,length(df$BMI)), "to", mean(df$BMI)+3*SD_pop(df$BMI,length(df$BMI))) #99.7%
#b.	Plot the normal distribution for BMI as a histogram in R
hist(df$BMI)
abline(v=mean(df$BMI)-1*SD_pop(df$BMI,length(df$BMI)),col="red",lty=2)
abline(v=mean(df$BMI)-2*SD_pop(df$BMI,length(df$BMI)),col="green",lty=4)
abline(v=mean(df$BMI)-3*SD_pop(df$BMI,length(df$BMI)),col="blue",lty=4)
abline(v=mean(df$BMI)+1*SD_pop(df$BMI,length(df$BMI)),col="red",lty=2)
abline(v=mean(df$BMI)+2*SD_pop(df$BMI,length(df$BMI)),col="green",lty=4)
abline(v=mean(df$BMI)+3*SD_pop(df$BMI,length(df$BMI)),col="blue",lty=4)

#e.	Calculate the above stated values, the z-scores and the probabilities
z_scores=(df$BMI-mean(df$BMI))/SD_pop((df$BMI),length(df$BMI))
print(z_scores)
pnorm(mean(df$BMI)-1*SD_pop(df$BMI,length(df$BMI)),mean=mean(df$BMI),sd=SD_pop(df$BMI,length(df$BMI)), lower.tail = TRUE)
pnorm(mean(df$BMI)+1*SD_pop(df$BMI,length(df$BMI)),mean=mean(df$BMI),sd=SD_pop(df$BMI,length(df$BMI)), lower.tail = TRUE)
pnorm(mean(df$BMI)-2*SD_pop(df$BMI,length(df$BMI)),mean=mean(df$BMI),sd=SD_pop(df$BMI,length(df$BMI)), lower.tail = TRUE)
pnorm(mean(df$BMI)+2*SD_pop(df$BMI,length(df$BMI)),mean=mean(df$BMI),sd=SD_pop(df$BMI,length(df$BMI)), lower.tail = TRUE)
pnorm(mean(df$BMI)-3*SD_pop(df$BMI,length(df$BMI)),mean=mean(df$BMI),sd=SD_pop(df$BMI,length(df$BMI)), lower.tail = TRUE)
pnorm(mean(df$BMI)+3*SD_pop(df$BMI,length(df$BMI)),mean=mean(df$BMI),sd=SD_pop(df$BMI,length(df$BMI)), lower.tail = TRUE)

hist(z_scores)
#generate a simulated normal distribution curve  
mean<-mean(df$BMI);sd=SD_pop(df$BMI,length(df$BMI))
x <- seq(0, 10, by=0.1)
dnorm(x, mean = mean, sd = sd, log = FALSE)
plot(x,dnorm(x, mean = mean, sd = sd, log = FALSE))

