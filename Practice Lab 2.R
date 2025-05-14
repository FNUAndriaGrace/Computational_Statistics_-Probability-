#clean the environment 
rm(list=ls())

#Loading packages
library(plotrix)

#load the cdc dataset
source("http://www.openintro.org/stat/data/cdc.R")

#investigate dataset
#1.	What data does it contain?
#2.	What kind of variables are there?
#3.	What type are the parameters contained?
ls()
ls(cdc)
a<-1
a <- c(1,2,3,4,5)
a<- cdc$age
a[3]
cdc$age[3]

cdc[3 ,  8]
str(cdc)
unique(cdc$genhlth)

#Describe the object:
#1.	How many parameters
dim(cdc)
length(cdc)
#2.	How many cases?
length(cdc$genhlth)
#3.	How many males/females?
summary(cdc$gender)
cdc$FEMALE <- ifelse(cdc$gender ==  "f",1,0) 
sum(cdc$FEMALE)
cdc$FEMALE <- ifelse(cdc$gender !=  "m",1,0) 
sum(cdc$FEMALE)
#4.	How many older than 25? 35?
length(cdc$FEMALE[cdc$age>25])
length(cdc$FEMALE[cdc$age>35])
#5.	Show the distribution of age as a histogram
hist(cdc$height)
summary(cdc$height)
sd(cdc$height)
hist(cdc$weight)
summary(cdc$weight)
sd(cdc$weight)

#Investigate outliers
cdc[ cdc$weight > 400, ]

#4.	How many older than 25? 35?
length(cdc$age[ cdc$age > 25 ])
length(cdc$age[ cdc$age > 35 ])

#5.	Show the distribution of age as a histogram
hist(cdc[,8], xlab="age [years]", main = "Age distribution")

#6Describe one continuous variable:
#1.	Calculate the summary statistics
mean(cdc$age)
median(cdc$age)
summary(cdc$age)
a <-summary(cdc$age)
a[3]
a[4]
as.numeric(a[4]-a[3])
var(cdc$age)
sqrt(var(cdc$age))
sd(cdc$age)
sd(cdc$age) / sqrt(length(cdc$age))
std.error(cdc$age)
as.numeric(a[5]-a[2])

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Describe one categorical variable:
#1.	Create a frequency table
#2.	Depict the frequency table using a barplot
counts<-table(cdc$gender,cdc$genhlth)
barplot(counts, main="General Health and Gender",
        xlab="General Health", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


