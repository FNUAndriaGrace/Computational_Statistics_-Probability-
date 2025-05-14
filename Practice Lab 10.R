#Load the dataset from the module into your R environment. 
df<-read.csv("C:/Users/acer/Desktop/Assignment/Practice Lab 10.csv")
#Stratify the sample population in SBP tertiles. 
library(dplyr)
df$SBP_tertiles<-ntile(df$SBP,3)
df$SBP_tertiles_factor<-factor(df$SBP_tertiles,levels=c(1:3),labels=c("Tertile1","Tertile2","Tertile3"))

#compare the age of subjects in the respective tertiles. 
library(doBy)
summaryBy(SBP ~ SBP_tertiles_factor,data=df,FUN=c(mean,sd,min,max))
summaryBy(age ~ SBP_tertiles_factor,data=df,FUN=c(mean,sd,min,max))

#Stratify the population in subjects younger that 35, between 35 and 65 and those older than 65 years.
df$age_cat<-NA
df$age_cat[df$age<35]<-1

df$age_cat[df$age>=35 & df$age<=65]<-2
df$age_cat[df$age>65]<-3
df$age_cat_factor<-factor(df$age_cat,levels=c(1:3),labels=c("below35","between35_65","above65"))
summary(df$age_cat_factor)

#Compare the age of subjects in the respective SBP tertiles utilizing ANOVA.
anova_test<-aov(df$age~df$SBP_tertiles_factor)
summary(anova_test)
pairwise.t.test(df$age,df$SBP_tertiles_factor,p.adjust.method = "bonferroni")

#Compare the SBP (as a continuous variable) of subjects in the age categories utilizing ANOVA.
anova_test<-aov(df$SBP~df$age_cat_factor)
summary(anova_test)
pairwise.t.test(df$SBP,df$age_cat_factor,p.adjust.method = "bonferroni")

#Test for associations between these categories using the chi square test. 
SBP_age<-table(df$age_cat_factor,df$SBP_tertiles_factor)
SBP_age
test<-chisq.test(SBP_age)
ls(chisq.test(SBP_age))
chisq.test(SBP_age)$observed
chisq.test(SBP_age)$expected
chisq.test(SBP_age)$residuals

#Calculate relative risk to be in the highest tertile of SBP for the oldest in the dataset compared to those younger than 65 years.
Risk_young<-sum(SBP_age[1:2,3])/sum(SBP_age[1:2,1:3])
Risk_old<-sum(SBP_age[3,3])/sum(SBP_age[3,1:3])
Relative_Risk<-Risk_old / Risk_young
print(Relative_Risk)

