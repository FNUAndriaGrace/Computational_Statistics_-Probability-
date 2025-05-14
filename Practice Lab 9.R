#Install the "pwr" package and perform the following examples:
library(pwr)  
#  Researchers want to compare for a planned RCT the sample sizes needed (sig level of 0.05; type =two- sided) to detect a difference with a power of 90% under the assumption of
#a small effect size (0.2)
pwr.t.test(power=0.9, d=0.2, sig.level=.05)
#a medium effect size (0.5)
pwr.t.test(power=0.9, d=0.5, sig.level=.05)
#a large effect size (0.8).
pwr.t.test(power=0.9, d=0.8, sig.level=.05)
#What is the power at large effect size when 36 subjects are enrolled?
pwr.t.test(n=36, d=0.8, sig.level=.05)

#  Researchers want to compare the sample sizes needed within each group when comparing 5 groups using a one-way ANOVA in a planned RCT (sig level of 0.05) aiming for a power of 90% with
#a small effect size (0.2)
pwr.anova.test(k=5,sig.level = .05, f=0.2, power=0.9)
#a medium effect size (0.5)
pwr.anova.test(k=5,sig.level = .05, f=0.5, power=0.9)
#a large effect size (0.8).
pwr.anova.test(k=5,sig.level = .05, f=0.8, power=0.9)
#What is the power when 40 subjects are enrolled?
pwr.anova.test(n=8,f=0.8, sig.level = 0.05,k=5)

#ANOVA: Load the dataset  downloadinto your R environment. 
df_source<-read.csv("C:/Users/acer/Desktop/Assignment/Practice Lab 9.csv")
df<-df_source
#Stratify the sample population in SBP tertiles. 
library(dplyr)
df$SBP_tertiles<-ntile(df$SBP,3)
df$SBP_tertiles_factor<-factor(df$SBP_tertiles,levels=c(1:3),labels=c("Tertile1","Tertile2","Tertile3"))
#compare the age of subjects in the respective tertiles. 
library(doBy)
summaryBy(SBP ~ SBP_tertiles_factor,data=df,FUN=c(mean,sd,min,max))
summaryBy(age ~ SBP_tertiles_factor,data=df,FUN=c(mean,sd,min,max))
#compare the age of subjects applying Bonferroni correction. 
t.test(df$age[df$SBP_tertiles==1],df$age[df$SBP_tertiles==2])
t.test(df$age[df$SBP_tertiles==1],df$age[df$SBP_tertiles==3])
t.test(df$age[df$SBP_tertiles==2],df$age[df$SBP_tertiles==3])
anova_test<-aov(df$age~df$SBP_tertiles_factor)
summary(anova_test)
pairwise.t.test(df$age,df$SBP_tertiles_factor,p.adjust.method = "bonferroni")
Tukey<-TukeyHSD(anova_test)
plot(Tukey)


