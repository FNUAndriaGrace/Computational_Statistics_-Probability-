#Loading libraries
library(pROC)

#Building a simulated dataset
df<-data.frame('PATIENT_ID'=1:649,"EGFR30"=rbinom(649, 1, 0.177))
df$EGFR30<-factor(df$EGFR30)
summary(df$EGFR30)
df$SUN_diag<-NA
df$SUN_diag[df$EGFR30==0]<-rbinom(length(df$EGFR30[df$EGFR30==0]), 1, (1-0.846))
df$SUN_diag[df$EGFR30==1]<-rbinom(length(df$EGFR30[df$EGFR30==1]), 1, 0.661)
df$SUN_diag<-factor(df$SUN_diag)
summary(df$SUN_diag)
df$SUN<-1
df$SUN[df$SUN_diag == 1] <- sample(2:6,replace=TRUE,size=length(df$SUN[df$SUN_diag == 1]))
df$SUN<-factor(df$SUN)
summary(df$SUN)
df<-df[order(df$SUN),]

#assigning diagnostic criteria 
df$Diag_cat<-NA
df$Diag_cat[df$EGFR30==1 & df$SUN_diag == 1]<-"TP"
df$Diag_cat[df$EGFR30==0 & df$SUN_diag == 0]<-"TN"
df$Diag_cat[df$EGFR30==1 & df$SUN_diag == 0]<-"FN"
df$Diag_cat[df$EGFR30==0 & df$SUN_diag == 1]<-"FP"
df$Diag_cat_factor<-factor(df$Diag_cat)
summary(df$Diag_cat_factor)

#calculate sens, spec and predictive values
TP<-length(df$Diag_cat_factor[df$Diag_cat_factor=="TP"])
TP<-summary(df$Diag_cat_factor)[4]
sens<-as.numeric(summary(df$Diag_cat_factor)[4] / sum(summary(df$Diag_cat_factor)[c(1,4)]))
spec<-as.numeric(summary(df$Diag_cat_factor)[3] / sum(summary(df$Diag_cat_factor)[c(3,2)]))
PPV<-as.numeric(summary(df$Diag_cat_factor)[4] / sum(summary(df$Diag_cat_factor)[c(2,4)]))
NPV<-as.numeric(summary(df$Diag_cat_factor)[3] / sum(summary(df$Diag_cat_factor)[c(3,1)]))
sens
spec
PPV
NPV

#build the ROC curve <30
roc(df$EGFR30==1 ,as.numeric(df$SUN),auc=TRUE, ci=TRUE, plot=TRUE)

#eGFR<15 mL/min/1.73m^2.
# Building a simulated dataset for eGFR < 15
df <- data.frame('PATIENT_ID' = 1:649, "eGFR15" = rbinom(649, 1, 0.096))  # Assuming a new probability for eGFR < 15
df$eGFR15 <- factor(df$eGFR15)

# Assigning diagnostic categories
df$SUN_diag <- NA
df$SUN_diag[df$eGFR15 == 0] <- rbinom(length(df$eGFR15[df$eGFR15 == 0]), 1, (1 - 0.818))
df$SUN_diag[df$eGFR15 == 1] <- rbinom(length(df$eGFR15[df$eGFR15 == 1]), 1, 0.823)
df$SUN_diag <- factor(df$SUN_diag)

# Calculate sens, spec, PPV, and NPV
TP <- length(df$SUN_diag[df$eGFR15 == 1 & df$SUN_diag == 1])
sens <- as.numeric(TP / sum(df$eGFR15 == 1))
TN <- length(df$SUN_diag[df$eGFR15 == 0 & df$SUN_diag == 0])
spec <- as.numeric(TN / sum(df$eGFR15 == 0))
PPV <- as.numeric(TP / sum(df$SUN_diag == 1))
NPV <- as.numeric(TN / sum(df$SUN_diag == 0))
sens
spec
PPV
NPV

# Build the ROC curve for eGFR < 15
roc(df$eGFR15 == 1, as.numeric(df$SUN), auc = TRUE, ci = TRUE, plot = TRUE)