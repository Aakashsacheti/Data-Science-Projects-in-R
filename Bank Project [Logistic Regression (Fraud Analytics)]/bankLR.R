list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych) #for calculating psedeuo Rsq
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)




Path<-"C:/Users/aakas/OneDrive/IVY/IVY- R Arpendu/data"

setwd(Path)
getwd()


data<- read.csv("Data_for_Logistic_Regression.csv")
str(data)
summary(data)

colnames(data)[colnames(data) == "Default_On_Payment"]<-'defaul'

#missing value check
data.frame(colSums(is.na(data)))
#no missing values

#dividing data in numeric and catagorical 
bankdata<-data
bankdata$defaul<-as.factor(bankdata$defaul)

num <- bankdata[,c(1,3,6,9,12,14,17,19,22,23)]#Numerical Data Frame
cat <- bankdata [,-c(1,3,6,9,12,14,17,19,23)]
str(num)
str(cat)



# GETTING INFORMATION vALUES (IV) FOR NUMERIC DATA
IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- Hmisc::cut2(data[,variable],g=groups)
  tableOutput <- sqldf::sqldf (sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf::sqldf ("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCal("Duration_in_Months","defaul",num,groups=10)
a2<- IVCal("Credit_Amount","defaul",num,groups=10)
a3<- IVCal("Inst_Rt_Income","defaul",num,groups=10)
a4<- IVCal("Current_Address_Yrs","defaul",num,groups=10)
a5<- IVCal("Age","defaul",num,groups=10)
a6<- IVCal("Num_CC","defaul",num,groups=10)
a7<- IVCal("Dependents","defaul",num,groups=10)
a8<- IVCal("Count", "defaul", num, groups = 10)


IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8))
IV_num

#IV calculation of catagorical variables

CA<-function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}


str(cat)
A<- CA("defaul","Status_Checking_Acc",cat)
B<- CA("defaul","Credit_History",cat)
C<- CA("defaul","Purposre_Credit_Taken",cat)
D<- CA("defaul","Savings_Acc",cat)
E<- CA("defaul","Years_At_Present_Employment",cat)
F<- CA("defaul","Marital_Status_Gender",cat)
G<- CA("defaul","Other_Debtors_Guarantors",cat)
H<- CA("defaul","Property",cat)
I<- CA("defaul","Other_Inst_Plans",cat)
J<- CA("defaul","Housing",cat)
K<- CA("defaul","Job",cat)
L<- CA("defaul","Telephone",cat)
M<- CA("defaul","Foreign_Worker",cat)

IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M))
IV_cat

Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV
write.csv(Final_IV,"BankLrFinal_IV.csv")

#Splitting the data into training and test data set
set.seed(144)
spl = sample.split(bankdata$defaul, 0.7)
data.train = subset(bankdata, spl == TRUE)
data.test = subset(bankdata, spl == FALSE)


#Logistic Regression Model Building
model <- glm(defaul~., data=data.train, family=binomial())
summary(model)


#removing insignificant variables
modelx<- glm(defaul~     Status_Checking_Acc + 
              I(Credit_History=="A32") + I(Credit_History=="A33") +I(Credit_History=="A34") +
              I(Purposre_Credit_Taken=="A41") + I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42") +
              I(Purposre_Credit_Taken=="A43") + I(Purposre_Credit_Taken=="A48") + I(Purposre_Credit_Taken=="A49") + 
              Credit_Amount + 
              I(Savings_Acc=="A62") + I(Savings_Acc=="A64") + I(Savings_Acc=="A65") +
              I(Years_At_Present_Employment=="A73") + I(Years_At_Present_Employment=="A74") + I(Years_At_Present_Employment=="A75") +
              Inst_Rt_Income + 
              I(Marital_Status_Gender=="A93") + I(Other_Debtors_Guarantors=="A102") + I(Other_Debtors_Guarantors=="A103")+
              I(Property=="A124") + Age + I(Other_Inst_Plans=="A143") + I(Housing=="A152") + I(Housing=="A153") + Num_CC +
              I(Job=="A172") + I(Job=="A173") +  I(Telephone=="A192") + Foreign_Worker + Count 
              , data=data.train, family=binomial () )
summary(modelx)

#removing insignificant variables
modelx1<- glm(defaul~     Status_Checking_Acc + 
               I(Credit_History=="A32") + I(Credit_History=="A33") +I(Credit_History=="A34") +
               I(Purposre_Credit_Taken=="A41") + I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42") +
               I(Purposre_Credit_Taken=="A43") + I(Purposre_Credit_Taken=="A48") + I(Purposre_Credit_Taken=="A49") + 
               Credit_Amount + 
               I(Savings_Acc=="A62") + I(Savings_Acc=="A64") + I(Savings_Acc=="A65") +
               I(Years_At_Present_Employment=="A74") + I(Years_At_Present_Employment=="A75") +
               Inst_Rt_Income + 
               I(Marital_Status_Gender=="A93") +I(Other_Debtors_Guarantors=="A103")+
               I(Property=="A124") + I(Property=="A123")  + Age + I(Other_Inst_Plans=="A143") + 
               I(Housing=="A152") + I(Housing=="A153") + Num_CC +
               I(Job=="A172") + I(Job=="A173") +  I(Telephone=="A192") + Foreign_Worker + Count
             , data=data.train, family=binomial () )
summary(modelx1)

#removng insignificant variables
modelx2<- glm(defaul~     Status_Checking_Acc + 
                I(Purposre_Credit_Taken=="A41") + I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42") +
                I(Purposre_Credit_Taken=="A43") + I(Purposre_Credit_Taken=="A48") + I(Purposre_Credit_Taken=="A49") + 
                Credit_Amount + 
                I(Savings_Acc=="A62") + I(Savings_Acc=="A64") + I(Savings_Acc=="A65") +
                I(Years_At_Present_Employment=="A74")+
                Inst_Rt_Income + 
                I(Marital_Status_Gender=="A93") +I(Other_Debtors_Guarantors=="A103")+
                Age + I(Other_Inst_Plans=="A143") + 
                I(Housing=="A152")+
                I(Telephone=="A192") + Foreign_Worker
              , data=data.train, family=binomial () )
summary(modelx2)



#Checking ulticolienearity by VIF
vif(modelx2)
install.packages("lava", dependencies = TRUE)

#applying Score tests
#wald test (to chexk weather all the B-coefficients are 0 or not)
wald.test(b=coef(modelx2), Sigma= vcov(modelx2), Terms=1:19)


#Lagrange Multiplier/ Score Test (to find out weather the current variables significantly improves the model fit or not)
# Difference betweene null deviance and deviance
modelscore <- modelx2$null.deviance - modelx2$deviance
modelscore

#Finding the degree of freedom for Null model and model with variables bw null model and devience model.
#null model is a baseline model built on intercept and with no independent variables, davience model is built on significant independent
#variables
chidf <- modelx2$df.null - modelx2$df.residual
chidf

# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.(model is much better than my null model wch have only the intercept term)
chisq.prob <- 1 - pchisq(modelscore, chidf)
chisq.prob
format(round(chisq.prob, 2), nsmall = 3)

#Predicting power of the model using R2----------values bw 0.2 to 0.4 considered good------------------#
  PseudoR2(modelx2)
 

#--------------------Lackfit Deviance test similar to hoshmer
residuals(modelx2) # deviance residuals
residuals(modelx2, "pearson") # pearson residuals

sum(residuals(modelx2, type = "pearson")^2)
deviance(modelx2)

#########Larger p value indicate good model fit
1-pchisq(deviance(modelx2), df.residual(modelx2))
#should be 1
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies




################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## This is not a very useful test. Some authors have suggested that sometimes it produces wrong result##
## High p value incidates the model fits well


# Hosmer and Lemeshow test 


hl <- hoslem.test(as.integer(data.train$defaul), fitted(modelx2), g=10)
hl


#####################################################################################################################
# Coefficients (Odds)
modelx2$coefficients
# Coefficients (Odds Ratio)
exp(modelx2$coefficients)

install.packages("lava", dependencies = TRUE)
# Variable Importance of the model
VarImp(modelx2)

# Predicted Probabilities
prediction <- predict(modelx2,newdata = data.train,type="response")
prediction

write.csv(prediction,"banklrtrainpred.csv")


rocCurve   <- roc(response = data.train$defaul, predictor = prediction, 
                  levels = rev(levels(data.train$defaul)))
data.train$defaul <- as.factor(data.train$defaul)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$defaul)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric
Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$defaul)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7

############################################################################################################


###################### Residual Analysis ################################################################################


logistic_data <- data.train

logistic_data$predicted.probabilities<-fitted(modelx2)

logistic_data$standardized.residuals<-rstandard(modelx2)
logistic_data$studentized.residuals<-rstudent(modelx2)
logistic_data$dfbeta<-dfbeta(modelx2)
logistic_data$dffit<-dffits(modelx2)
logistic_data$leverage<-hatvalues(modelx2)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, "Resbanklr.csv")




###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################




# Logistic Regression on full data


modelt1 <-glm(defaul~     Status_Checking_Acc + 
                I(Purposre_Credit_Taken=="A41") + I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42") +
                I(Purposre_Credit_Taken=="A43") + I(Purposre_Credit_Taken=="A48") + I(Purposre_Credit_Taken=="A49") + 
                Credit_Amount + 
                I(Savings_Acc=="A62") + I(Savings_Acc=="A64") + I(Savings_Acc=="A65") +
                I(Years_At_Present_Employment=="A74")+
                Inst_Rt_Income + 
                I(Marital_Status_Gender=="A93") +I(Other_Debtors_Guarantors=="A103")+
                Age + I(Other_Inst_Plans=="A143") + 
                I(Housing=="A152")+
                I(Telephone=="A192") + Foreign_Worker
              , data=data.test, family=binomial () )
summary(modelt1)


modelt2 <- glm(defaul~     Status_Checking_Acc + 
                 I(Purposre_Credit_Taken=="A41") + I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42") +
                 I(Purposre_Credit_Taken=="A43") + I(Purposre_Credit_Taken=="A49") + 
                 Credit_Amount + 
                 I(Savings_Acc=="A64") + I(Savings_Acc=="A65") +
                 I(Years_At_Present_Employment=="A74")+
                 Inst_Rt_Income + 
                 I(Marital_Status_Gender=="A93") +I(Other_Debtors_Guarantors=="A103")+
                 Age + I(Other_Inst_Plans=="A143") + 
                 I(Housing=="A152")+
                 I(Telephone=="A192") + Foreign_Worker
               , data=data.train, family=binomial () )
summary(modelt2)




vif(modelt2)

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt2$null.deviance - modelt2$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt2$df.null - modelt2$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/modelt2$null.deviance
R2.hl


# Cox and Snell R Square (the last number; here is 2000 should be total no. of ovservation)

R.cs <- 1 - exp ((modelt2$deviance - modelt2$null.deviance) /4000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(modelt2$null.deviance/4000))))
R.n



######### Lackfit Deviance ######################################################
residuals(modelt2) # deviance residuals
residuals(modelt2, "pearson") # pearson residuals

sum(residuals(modelt2, type = "pearson")^2)
deviance(modelt2)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt2), df.residual(modelt2))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## High p value incidates the model fits well

hosmerlem(y = data.test$defaul, yhat = fitted(modelt2))

################################################################################################################
# Hosmer and Lemeshow test in a different way
## High p value incidates the model fits well

library(ResourceSelection)
hl <- hoslem.test(data.test$defaul, fitted(modelt2), g=20)
hl
#####################################################################################################################
# Coefficients (Odds)
modelt2$coefficients
# Coefficients (Odds Ratio)
exp(modelt2$coefficients)

# Predicted Probabilities
prediction <- predict(modelt2,newdata = data.test,type="response")
prediction

write.csv(prediction, "banklrtestpred.csv")


rocCurve   <- roc(response = data.test$defaul, predictor = prediction, 
                  levels = rev(levels(data.test$defaul)))
data.test$defaul<- as.factor(data.test$defaul)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$defaul)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.test$m1.yhat <- predict(modelt2, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$defaul)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################

#########################################################################################################################
###################### Residual Analysis ################################################################################


logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(modelt)
logistic_data$standardized.residuals<-rstandard(modelt)
logistic_data$studentized.residuals<-rstudent(modelt)
logistic_data$dfbeta<-dfbeta(modelt)
logistic_data$dffit<-dffits(modelt)
logistic_data$leverage<-hatvalues(modelt)

#logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
#write.csv(logistic_data, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")




#######################################################################################################

##########################################



