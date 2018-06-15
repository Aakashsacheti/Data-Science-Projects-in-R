#Building a model to predict the monthly sales of the Hyundai Elantra in the United States#

Path<-setwd("C:/Users/aakas/OneDrive/IVY/IVY- R Arpendu/data")
Path
install.packages("dplyr")
elantra<-read.csv("elantra.csv")
View(elantra)
str(elantra)

#converting month in factor
#elantra$Month <- as.factor(elantra$Month)

#Renaming the Dependent var
colnames(elantra)[which(names(elantra)=="ElantraSales")]="sales"

#checking outliers
quantile(elantra$sales, c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1))
#no outliers present in data

#checking missing values
as.data.frame(colSums(is.na(elantra)))
#no missing values

#writing the csv
write.csv(elantra, "myelantra")

#splitting the data based on years for 2012 and earlier in the training set,
#and all observations for 2013 and 2014 into the testing set.

train.el <- dplyr::filter(elantra, Year <= 2012)
test.el <- dplyr::filter(elantra, Year >= 2013)


#getting the structure of training dataset
str(train.el)
#counting number of observations in training data set
nrow(train.el)

#building a leniar model with all variables
LinearModel0=lm(sales~., data = train.el)
summary (LinearModel0)
car::vif(LinearModel0)

#Building a linear regression model to predict monthly Elantra sales using 
#Unemployment, CPI_all, CPI_energy and Queries as the independent variables. 
#in training set data.
LinearModel1=lm(sales~Unemployment+ CPI_all+ CPI_energy+ Queries, data = train.el	)
summary(LinearModel1)
car::vif(LinearModel1)


#taking month as variable
LinearModel2=lm(sales~Unemployment+ CPI_all+ CPI_energy+ Queries + Month, data = train.el	)
summary(LinearModel2)


#taking "month" as variable (but as factor**)
train.el$Month<-as.factor(train.el$Month)
linearmodel3 <- lm(sales~Unemployment + Queries + CPI_energy + CPI_all + Month, data=train.el)
summary(linearmodel3)

#removing the redundant variable
linearmodel4<- lm(sales~Unemployment + CPI_energy + CPI_all + Month, data=train.el)
summary(linearmodel4)
car::vif(linearmodel4)

#removing the redundant months
linearmodel5 <-lm(sales~  + CPI_energy + I(Month == 3)+ I(Month == 4)+ I(Month == 5)
                  + I(Month == 6)+ I(Month == 7)+ I(Month == 8), data = train.el)
summary(linearmodel5)

#getting the predicted values for training data
fitted(linearmodel5)
par(mfrow=c(2,2))
plot(linearmodel5)

#fitting the predicted values in training dataset
train.el$pred<- fitted(linearmodel5)
write.csv(train.el, "train_el.csv")

pred.train<- read.csv("train_el.csv")
plot(pred.train$sales,pred.train$pred )

#Calculating MAPE
attach(pred.train)
MAPE<-print((sum((abs( sales - pred))/sales))/nrow(pred.train))

#------------------------------------------Testing the data-----------------------------------------------

#applying the model on test data
View(test.el)
str(test.el)

#converting Month to factor
test.el$Month<-as.factor(test.el$Month)

testmodel<-lm(sales~  + CPI_energy + I(Month == 3)+ I(Month == 4)+ I(Month == 5)
              + I(Month == 6)+ I(Month == 7)+ I(Month == 8), data = test.el)
summary(testmodel)

#getting the predicted values for testing data
fitted(testmodel)
par(mfrow=c(2,2))
plot(testmodel)

#fitting the predicted values in testing data set
test.el$pred<- fitted(testmodel)
write.csv(test.el, "test prediction.csv")
pred.test<-read.csv("test prediction.csv")

#alternate way of getting predicted values
fit1 <- predict(linearmodel5, newdata=test.el)
View(fit1)



#plotting the predicted variable
plot(pred.test$sales,pred.test$pred )


#Calculating MAPE
attach(pred.test)
MAPE<-print((sum((abs( sales - pred))/sales))/nrow(pred.test))

#vif
car::vif(testmodel)

#getting highest error in testing model
test.el[which.max(abs(test.el$sales-test.el$pred)),]

