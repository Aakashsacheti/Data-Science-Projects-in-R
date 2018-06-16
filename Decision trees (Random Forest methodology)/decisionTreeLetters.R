

#Application of CART and Random Forests



#packages needed
list.of.packages <- c("caret", "e1071","ggplot2","rpart", "rpart.plot","pROC","ROCR","randomForest","caTools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)

#setting data location
Path<-"C:/Users/aakas/OneDrive/IVY/IVY- R Arpendu/data"
setwd(Path)
getwd()

#loading data
letters<- read.csv("letters_ABPR.csv")
head(letters)
 str(letters)

#checking missing values
data.frame(colSums(is.na(letters)))

#dependent variable
#letter = the letter that the image corresponds to (A, B, P or R) 


#splitting the data by dependent variable (letter)
set.seed(001)
splt = caTools::sample.split(letters$letter, SplitRatio = 0.6)
Train = subset(letters, splt==TRUE)
Test = subset(letters, splt==FALSE)

dim(Train)
dim(Test)

#Applying CART Methodology on Train dataset
cart1<- rpart::rpart (letter~., data=Train, method = "class")
prp(cart1)
cart1


#Checking the accuracy of the model in the train data------------------------------#
predictCART1<-predict(cart1, newdata=Train, type = "class")
table(Train$letter,predictCART1)
(430+391+439+418)/(4+3+36+10+12+47+4+27+12+6+31+0+430+391+439+418)

#Checking the accuracy of the model in the test data------------------------------#
predictCART2<-predict(cart1, newdata=Test, type = "class")
table(Test$letter,predictCART2)
(274+261+283+273)/(1+4+37+9+5+31+6+28+4+6+24+0+274+261+283+273)

#ConfusionMatrix
confusionMatrix(predictCART2,Test$letter)


#--------------------------------------------------------------------------------------------------------#


set.seed(1000) 
spl = sample.split(letters$letter, SplitRatio = 0.6)
train2 = subset(letters, spl==TRUE)
test2= subset(letters, spl==FALSE)

#building Random forest model
PredictForest1<-randomForest(letter~.,data = train2)
PredictForest1

#Checking the accuracy of the model-------------------------------------------#
predForest1<-predict(PredictForest1, newdata=Test, type = "class")
table(Test$letter,predForest1)
(316+300+321+298)/(6+5+316+300+321+298)

#building variable importance chart
vu = varUsed(PredictForest1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(PredictForest1$forest$xlevels[vusorted$ix]), main = "Variable Importance Chart_Splitting")
#Interpretation: Here, 'Yedge' variable is most important in terms of number of splits, 
# Yedge is not absolute important but reletively important in accordance to other variables

#impurity chart shows that which variable reduces the most imputity(gini) 
#so as considered as the most important variable acc to this chart
varImpPlot(PredictForest1, main = "Variable Importance Chart by impurity")



