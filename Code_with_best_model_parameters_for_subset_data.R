#SVM Code tested on subset of the entire Default dataset
#install.packages("Rcpp")
#install.packages("e1071")

#Call Library
library(e1071)
library(dplyr)
library(ISLR)

#Reuse Variables
set.seed(1)

#Read Data
# You can choose the predictors here which you want.
customer_data_subset1 <- subset(Default,select = c("default","student","balance"))
#customer_data_subset1 <- subset(Default,select = c("default","student","income"))
#customer_data_subset1 <- subset(Default,select = c("default","income","balance"))
head(customer_data_subset1)

#Convert target column to factor
customer_data_subset1$default <- as.factor(customer_data_subset1$default)
customer_data_subset1$student <- as.numeric(customer_data_subset1$student)

#Understand the Data
# You can choose to plot the one which you have selected above.
plot(x=customer_data_subset1$student,y=customer_data_subset1$balance,col=customer_data_subset1$default,pch=19)
#plot(x=customer_data_subset1$student,y=customer_data_subset1$income,col=customer_data_subset1$default,pch=19)
#plot(x=customer_data_subset1$income,y=customer_data_subset1$balance,col=customer_data_subset1$default,pch=19)

boxplot(balance~default,data = customer_data_subset1,col=c("blue","red"))

# shuffle
x <- customer_data_subset1[sample(1:nrow(customer_data_subset1)),]

# Create training and testing data
training <- x[1:8000,]
testing <- x[8001:nrow(customer_data_subset1),]

#svmfit=svm(default~.,data = training,kernel="radial",gamma=1,cost=1)
#plot(svmfit,training,svSymbol = 17,dataSymbol = 1)
#points(customer_data_subset1[svmfit$index,c(1,2)],col="blue",cex=2)

#Learn MOdel - Kernel = "Radial"
svmfit_radial=svm( default ~ .,data=training,kernel="radial",gamma=1,cost=10)
plot(svmfit_radial,training,svSymbol = 9,dataSymbol = 10)
#Learn MOdel - Kernel = "Polynomial"
svmfit_poly=svm( default ~ .,data=training,kernel="polynomial",gamma=1,cost=10)
plot(svmfit_poly,training,svSymbol = 17,dataSymbol = 1)
#LEarn Model - Kernel = "Sigmoid"
svmfit_sigmoid=svm( default ~ .,data=training,kernel="sigmoid",gamma=1,cost=10)
plot(svmfit_sigmoid,training,svSymbol = 17,dataSymbol = 1)

#Summary of different Models built above
summary(svmfit_radial)
summary(svmfit_poly)
summary(svmfit_sigmoid)

#Tune takes a lot of time hence commenting out at present
tune.out_radial=tune(svm,default ~ .,data=training,kernel="radial",ranges = list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
tune.out_poly=tune(svm,default ~ .,data=training,kernel="polynomial",ranges = list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
tune.out_sigmoid=tune(svm,default ~ .,data=training,kernel="sigmoid",ranges = list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))

summary(tune.out_radial)
summary(tune.out_poly)
summary(tune.out_sigmoid)

#Find the best model
bestmod_radial=tune.out_radial$best.model
bestmod_poly=tune.out_poly$best.model
bestmod_sigmoid=tune.out_sigmoid$best.model


#Compute the SVM model on testing dataset
newpred_radial=predict(tune.out_radial$best.model,testing)
newpred_poly=predict(tune.out_poly$best.model,testing)
newpred_sigmoid=predict(tune.out_sigmoid$best.model,testing)

#Plot the oncfusion Matrix
#table(pred=newpred,actual=testing$default)

#Append the predicted column to testing dataset
result_radial<- cbind(newpred_radial, data.frame(testing))
result_poly<- cbind(newpred_poly, data.frame(testing))
result_sigmoid<- cbind(newpred_sigmoid, data.frame(testing))

#Calculate Accuracy of the SVM Model

pred_radial <- nrow(result_radial[(result_radial$newpred_radial) == result_radial$default,])/nrow(result_radial)
pred_radial
pred_radial <- pred_radial*100

pred_poly <- nrow(result_poly[(result_poly$newpred_poly) == result_poly$default,])/nrow(result_poly)
pred_poly <- pred_poly*100

pred_sigmoid <- nrow(result_sigmoid[(result_sigmoid$newpred_sigmoid) == result_sigmoid$default,])/nrow(result_sigmoid)
pred_sigmoid<- pred_sigmoid*100

cat("Accuracy_Radial_kernel = ",pred_radial,"\n")
cat("Accuracy_Polynomial_kernel = ",pred_poly,"\n")
cat("Accuracy_Sigmoid_kernel = ",pred_sigmoid,"\n")

#Plot the confusion Matrix
table(prediction=newpred_radial,actual=testing$default)
table(prediction=newpred_poly,actual=testing$default)
table(prediction=newpred_sigmoid,actual=testing$default)