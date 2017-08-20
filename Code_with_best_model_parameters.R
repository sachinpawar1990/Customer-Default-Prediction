#Install the required Packages
#install.packages("Rcpp")
#install.packages("e1071")

#call Library
library(e1071)
library(dplyr)
library(ISLR)

#Reuse Varaibles
set.seed(1)

#Read Data
customer_data <- Default
head(customer_data)

#Convert target column to factor
customer_data$default <- as.factor(customer_data$default)

# shuffle the data using random samples
x <- customer_data[sample(1:nrow(customer_data)),]

# Create training and testing data
training <- x[1:8000,]
testing <- x[8001:nrow(customer_data),]

#The parameter values used below to train the model are first derived using the tune function 
#which is presented in a separate R file as the code takes a long time to find best model

#Learn Model - Kernel = "Radial"
svmfit_radial=svm( default ~ .,data=training,kernel="radial",gamma=2,cost=1)
#Learn Model - Kernel = "Polynomial"
svmfit_poly=svm( default ~ .,data=training,kernel="polynomial",gamma=0.5,cost=1)
#Learn Model - Kernel = "Sigmoid"
svmfit_sigmoid=svm( default ~ .,data=training,kernel="sigmoid",gamma=0.5,cost=0.1)

#Summary of different Models built above
summary(svmfit_radial)
summary(svmfit_poly)
summary(svmfit_sigmoid)

#Compute the SVM model on testing dataset
newpred_radial=predict(svmfit_radial,testing)
newpred_poly=predict(svmfit_poly,testing)
newpred_sigmoid=predict(svmfit_sigmoid,testing)

#Plot the oncfusion Matrix
#table(pred=newpred,actual=testing$default)

#Append the predicted column to testing dataset
result_radial<- cbind(newpred_radial, data.frame(testing))
result_poly<- cbind(newpred_poly, data.frame(testing))
result_sigmoid<- cbind(newpred_sigmoid, data.frame(testing))

#Calculate Accuracy of the SVM Model

pred_radial <- nrow(result_radial[(result_radial$newpred_radial) == result_radial$default,])/nrow(result_radial)
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