# This program uses the tune function which uses the Grid-Search method to choose 
# the optimal values of the various parameters required for the tuning.
# With the usage of the tune function, we don't have to manually choose the value
# and check the accuracy.

#install.packages("Rcpp")
#install.packages("e1071")

#Call Library
library(e1071)
library(dplyr)
library(ISLR)

#Reuse VAraibles
set.seed(1)

#Read Data
customer_data <- Default
head(customer_data)

#Convert target column to factor
customer_data$default <- as.factor(customer_data$default)

# Shuffle
x <- customer_data[sample(1:nrow(customer_data)),]

# Create training and testing data
training <- x[1:8000,]
testing <- x[8001:nrow(customer_data),]

#Tune Function used to calculate the optimal values of cost and gamma
tune.out_radial=tune(svm,default ~ .,data=training,kernel="radial",ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
tune.out_poly=tune(svm,default ~ .,data=training,kernel="polynomial",ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
tune.out_sigmoid=tune(svm,default ~ .,data=training,kernel="sigmoid",ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))

#Find the best model
bestmod_radial=tune.out_radial$best.model
bestmod_poly=tune.out_poly$best.model
bestmod_sigmoid=tune.out_sigmoid$best.model

#Display the optimal parameters to be selected for training and testing of the SVM Model
tune.out_radial$best.model
tune.out_poly$best.model
tune.out_sigmoid$best.model

#Next steps are performed in a separate R code to achieve high response time of the code.
#Code_with_best_model_paramters.R