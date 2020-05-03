# Support Vector Machine

# Clear any existing user-defined objects
rm(list=ls())

# Predict Baseball Runs using singles, doubles, triples, homeruns, walks+hit by pitch

# Setup and connections
library(RMySQL)
mydb = dbConnect(MySQL(), )

# Fetch data
rs = dbSendQuery(mydb, "SELECT R, H-2B-3B-HR 1B, 2B, 3B, HR, BB+HBP FROM Batting WHERE yearID=2017 AND AB>=50")
data = fetch(rs, n=2000)

# Import libraries
library(e1071)
library(Matrix)
library(Metrics) #needed temporarily for rmse. It was accidentally left out of R version 1.1.442

# Setting up training and testing sets
set.seed(1)
ss <- sample(1:2, size=nrow(data), replace=TRUE, prob=c(0.67, 0.33))
train_data <- data[ss==1,]
test_data <- data[ss==2,]
dependent_variable <- train_data[,1]
independent_variables <- train_data[,-1]

# Regression with SVM on the training set
modelsvm = svm(x = independent_variables, y = dependent_variable, type = "eps-regression", kernel = "linear")
summary(modelsvm)

# Prediction on the test set
predsvm = predict(modelsvm, test_data[,-1], decision.values=TRUE)

# Reveal SVM parameters
w = t(modelsvm$coefs) %*% modelsvm$SV
b = modelsvm$rho
RMSEsvm = rmse(predsvm, test_data[,1])

# SVM Predicted Runs = 0.356(1B) + 0.188(2B) + 0.1(3B) + 0.23(HR) + 0.263(BB+HBP) - 0.0030
# RMSE for SVM 6.34

# Run linear regression on training set for comparison
modellm <- lm(train_data[,1] ~ train_data[,2] + train_data[,3] + train_data[,4] + train_data[,5] + train_data[,6])

# Calculate linear regression RMSE on testing set
predlm <- test_data
predlm$predict = 0.309*predlm[,2] + 0.437*predlm[,3] + 1.743*predlm[,4] + 0.687*predlm[,5] + 0.324*predlm[,6] - 1.142
predlm$predminusobssqr = (predlm$predict - predlm$R)^2
RMSElm = sqrt(sum(predlm$predminusobssqr)/NROW(predlm$predminusobssqr))

# lm Predicted Runs = 0.309(1B) + 0.437(2B) + 1.743(3B) + 0.687(HR) + 0.324(BB+HBP) - 1.142
# RMSE for linear regression 6.20 - our SVM model sucks!

# Let's see if tuning the SVM algorithm can improve accuracy
tuneResult <- tune(svm, train_data[,1] ~ train_data[,2] + train_data[,3] + train_data[,4] + train_data[,5] + train_data[,6], data = train_data, ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(2:9)))
RMSEsvmtuned = sqrt(tuneResult$best.performance) # RMSE for tuned SVM 4.552
print(tuneResult)
plot(tuneResult) #darker blue region is better performance if you REALLY want to tune it

# Let's tune it like a piano
tuneResult <- tune(svm, train_data[,1] ~ train_data[,2] + train_data[,3] + train_data[,4] + train_data[,5] + train_data[,6], data = train_data, ranges = list(epsilon = seq(0, 0.2, 0.01), cost = 2^(2:9)))
RMSEsvmtuned = sqrt(tuneResult$best.performance) # RMSE for tuned SVM 4.327
print(tuneResult)
plot(tuneResult) #darker blue region is better performance if you REALLY want to tune it

