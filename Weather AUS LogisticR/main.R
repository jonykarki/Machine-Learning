rm(list=ls())

data = read.csv("data.csv")
# change into 0 and 1
data$RainToday <- ifelse(data$RainToday=="Yes",1,0)
data = data[complete.cases(data),]
model = glm(data$RainToday ~ data$MinTemp + data$MaxTemp +data$WindGustSpeed,family=binomial(link='logit'),data=data)
print(model)
data$Prob <- 1/(1+exp(-(1.07287+0.25867*data$MinTemp-0.28250*data$MaxTemp+0.01608*data$WindGustSpeed)))

theta <-0.2
actfunction <- function(x) {
  ifelse(x[25]<=theta, return(0), return(1)) #[3] for column 3, Prob
}
data$Predict <- apply(data, 1, actfunction)

sensitivity <- function(x) {
  if(x[22]==1 && x[26]==1) {return(1)}
  if(x[22]==1 && x[26]==0) {return(0)}
}
data$Sensitivity <- apply(data, 1, sensitivity)
specificity <- function(x) {
  if(x[22]==0 && x[26]==0) {return(1)}
  if(x[22]==0 && x[26]==1) {return(0)}
}
data$Specificity <- apply(data, 1, specificity)
data[data=="NULL"] <- NA
data[["Sensitivity"]] <- as.numeric(data[["Sensitivity"]])
data[["Specificity"]] <- as.numeric(data[["Specificity"]])

# calculate and print the sensitivity and 1-specificity
print(c("Sensitivity:", length(which(data$Sensitivity==1))/length(which(data$Sensitivity>=0))))
print(c("1-Specificity:", 1-length(which(data$Specificity==1))/length(which(data$Specificity>=0))))
