# required libraries
#install the older version of textir
#package_url <- "https://cran.r-project.org/src/contrib/Archive/textir/textir_1.8-8.tar.gz"
#install.packages(package_url, repos=NULL, type='source', dependencies = T)

library(textir)
library(MASS)

# the forensic glass data
data(fgl)

#set the grid size
par(mfrow=c(3,3), mai=c(.3,.6,.1,.1))

# boxplots showing how the various classes differ w.r.t glass type
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6))
plot(K ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ca ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Fe ~ type, data=fgl, col=c(grey(.2),2:6))

# Use only Al and RI plane for classification as the have most separation comparatively
# total data
n = length(fgl$type)
# total training data 
nt = 200
# set seed for reproducing
set.seed(7)

# take nt random samples from n total data without replacement
train <- sample(n, nt)

# As the units of Al(col 4) and Ri(col 1) are different we standarize them
# we use the normalize function
x <- scale(fgl[,c(1,4)])
x[1:3,]

# classification library
library(class)

# returns the classification of the train data
nearest1 <- knn(train=x[train,], test=x[-train,], cl=fgl$type[train],k=1)
nearest5 <- knn(train=x[train,], test=x[-train,], cl=fgl$type[train], k=5)

# create dataframe with both
data.frame(fgl$type[-train],nearest1,nearest5)

# plots
par(mfrow=c(1,2))
# Plot for k=1
plot(x[train,], col=fgl$type[train], cex=.8, main = "K=1")
points(x[-train,], bg=nearest1, pch=25, col=grey(.9), cex=1.25)

# plot for k = 5
plot(x[train,],col=fgl$type[train], cex=.8, main="K=5")
points(x[-train,], bg=nearest5, pch=25, col=grey(.9), cex=1.25)
legend("topright", legend=levels(fgl$type), fill=1:5, bty="n", cex=.75)

# proportions of correct classification
pcorrn1 = 100 * sum(fgl$type[-train]==nearest1)/(n-nt)
pcorrn2 = 100 * sum(fgl$type[-train]==nearest5)/(n-nt)
nearest6 <- knn(train=x[train,], test=x[-train,], cl=fgl$type[train], k=6)
pcorrn6 = 100 * sum(fgl$type[-train]==nearest6)/(n-nt)
pcorrn1
pcorrn2
pcorrn6


# cross-validation to choose the k value
pcorr = dim(10)
for (k in 1:10){
  pred = knn.cv(x,fgl$type,k)
  pcorr[k] = 100*sum(fgl$type==pred)/n
}
pcorr


# using all glasses
x <- scale(fgl[,c(1:9)])
x

nearest1 = knn(train=x[train,], test=x[-train,], cl=fgl$type[train], k=1)
nearest5 = knn(train=x[train,], test=x[-train,], cl=fgl$type[train], k=5)
data.frame(fgl$type[-train],nearest1,nearest5)

pcorrn1 = 100*sum(fgl$type[-train]==nearest1)/(n-nt)
pcorrn1
pcorrn2 = 100*sum(fgl$type[-train]==nearest5)/(n-nt)
pcorrn2

# cross-validation
pcoor = dim(10)
for (k in 1:10){
  pred = knn.cv(x,fgl$type,k)
  pcorr[k] = 100*sum(fgl$type==pred)/n
}
pcorr
