rm(list=ls())

library(MASS)
library(tree)
iris

iristree <- tree(Species~., data=iris)

# display the tree
iristree
plot(iristree)
plot(iristree,col=8)
text(iristree,digits=2)

levels(iris$Species)

# display the summary of the data
# misclassification rate of 2.667%
summary(iristree)

# remove the splits with identical classes on node 12 and 7
irisremove = snip.tree(iristree, nodes=c(7,12))
plot(irisremove)
text(irisremove)
