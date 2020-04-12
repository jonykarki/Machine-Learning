rm(list= ls())

library(tree)
setwd("")

data = read.csv("prostate.csv")
# set the max at child to 1 (overfitting)
ptree <- tree(lcavol~., data=data, mincut=1)
ptree

plot(ptree,col=8)
text(ptree, digits=2)

# pruning
p_tree <- prune.tree(ptree,k=1.7)
plot(p_tree)
p_tree

# different k-value
p_tree_2 <- prune.tree(ptree, k=2)
plot(p_tree_2)
p_tree_2

# k=3
p_tree_3 <- prune.tree(ptree, k=8)
p_tree_3
plot(p_tree_3)

p_tre <- prune.tree(ptree)
p_tre
plot(p_tre)

# get the best 3
pb_three <- prune.tree(ptree,best = 3)
plot(pb_three)
pb_three
text(pb_three)

# v-crossfoldvalidate
set.seed(2)
cvpst <- cv.tree(ptree,K=10)
cvpst$size
cvpst$dev
plot(cvpst, pch=21, bg=8, type="p", cex=1.5, ylim=c(65,100))

# 3 seems to be the best
best <- prune.tree(ptree,best=3)
plot(best)
text(best)

# lcp and lpsa are used
# plot the graph with the size indicating value
plot(data[,c("lcp", "lpsa")], cex=0.2*exp(data$lcavol))
# draw the line for the first separation
abline(v=.261624,col=4,lwd=2)
lines(x=c(-2,.261624), y=c(2.30257,2.30257), col=4,lwd=2)
