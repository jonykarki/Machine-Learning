rm(list=ls())
#install.packages('neuralnet')
library(neuralnet)
#data <- infert
#library(xtable)
#print(xtable(head(data)), type='html')

sample_size = round(nrow(infert)*.90) 
index <- sample(seq_len(nrow(infert)), size = sample_size)

train <- infert[index, ]
test <- infert[-index, ]

nn <- neuralnet(
  case ~ age + parity + induced + spontaneous,
  data = train, hidden=2, err.fct='ce',
  linear.output = FALSE
)
plot(nn)
nn$result.matrix

#pred = ifelse(compute(nn,test)$net.result>0.5, 1, 0)
pred = ifelse(compute(nn,test)$net.result>0.5, 1, 0)
pred
final = data.frame("pred"= pred, "actual"= test$case)
