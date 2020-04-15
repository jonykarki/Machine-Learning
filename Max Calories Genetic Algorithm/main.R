rm(list=ls())

library(genalg)

meals <- data.frame(
  food=c('pie', 'cake', 'ramen', 'poptarts', 'tacos', 'coke', 'bread'),
  calories=c(150,200,75,250,50,100,40),
  cost=c(7,8,1,5,2,3,2)
)
wallet <- 10

chromosome <- c(1,0,0,1,1,0,0)
meals[chromosome == 1,]
cat(chromosome %*% meals$calories)

evalfunc <- function(x){
  current_solution_calories <- x %*% meals$calories
  current_solution_cost <- x %*% meals$cost
  
  if (current_solution_cost > wallet)
    return (0) else return(-current_solution_calories)
}

evalfunc(chromosome)
iter = 100
model <- rbga.bin(size=7, popSize = 200, iters = iter, mutationChance = 0.01, elitism = T, evalFunc = evalfunc)
cat(summary(model))

solution = c(0,0,1,1,0,1,0)
meals[solution==1,]
