
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# This function simulates a population over time given set of input parameters and max time to be run. Assumes initial population of 1000

simulate_pop = function(r,phi,sigma,max.time){
  pop = matrix(1000,nrow=1,ncol=max.time)
    for (t in 1:(max.time-1)){
      pop[t + 1] = pop[t] + r + phi*rnorm(n = 1,mean=0,sd = sigma) # this is the simple population model, other models can also be used here
    }
  return(c(pop)) #return vector of population sizes
}