
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# This function simulates a population over time given set of input parameters and max time to be run. Assumes initial population of 1000

# For presentation, I could leave out autocorrelation results

simulate_pop = function(r,phi,sigma,max.time){
  #set.seed(12345)
  pop = matrix(1000,nrow=1,ncol=max.time)
    for (t in 1:(max.time-1)){
      #pop[t + 1] = pop[t] + rnorm(n = 1,mean=r*phi,sd = sigma)
      #pop[t + 1] = pop[t] + r + phi*rnorm(n = 1,mean=0,sd = sigma) # this is the simple population model, other models can also be used here
      # if (pop[t]>=0){
         #pop[t + 1] = pop[t]*rnorm(1,phi*r - ((sigma^2)/2) , sigma) 
      # }else{
      #   pop[t+1]=0# From Keith et al 2015
      # }
      pop[t + 1] = pop[t] + r + rnorm(n = 1,mean=0,sd = sigma)
    }    
  return(c(pop)) #return vector of population sizes
}


#y=simulate_pop(4,-0.2,.2,100)
##par(mfrow=c(1,2))
#plot(y[1:99],y[2:100])
#plot(y)
