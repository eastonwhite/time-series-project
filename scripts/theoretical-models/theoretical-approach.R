
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# This code calculates the minimum time needed to achieve certain alpha and power for population model with various imput parameter. The end of this script evaluates min time needed for different parameter values (uses lapply to evaluate a number of values)


# simple model: pop[t+1] = pop[t] + beta + phi*rnorm(0,sigma)


min_time_needed= function(phi){
  years_to_sample=seq(10,50,by=1)
  y=lapply(X=years_to_sample,FUN = calculate_power,phi = phi,r=0.5,sigma = 2,trials=1000)
  min_time_needed = years_to_sample[tail(which(y<0.8),1)] + 1
  return(min_time_needed)
}


r_values = seq(0.1,0.9,by=0.1)
zed=lapply(X=r_values,FUN = min_time_needed)
