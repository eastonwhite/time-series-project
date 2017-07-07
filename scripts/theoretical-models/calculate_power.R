# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# This function calculates statistical power by simulating a number of populations and determining what fraction of them produce significant slope coefficients from linear regression given the slope coefficient is truely different from zero. It takes population parameters as imputs to simulate the population for a set number of trials

calculate_power = function(r,phi,sigma,trials,max.time){
  
  source('../scripts/calculate_slope.R') # calls another function which calculates the slope and p_value of a linear regression on a time series
  # Parameter values
  trials   = trials
  max.time = max.time
  r        = r
  phi      = phi
  sigma    = sigma
  
  y=replicate(n=trials,simulate_pop(r,phi,sigma,max.time)) # simulate a number of populations with same parameter values
  
  power=sum((apply(y,2,calculate_p_value)<0.05) & sign(apply(y,2,calculate_slope))==sign(r))/trials # evaluates slope coefficients compared to 0.05 (this can be modified)
  # Alternative definition of power which does not require linear regression to have same sign of "true" trend
  # power=sum((apply(y,2,calculate_p_value)<0.05))/trials 
  
  return(power)
}