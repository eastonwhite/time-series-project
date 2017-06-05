


# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 2-June-2017


# Functions for calculating power metric and minimum time series required for GAM example

# Calculate power for series of subsamples (what fraction of subsamples have trened that is significant - dictated by alpha)
require(plyr)

#pull in neccessary functions
source('scripts/calculate_p_value_gam.R')
source('scripts/create_subsamples.R')


######## Implement two-tailed test
calculate_power_for_subsamples = function(pop,alpha){
  subset_times=create_subsamples(pop)
  power=matrix(0,nrow=(length(pop)-2),1)
  for (n in 3:(length(pop)-2)){
    power[n,1]=sum(apply(na.omit(subset_times[[n]]),1,calculate_p_value_gam)<alpha)/(length(pop) - n)
  }
  return(power)
}  



#######




# give population time series, alpha, and a threshold for power (usually 0.8), calculate the minimum time needed to guarentee time series reaches required levels of alpha and power
min_time_needed = function(pop, alpha,threshold){
  alpha=alpha
  power = calculate_power_for_subsamples(pop,alpha)
  min_value = tail(which(abs((power - as.numeric(tail(power,1)))/as.numeric(tail(power,1)))>(1-threshold)),1)+1
  
  if (length(min_value)==0){
    return(2)
  }else if (min_value ==length(pop)-2){
    return(-999)
  }else{
    return(min_value)
  }
}




