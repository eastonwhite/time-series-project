
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# This code calculates the fraction of linear regressions for subsamples of a time series which are of the correct long term sign (pos vs neg)

require(plyr)
#repeat slope calculations but only look for positive or negative instead of actual value

#pull in neccessary functions
source('calculate_slope.R')
source('create_subsamples.R')

calculate_fraction_correct_sign = function(pop){
  subset_times=create_subsamples(pop)
  fraction_correct_sign=matrix(0,nrow=(length(pop)-1),1)
  for (n in 1:(length(pop)-1)){
    fraction_correct_sign[n,1]=sum(sign(apply(na.omit(subset_times[[n]]),1,calculate_slope))==sign(calculate_slope(pop))) / (length(pop) - n)
  }
  return(fraction_correct_sign)
}  



min_time_needed = function(pop, threshold){
  fraction_correct_sign = calculate_fraction_correct_sign(pop)
  min_value = tail(which(fraction_correct_sign < threshold),1)+1
  
  if (length(min_value)==0){
    return(2)
  }else if (min_value ==length(pop)){
    return(NA)
  }else{
    return(min_value)
  }
}


##########
# run a loop to find the min time series length rrequrired for each population
mimimum_time_required = matrix(0,nrow=nrow(pop_info),ncol=1)
###run loop to make plot
for (j in names(table(long_dat$ID))){
  
  pop=subset(long_dat,long_dat$ID==j)$popvalue
  
  mimimum_time_required[which(j==pop_info$ID),1] = min_time_needed(pop,0.8)
  
  print(paste(j,":",mimimum_time_required[which(j==pop_info$ID),1]))
}
