
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# Calculate time required to be within XX% of the overall slope trend
require(plyr)

#pull in neccessary functions
source('calculate_slope.R')
source('create_subsamples.R')

calculate_average_slopes = function(pop){
  subset_times=create_subsamples(pop)
  average_slopes=matrix(0,nrow=(length(pop)-1),1)
  for (n in 1:(length(pop)-1)){
    average_slopes[n,1]=mean(apply(na.omit(subset_times[[n]]),1,calculate_slope))
  }
  return(average_slopes)
}  





min_time_needed = function(pop, threshold){
  average_slopes = calculate_average_slopes(pop)
  min_value = tail(which(abs((average_slopes - as.numeric(tail(average_slopes,1)))/as.numeric(tail(average_slopes,1)))>threshold),1)+1
  
  if (length(min_value)==0){
    return(2)
  }else if (min_value ==length(pop)){
    return(-999)
  }else{
    return(min_value)
  }
}



##########
# run a loop to find the min time series length rrequrired for each population
mimimum_time_required = matrix(0,nrow=nrow(pop_info),ncol=1)
###run loop to make plot
for (j in names(table(long_dat$ID))){
  
  pop=subset(long_dat,long_dat$ID==j)$popvalue[1:40]/max(subset(long_dat,long_dat$ID==j)$popvalue[1:40])

  mimimum_time_required[which(j==pop_info$ID),1] = min_time_needed(pop,0.05)

  print(paste(which(j==pop_info$ID),j,":",mimimum_time_required[which(j==pop_info$ID),1]))
}
