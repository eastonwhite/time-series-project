# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# This script is designed to calculate the min time required to have 0.X power

# Calculate power for series of subsamples (what fraction of subsamples have trened that is significant - dictated by alpha)
require(plyr)

#pull in neccessary functions
source('scripts/calculate_slope.R')
source('scripts/create_subsamples.R')


#This is a two-sided test (should use 1 tailed test)
# calculate_power_for_subsamples = function(pop,alpha){
#   subset_times=create_subsamples(pop)
#   power=matrix(0,nrow=(length(pop)-1),1)
#   for (n in 1:(length(pop)-1)){
#     power[n,1]=sum(apply(na.omit(subset_times[[n]]),1,calculate_p_value)<alpha)/(length(pop) - n)
#   }
#   return(power)
# }  


######## Implement one-tailed test
calculate_power_for_subsamples = function(pop,alpha){
  subset_times=create_subsamples(pop)
  power=matrix(0,nrow=(length(pop)-1),1)
  for (n in 1:(length(pop)-1)){
    power[n,1]=sum(apply(na.omit(subset_times[[n]]),1,calculate_p_value)<alpha  & sign(apply(na.omit(subset_times[[n]]),1,calculate_slope))==sign(calculate_slope(pop))  )/(length(pop) - n)
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
  }else if (min_value ==length(pop)){
    return(-999)
  }else{
    return(min_value)
  }
}





## OLD CODE BELOW ##

##########
# run a loop to find the min time series length requrired for each population
# mimimum_time_required = matrix(0,nrow=nrow(pop_info),ncol=1)
# ###run loop to make plot
# for (j in names(table(long_dat$ID))){
#   
#   pop=subset(long_dat,long_dat$ID==j)$popvalue[1:40]/max(subset(long_dat,long_dat$ID==j)$popvalue[1:40])
#   mimimum_time_required[which(j==pop_info$ID),1] = min_time_needed(pop,0.05,0.8)
# 
#   print(paste(which(j==pop_info$ID),j,":",mimimum_time_required[which(j==pop_info$ID),1]))
# }



### I should create plotting function here ###
# par(mfrow=c(1,2))
# pop=subset(long_dat,long_dat$ID=='B00412')$popvalue
# 
# plot(pop,ylim=c(0.9*min(pop,na.rm=T),1.1*max(pop,na.rm=T)),pch=16,type='l')
# abline(lm(pop~c(1:length(pop))),col='red',lty=2,lwd=2)
# fraction_correct_sign = calculate_fraction_correct_sign(pop)
# plot(fraction_correct_sign,pch=16,ylim=c(0,1), ylab='Fraction of subsequences that correctly predict longterm trend',type='l')
# abline(h=0.8,lty=2,lwd=3,col='red')
#######



# run a loop to find the min time series length rrequrired for each population
# mimimum_time_required = matrix(0,nrow=nrow(pop_info),ncol=1)
# ###run loop to make plot
# for (j in names(table(long_dat$ID))){
#   
#   pop=subset(long_dat,long_dat$ID==j)$popvalue[1:40]/max(subset(long_dat,long_dat$ID==j)$popvalue[1:40])
#   #plot(pop,ylim=c(0.9*min(pop,na.rm=T),1.1*max(pop,na.rm=T)),pch=16,type='l')
#   #abline(lm(pop~c(1:length(pop))),col='red',lty=2,lwd=2)
#   mimimum_time_required[which(j==pop_info$ID),1] = min_time_needed(pop,0.05)
#   
#   #   par(mfrow=c(1,1))
#   #   points(calculate_fraction_correct_sign(pop),ylim=c(0,1), ylab='Fraction of subsequences that correctly predict longterm trend',type='l',col=rgb(0.5,.5,0.5,alpha=0.3),xlim=c(0,60))
#   #   #abline(h=0.8,lty=2,lwd=3,col='red')
#   print(paste(which(j==pop_info$ID),j,":",mimimum_time_required[which(j==pop_info$ID),1]))
# }