
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# this code imputes data of a time series between the first and last point for which there is data. Currenly the code runs a loop for the LPI data


library('imputeTS')
# Example of package
#y=na.interpolation(as.numeric(pop$pop_value), option ="spline")
#plotNA.imputations(as.numeric(pop$pop_value),y)
#x <- ts(c(NA,NA,NA,2,3,4,5,6,NA,7,8))
#y=na.interpolation(x)


# For LPI data (This interpolates between all point that have values regardless of number of NAs)

for (index in names(table(testing$ID))[2598:length(names(table(testing$ID)))]){
  aaa=subset(testing,testing$ID==index)
  
  if (sum(is.na(aaa$pop_value))!=length(aaa$pop_value)){
    start=head(which(is.na(aaa$pop_value)==F),1)
    end=tail(which(is.na(aaa$pop_value)==F),1)
  
    pop=as.numeric(aaa$pop_value[start:end])
    #na.interpolation(pop,option='spline')
  
    aaa$pop_value[start:end]=na.interpolation(pop,option='spline')
  
  
  testing[which(testing$ID==index),]=aaa
}

print(index)
}