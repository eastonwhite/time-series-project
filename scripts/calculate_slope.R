# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# Here are two functions. The first extracts the slope coefficient for linear regression while the second calculates the p_value

calculate_slope= function(x){
  return(as.numeric(coef(lm(c(x)~c(1:length(x))))[2]))
}


calculate_p_value = function(x){
  return(summary(lm(c(x)~c(1:length(x))))$coefficients[2,4])
}