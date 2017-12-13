


percent_change = function(time_series,num_years){
  
  model_estimates = coef(lm(time_series~c(1:length(time_series))))
  prediction = as.numeric(model_estimates[1] + model_estimates[2]*num_years)
  
  percent_change = abs(100 - as.numeric(100*prediction/model_estimates[1]))
  
  return(percent_change)
  
}

#plot(IUCN$min_time_for_power,IUCN$IUCN,ylim=c(0,40),xlim=c(0,40),pch=16)
#segments(0,0,100,100)
