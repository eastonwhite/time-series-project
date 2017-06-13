
# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# This code calculates the minimum time needed to achieve certain alpha and power for population model with various imput parameter. The end of this script evaluates min time needed for different parameter values (uses lapply to evaluate a number of values)


# simple model: pop[t+1] = pop[t] + beta + phi*rnorm(0,sigma)


min_time_needed= function(sigma){
  years_to_sample=seq(2,100,by=1)
  y=lapply(X=years_to_sample,FUN = calculate_power,phi =0.5,r=0.5,sigma = sigma,trials=100)
  min_time_needed = years_to_sample[tail(which(y<0.8),1)] + 1
  return(min_time_needed)
}


r_values = seq(1,3,by=0.5)
phi_values = seq(0.1,0.9,by=0.1)
sigma_values=seq(1,12,by=0.5)
zed=lapply(X=sigma_values,FUN = min_time_needed)
sigma_zed=zed

##########################################################
# Run models from r values and sigma values to make nice plot
parameter_combinations = expand.grid(r_values,5)
parameter_combinations$min_time_required = NA
names(parameter_combinations)= c('r','sigma','min_time_required')

for (i in 1:nrow(parameter_combinations)){
  years_to_sample=seq(2,50,by=1)
  y=lapply(X=years_to_sample,FUN = calculate_power,phi =0.5,r=parameter_combinations$r[i],sigma = parameter_combinations$sigma[i],trials=50)
  if (length(tail(which(y<0.8),1))>0){
  parameter_combinations$min_time_required[i] = years_to_sample[tail(which(y<0.8),1)] + 1
  }
  print(i)
}


require(ggplot2)
ggplot(parameter_combinations, aes(x = r, y = sigma, z = min_time_required, fill=min_time_required)) +
  geom_tile() + 
  scale_fill_distiller(palette="Spectral", na.value="white") +
  theme_bw()
#geom_contour(color = "white", alpha = 0.5) + 



# Simple plot
par(mfrow=c(1,2),oma=c(3,0.3,1,1),mar=c(2,6,0,0))
plot(r_values,as.numeric(r_zed),las=1,pch=16,cex.axis=1.2,ylab='',xlab='')
mtext(text = 'trend strength (r)',1,cex = 1.4,line=3)
mtext(text = 'minimum time required',2,cex = 1.4,line=3)

plot(sigma_values,as.numeric(sigma_zed),las=1,pch=16,cex.axis=1.2,ylab='',xlab='')
mtext(text = 'variability (sigma)',1,cex = 1.4,line=3)
mtext(text = 'minimum time required',2,cex = 1.4,line=3)
