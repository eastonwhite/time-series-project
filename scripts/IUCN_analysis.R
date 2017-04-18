

#IUCN analysis


#extract intercept
calculate_intercept = function(pop){
  return(as.numeric(coef(lm(pop~c(1:length(pop))))[1]))
}


#IUCN_decline = 0.7*pop_info_declining$intercept/pop_info_declining$IUCN


for (j in 1:nrow(pop_info)){
  pop= subset(long_dat,long_dat$ID==pop_info$ID[j])
  pop$popvalue=pop$popvalue/max(pop$popvalue)
  pop_info$intercept[j]=calculate_intercept(pop$popvalue)
  pop_info$IUCN_decline[j]=pop$popvalue[pop_info$IUCN[j]]/pop_info$intercept[j]
}


pop_info_declining = subset(pop_info,pop_info$overall_trend<0)
sum(pop_info_declining$IUCN_decline<0.7,na.rm=T)
# a lot of NAs produced

new=subset(pop_info_declining,pop_info_declining$IUCN_decline<0.7)
sum(new$min_time_for_power>new$IUCN)/nrow(new)


###############
#IUCN analyses
pop_info=pop_info[pop_info$trend_p_value<0.05 & sign(pop_info$overall_trend)==-1,]
long_dat=subset(long_dat,long_dat$ID %in% pop_info$ID)
long_time_names=subset(long_time_names,long_time_names %in% pop_info$ID)


pop=subset(long_dat,long_dat$ID=='10027')
pop=pop$popvalue
summary(lm(pop~c(1:length(pop))))

percent_change = function(pop){
  percent_change=(tail(pop,1) - head(pop,1) )/ head(pop,1)
  return(percent_change)
}



