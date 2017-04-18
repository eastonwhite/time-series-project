# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


#This code loads in data and then creates a table called pop_info which includes population information on each unique time series

# chuck for loading in data, removing NAs, looking at only long time series
# also calculates a pop_info table which includes basic information on each population that I later analyze

# Pull in data
load('data/For_easton.Rdata')

dat=dat[,c(1:7,11)] #
#dat=na.omit(dat) #this doesn't work well

#this subsets data to find long time series (e.g. >40 years)
#dat=subset(dat,dat$class != 'Aves')
long_time_names=names(table(dat$ID))[as.numeric(table(dat$ID))>35]
long_dat = subset(dat,dat$ID %in% long_time_names)
length(table(long_dat$ID) )


#this creates pop_info table for each analyzed pop time series
pop_info= long_dat[1,]
  for (j in 1:length(table(long_dat$ID))){
    index=as.numeric(which(long_dat$ID==long_time_names[j])[1])
    pop_info[j,] = long_dat[index,]
  }

#add variables to pop_info table
pop_info$series_length = as.numeric(table(long_dat$ID))
pop_info$variance=NA
pop_info$overall_trend=NA
pop_info$autocorrelation= NA
pop_info$trend_p_value=NA
pop_info$min_time_for_power=NA
pop_info$pop_value=as.numeric(pop$pop_value)
long_dat$pop_value=long_dat$pop_value
  for (j in 1:nrow(pop_info)){
    pop= subset(long_dat,long_dat$ID==pop_info$ID[j])
    pop$pop_value=as.numeric(pop$pop_value)
    pop$pop_value=pop$pop_value/max(pop$pop_value) #should I standaridize this in a better way
    pop_info$variance[j]= var(pop$pop_value)
    pop_info$overall_trend[j] = calculate_slope(pop$pop_value)
    pop_info$autocorrelation[j] = acf(pop$pop_value,plot = FALSE)$acf[2]
    pop_info$trend_p_value[j]=calculate_p_value(pop$pop_value)
    
    source('scripts/calculate_power_metric.R')
    pop_info$min_time_for_power[j] = min_time_needed(pop$pop_value,0.05,0.8)
    print(paste(j,':',pop_info$min_time_for_power[j],sep=' '))
  }

#IUCN calculation
pop_info$IUCN = 3*pop_info$gen_len
pop_info$IUCN[pop_info$IUCN<10]=10

# Save output here
save(pop_info,long_dat,long_time_names,file = 'pop_info_example_goodNAs.Rdata')

#Only look at populations with certain level of the overall trend
pop_info=pop_info[pop_info$trend_p_value<0.05,]
long_dat=subset(long_dat,long_dat$ID %in% pop_info$ID)
long_time_names=subset(long_time_names,long_time_names %in% pop_info$ID)


