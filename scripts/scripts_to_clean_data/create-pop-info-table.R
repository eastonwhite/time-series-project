# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


#This code loads in data and then creates a table called pop_info which includes population information on each unique time series

# chuck for loading in data, removing NAs, looking at only long time series
# also calculates a pop_info table which includes basic information on each population that I later analyze

# Pull in original data from Keith et al 2015.
load("data/Keith_et_al_2015_data.Rdata")

dat=dat[,c(1:7,11)] #

# This script removes time series with NAs present. This may take a few moments
source('scripts/scripts_to_clean_data/remove_NA_populations.R')


#this subsets data to find long time series (e.g. >=35 years)
#dat=subset(dat,dat$class != 'Aves')
long_time_names=names(table(dat$ID))[as.numeric(table(dat$ID))>34]
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
pop_info$popvalue=NA
time_series_data=NULL

source('scripts/calculate_slope.R')
source('scripts/calculate_power_metric.R')

for (j in 1:nrow(pop_info)){
    pop= subset(long_dat,long_dat$ID==pop_info$ID[j])
    pop = pop[(nrow(pop)-34):nrow(pop),]
    pop$popvalue=as.numeric(pop$popvalue)
    pop$popvalue=pop$popvalue/max(pop$popvalue) #should I standaridize this in a better way
    pop_info$year[j] = pop$year[1]
    pop_info$variance[j]= var(pop$popvalue)
    pop_info$overall_trend[j] = calculate_slope(pop$popvalue)
    pop_info$autocorrelation[j] = acf(pop$popvalue,plot = FALSE)$acf[2]
    pop_info$trend_p_value[j]=calculate_p_value(pop$popvalue)
    pop_info$popvalue = pop$popvalue[1] #initial abundance
    pop_info$series_length = nrow(pop)
    

     pop_info$min_time_for_power[j] = min_time_needed(pop$popvalue,0.05,0.8)
    print(paste(j,':',pop_info$min_time_for_power[j],sep=' '))
    
    
    time_series_data = rbind(time_series_data,pop)
}

long_dat = time_series_data

#IUCN calculation
pop_info$IUCN = 3*pop_info$gen_len
pop_info$IUCN[pop_info$IUCN<10]=10




##### SAVE OUTPUT HERE ######
save(pop_info[,c(1:13,15)],long_dat,long_time_names,file = 'cleaned-data/cleaned_timeseries_database.Rdata')

#Only look at populations with certain level of the overall trend
pop_info=pop_info[pop_info$trend_p_value<0.05,]
long_dat=subset(long_dat,long_dat$ID %in% pop_info$ID)
long_time_names=subset(long_time_names,long_time_names %in% pop_info$ID)

save(pop_info,long_dat,long_time_names,file = 'cleaned-data/cleaned_timeseries_database_with_min_time_linear_regression.Rdata')
