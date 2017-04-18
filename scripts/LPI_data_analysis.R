# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# This code reads in data from the () and cleans it up (mostly making it into a long table format)

library("tidyr")
library("dplyr")

LPI = read.table('data/LPI_LPR2016data_public_EW_edited.csv',sep=',',header=T)


LPI_long <- LPI %>%
  gather(year, pop_value, starts_with('X'))

LPI_long$year=as.numeric(sub('X','',LPI_long$year))

#head(LPI_long[sort(LPI_long$ID),],50)

newdata=arrange(LPI_long,ID,year)

testing=newdata[,c(1,2,3,10,11,19,20)]
testing[testing=='NULL']=NA
#removed=na.omit(testing)

long_term_series_names=names(sort(table(removed$ID),decreasing=T)[sort(table(removed$ID),decreasing=T)>35])

y=subset(removed,removed$ID %in% long_term_series_names)


for (species_code in long_term_series_names[1]){
  pop = subset(removed,removed$ID==species_code)
  
  
}



# Plots to make
load('example_min_time_LPI_data.Rdata')
par(mfrow=c(1,1))
plot(jitter(pop_info$Latitude,amount=2),pop_info$min_time_for_power,pch=16,col=rgb(0.5,0.5,0.5,0.5),xlim=c(0,80),las=1,ylab='',xlab='',cex.axis=1.2)
mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
mtext(text = 'latitude',side = 1,line = 3,cex = 1.4)

plot(jitter(pop_info$Longitude,amount=2),pop_info$min_time_for_power,pch=16,col=rgb(0.5,0.5,0.5,0.5))

