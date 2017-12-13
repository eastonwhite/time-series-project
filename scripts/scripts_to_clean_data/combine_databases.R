# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 10-Jun-2017


# This is a script to try and combine multiple population series time together to use biological information from Amniote Database in my other analyses

require(tidyr)
# Load cleaned population time series database (n=878). This includes a column for min time required to estimate significant slope (alpha<0.05) coefficient for linear regression with power>0.8
load('cleaned-data/cleaned_timeseries_database_with_min_time_linear_regression.Rdata')

# File for biological characteristics of amniotes (where is this from again?)
amniote = read.table('data/Amniote_Database_Aug_2015_erw.csv',sep=',',header=T,na.strings = -999)
amniote$Binomial = paste(amniote$genus,amniote$species,sep=' ')

combined=inner_join(pop_info,amniote[7:36],by='Binomial')

few_NAs=names(sort(colSums(is.na(combined)),decreasing=F))[1:30]


LPI_pop_info = combined[,c(few_NAs)] #number of time series remaining


LPI_pop_info = na.omit(LPI_pop_info)

new_dat = subset(long_dat,long_dat$ID %in% LPI_pop_info$ID)

# Create bio_info table to store information where a species is present in both databases
# bio_info = as.data.frame(matrix(NA,nrow=nrow(dat), ncol=ncol(amniote[,7:35])))
# colnames(bio_info)=names(amniote[,7:35])
# 
# dat = cbind(dat,bio_info)
# dat$match =0 # Create column to see if data matches
# 
# # inefficient way to create matches between the two databases
# for (j in 1:nrow(dat)){
#   if (dat$Binomial[j] %in% amniote$Binomial){
#     dat[j,14:42] = amniote[which(amniote$Binomial %in% dat$Binomial[j]),7:35]
#     dat[j,43]=1
#   }
#     if (j %in% seq(1000,80000,by=5000)){print(j)} #just a counter
# }
# 
# # remove columns with lots of NA (e.g. )
# few_NAs=names(sort(colSums(is.na(dat)),decreasing=F))[1:21]
# new_dat = dat[,which(names(dat) %in% few_NAs)]
# new_dat = new_dat[,1:21]
# 
# new_dat = na.omit(new_dat) #number of time series remaining
# 
# 
# LPI_pop_info = cbind(pop_info, matrix(NA,nrow=nrow(pop_info),ncol=ncol(new_dat[,3:ncol(new_dat)])))
# names(LPI_pop_info) = c(names(pop_info),names(new_dat[,3:ncol(new_dat)]))
# 
# for (j in 1:nrow(pop_info)){
#   if (nrow(subset(new_dat,pop_info$ID[j]==new_dat$ID))>0){
#     pop = subset(new_dat,pop_info$ID[j]==new_dat$ID)
#     LPI_pop_info[j,] = cbind(pop_info[j,],pop[1,3:ncol(pop)])
#   }else{
#     LPI_pop_info[j,] = NA
#   }
# }
# 
# LPI_pop_info = na.omit(LPI_pop_info)

save(LPI_pop_info,new_dat,file = 'cleaned-data/combined_databases_with_min_time_linear_regression.Rdata')




# Simple plots
# if (makePlot=TRUE){
# par(mfrow=c(1,3),oma=c(4,4,1,1),mar=c(1,3,0,0))
# with(LPI_pop_info,plot(gen_len,min_time_for_power,pch=16,col=rgb(0.5,0.5,0.5,0.5),las=1,ylab='',xlab='',cex.axis=1.2))
# mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
# mtext(text = 'generation length (years)',side = 1,line = 3,cex = 1.4)
# 
# with(LPI_pop_info,plot(litter_or_clutch_size_n,min_time_for_power,pch=16,col=rgb(0.5,0.5,0.5,0.5),las=1,ylab='',xlab='',cex.axis=1.2))
# #mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
# mtext(text = 'litter size',side = 1,line = 3,cex = 1.4)
# 
# with(LPI_pop_info,plot(log(adult_body_mass_g),min_time_for_power,pch=16,col=rgb(0.5,0.5,0.5,0.5),las=1,ylab='',xlab='',cex.axis=1.2))
# #mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
# mtext(text = 'log(adult body mass (grams))',side = 1,line = 3,cex = 1.4)
# }
# 
