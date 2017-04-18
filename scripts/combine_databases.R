# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# This is a script to try and combine multiple population series time together to use biological information from Amniote Database in my other analyses

# Population time series file
load('For_easton.Rdata')
dat= long_dat

# File for biological characteristics of amniotes
amniote = read.table('data/Amniote_Database_Aug_2015_erw.csv',sep=',',header=T,na.strings = -999)
amniote$Binomial = paste(amniote$genus,amniote$species,sep=' ')

# Create bio_info table to store informatino where a species is present in both databases
bio_info = as.data.frame(matrix(NA,nrow=nrow(dat), ncol=ncol(amniote[,7:35])))
colnames(bio_info)=names(amniote[,7:35])

dat = cbind(dat,bio_info)
dat$match =0

for (j in 1:nrow(dat)){
  if (dat$Binomial[j] %in% amniote$Binomial){
    dat[j,9:37] = amniote[which(amniote$Binomial %in% dat$Binomial[j]),7:35]
    dat[j,38]=1
  }
    if (j %in% seq(1000,80000,by=5000)){print(j)} #just a counter
}

# remove coluoms with lots of NA (egg_width_mm forward) #sort(colSums(is.na(dat)),decreasing=T)
few_NAs=names(sort(colSums(is.na(dat)),decreasing=F))[1:12]
new_dat = dat[,which(names(dat) %in% few_NAs)]
#new_dat = new_dat[,-which(names(new_dat) %in% c('year_ass'))]

test=na.omit(new_dat)
length(table(test$ID)) #number of time series

# Which biological variables are most available for long time series?
long_time_names=names(table(test$ID))[as.numeric(table(test$ID))>30]
long_dat = subset(test,test$ID %in% long_time_names)
length(table(long_dat$ID) )


pop_info= long_dat[1,]
for (j in 1:length(table(long_dat$ID))){
  index=as.numeric(which(long_dat$ID==long_time_names[j])[1])
  pop_info[j,] = long_dat[index,]
}

# All this results in 237 Aves time series and 3 mammal time series
LPI_pop_info=pop_info
LPI_pop_info$min_time_required=NULL
source('scripts/calculate_power_metric.R')
for (q in 1:nrow(LPI_pop_info)){
  pop= subset(long_dat,long_dat$ID==LPI_pop_info$ID[q])
  pop$popvalue=as.numeric(pop$popvalue)
  pop$popvalue=pop$popvalue/max(pop$popvalue) 
  LPI_pop_info$min_time_required[q] = min_time_needed(pop$popvalue,0.05,0.8)
  print(paste(q,':',LPI_pop_info$min_time_required[q],sep=' '))
}



# Simple plots
par(mfrow=c(1,3),oma=c(4,4,1,1),mar=c(1,3,0,0))
with(LPI_pop_info,plot(gen_len,min_time_required,pch=16,col=rgb(0.5,0.5,0.5,0.5),las=1,ylab='',xlab='',cex.axis=1.2))
mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
mtext(text = 'generation length (years)',side = 1,line = 3,cex = 1.4)

with(LPI_pop_info,plot(litter_or_clutch_size_n,min_time_required,pch=16,col=rgb(0.5,0.5,0.5,0.5),las=1,ylab='',xlab='',cex.axis=1.2))
#mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
mtext(text = 'litter size',side = 1,line = 3,cex = 1.4)

with(LPI_pop_info,plot(log(adult_body_mass_g),min_time_required,pch=16,col=rgb(0.5,0.5,0.5,0.5),las=1,ylab='',xlab='',cex.axis=1.2))
#mtext(text = 'minimum time required',side = 2,line = 3,cex = 1.4)
mtext(text = 'log(adult body mass (grams))',side = 1,line = 3,cex = 1.4)


