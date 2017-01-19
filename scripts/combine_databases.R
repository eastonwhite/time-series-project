# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# This is a script to try and combine multiple population series time together to use biological information from Amniote Database in my other analyses

# Population time series file
load('For_easton.Rdata')
dat= dat

# File for biological characteristics of amniotes
amniote = read.table('Amniote_Database_Aug_2015_erw.csv',sep=',',header=T,na.strings = -999)
amniote$Binomial = paste(amniote$genus,amniote$species,sep=' ')

# Create bio_info table to store informatino where a species is present in both databases
bio_info = as.data.frame(matrix(NA,nrow=nrow(dat), ncol=ncol(amniote[,7:35])))
colnames(bio_info)=names(amniote[,7:35])

dat = cbind(dat,bio_info)
dat$match =0

for (j in 1:nrow(dat)){
  if (dat$Binomial[j] %in% amniote$Binomial){
    dat[j,20:48] = amniote[which(amniote$Binomial %in% dat$Binomial[j]),7:35]
    dat[j,49]=1
  }
    if (j %in% seq(1000,80000,by=5000)){print(j)} #just a counter
}

# remove coluoms with lots of NA (egg_width_mm forward) #sort(colSums(is.na(dat)),decreasing=T)
many_NAs=names(sort(colSums(is.na(dat)),decreasing=T))[1:23]
new_dat = dat[,-which(names(dat) %in% many_NAs)]
new_dat = new_dat[,-which(names(new_dat) %in% c('year_ass'))]

test=na.omit(new_dat)
length(table(test$ID)) #number of time series

# Which biological variables are most available for long time series?
long_time_names=names(table(test$ID))[as.numeric(table(test$ID))>25]
long_dat = subset(test,test$ID %in% long_time_names)
length(table(long_dat$ID) )


pop_info= long_dat[1,]
for (j in 1:length(table(long_dat$ID))){
  index=as.numeric(which(long_dat$ID==long_time_names[j])[1])
  pop_info[j,] = long_dat[index,]
}

# All this results in 237 Aves time series and 3 mammal time series