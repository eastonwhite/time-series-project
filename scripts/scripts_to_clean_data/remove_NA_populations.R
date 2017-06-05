# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017


# Remove records on interior of time series for each individual population
#load('data/Keith_et_al_2015_data.Rdata')
#dat=dat[,c(1:7,11)] #

ID_codes = names(table(dat$ID))
good_rows = NULL

for (j in 1:length(ID_codes)){
  #pop = subset(dat,dat$ID==ID_codes[j])[1:35,]
  pop = subset(dat,dat$ID==ID_codes[j])
  if (nrow(na.omit(pop))>34){
    first_none_NA = head(which(is.na(as.numeric(pop$popvalue))==F),1)
    last_none_NA = tail(which(is.na(as.numeric(pop$popvalue))==F),1)
    pop=pop[first_none_NA:last_none_NA,]
      if (nrow(pop) == nrow(na.omit(pop))){
        good_rows=cbind(good_rows,row.names(pop))
      }
  }
 # print(j)
}

dat = dat[row.names(dat) %in% c(good_rows),]

