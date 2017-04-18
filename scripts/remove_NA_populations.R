


# Remove records on interior of time series for each individual population
load('data/For_easton.Rdata')

dat=dat[,c(1:7,11)] #

dat=testing
ID_codes = names(table(dat$ID))
good_rows = NULL

#dat=long_dat

for (j in 2650:length(ID_codes)){
  #pop = subset(dat,dat$ID==ID_codes[j])[1:35,]
  pop = subset(dat,dat$ID==ID_codes[j])
  pop=pop[head(which(is.na(as.numeric(pop$pop_value))==F),1):tail(which(is.na(as.numeric(pop$pop_value))==F),1),]
  if (nrow(pop) == nrow(na.omit(pop)) &nrow(na.omit(pop))>35){
    good_rows=cbind(good_rows,row.names(pop))
  }
  print(j)
}

dat = dat[row.names(dat) %in% c(good_rows),]


c(bad_IDs); length(bad_IDs)
#Removes over 500 populations!!