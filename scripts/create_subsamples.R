# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

# this is a function to create a list of all possible sub samples of a time series. For example, a time series of length ten could have 9 possible 2 year continuous subsamples, 8 possible, 3 year continuous subsamples, and so forth until 1 possible 10 year subsample

require(plyr)
create_subsamples = function(pop){
  subsamples=list()
  for (n in 1:(length(pop)-1)){
    subsamples[[n]] = ldply(1:length(pop), function(x){pop[x:(x+n)]})
  }
  return(subsamples)
}  