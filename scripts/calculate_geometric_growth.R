


# calculate geometric growth rate

calculate_geometric_growth = function(pop){
  
  return((prod(pop[2:length(pop)]/pop[1:(length(pop)-1)]))^(1/(length(pop)-1)))
  
}


#this gets all plot. Each row output is the slope calculated for each sub sample (e.g. row 1 has X number of slopes for all the subsamples of length X)
calculate_average_geometric_growth = function(pop){
  subset_times=create_subsamples(pop)
  average_slopes=matrix(NA,nrow=(length(pop)-1),ncol=(length(pop)-1))
  for (n in 1:(length(pop)-1)){
    average_slopes[n,1:(length(pop)-n)]=apply(na.omit(subset_times[[n]]),1,calculate_geometric_growth)
  }
  return(average_slopes)
  
}  


pop=0.2*(1:100) +100 + rnorm(100,0,5)
# zed=as.data.frame(calculate_average_geometric_growth(pop))
# 
# matplot(2:100,zed,ylim=range(zed,na.rm=T),pch=16,col=rgb(0.5,0.5,0.5,0.5))
# 
# plot(rep(2,times=99),zed[2,],ylim=range(zed,na.rm=T),xlim=c(1,100),pch=16,col=rgb(0.5,0.5,0.5,0.5))
# for (j in 2:99){
#   
#   plot(rep(j+1,times=99),zed[j,],ylim=range(zed,na.rm=T),xlim=c(1,100),pch=16,col=rgb(0.5,0.5,0.5,0.5))
# }
# 
# 
# saveGIF({
#   #ani.options(nmax = 30)
#   plot(-1,-1,ylim=range(zed,na.rm=T),pch=16,col=rgb(0.5,0.5,0.5,0.5))
# for (i in 1:99){
# for (j in 3:99){
#   plot(j,zed[j,i],ylim=range(zed,na.rm=T),pch=16,col=rgb(0.5,0.5,0.5,0.5))
# }
# }
# 
# }, interval = 0.01, movie.name = "bm_demo.gif", ani.width = 600, ani.height = 600)

