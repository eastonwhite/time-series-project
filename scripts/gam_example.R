

# Example gam
require(gam)
gam.object <- gam(popvalue ~ s(year,3),data=short_moose,family=poisson)
summary(gam.object)
plot(gam.object,se=TRUE)
points(short_moose$year,(short_moose$popvalue-mean(short_moose$popvalue))/mean(short_moose$popvalue),col='red',pch=16)


short_short_moose=short_moose[1:10]

gam.newdata = as.data.frame(c(1950:2020))
names(gam.newdata)='year'
predict(gam.object,type="terms",newdata=gam.newdata)
plot(1950:2020,predict(gam.object,type="terms",newdata=gam.newdata))
