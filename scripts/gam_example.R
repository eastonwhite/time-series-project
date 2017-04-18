

# Example gam
gam.object <- gam(popvalue ~ s(year,6),data=moose,family=poisson)
summary(gam.object)
plot(gam.object,se=TRUE)
gam.newdata = as.data.frame(c(1950:2020))
names(gam.newdata)='year'
predict(gam.object,type="terms",newdata=gam.newdata)
plot(1950:2020,predict(gam.object,type="terms",newdata=gam.newdata))
