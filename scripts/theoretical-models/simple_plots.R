
# Created by Easton White
# Created on 3-Feb-2017
# Last edited on 3-Feb-2017

# Plots for theoretical output


#plot for r
par(oma=c(0,0,0,0),mar=c(5,5,0.5,0.5))
plot(r_values,zed,ylab=' ',xlab=' ',type='l',las=1,cex.axis=1.2,lwd=2)
mtext(text = 'rate of increase (r)',side = 1,cex=1.2,line=3)
mtext(text = 'minimum time required',side = 2,cex=1.2,line=3)

# plot for p
plot(1-phi_values,phi_zed,ylab=' ',xlab=' ',type='l',las=1,cex.axis=1.2,lwd=2)
mtext(text = 'autocorrelation (1 - phi)',side = 1,cex=1.2,line=3)
mtext(text = 'minimum time required',side = 2,cex=1.2,line=3)

# plot for variance
plot(sigma_values,zed,ylab=' ',xlab=' ',type='l',las=1,cex.axis=1.2,lwd=2)
mtext(text = 'variability (sigma)',side = 1,cex=1.2,line=3)
mtext(text = 'minimum time required',side = 2,cex=1.2,line=3)