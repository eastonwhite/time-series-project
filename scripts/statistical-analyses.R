# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 30-Jan-2017

#Statistical analyses of minimum time needed and what it may correlate with
# Should probably do poisson or negative binomial with random effects (species or family)


# Results so far: It appears the Poisson glm does a pretty good job when examining residual plots. Generation length has positive effect size if you take trend, autocorrelation, and variance into account
# Using formula from pg. 218 of Zuur, I can explain about 68% of the variation in mimimum time required

# Pull in final data set here to be analyzed
load('pop_info_example.Rdata')

# Need to look for highly correlated predictor variables and only use those not highly correlated

pop_info$abs_overall_trend = abs(pop_info$overall_trend)
# Simple linear regression
linear_model <- with(pop_info, summary(lm(min_time_for_power~ abs_overall_trend + autocorrelation + variance + gen_len)))

# Plots of residuals

# Simple GLM with poisson error structure
poisson_model <-glm(min_time_for_power~ abs_overall_trend + autocorrelation + variance + gen_len,family = 'poisson',data=pop_info)

# Plots of residuals
EP <- resid(poisson_model,type='pearson')
ED <- resid(poisson_model,type='deviance')
mu <- predict(poisson_model, type='response')
E <- pop_info$min_time_for_power - mu
EP2 <- E/ sqrt(7.630148 *mu)
op <- par(mfrow=c(2,2),mar=c(4,4,0,0))
plot(mu,E,main='Response residuals')
plot(mu, EP, main='Pearson residuas')
plot(mu, EP2, main='Pearson residuals scaled')
plot(mu, ED, main='Deviance residuals')
par(op)

par(mfrow=c(2,2),mar=c(5,5,1,2),oma=c(0,0,0.5,0))
plot(poisson_model,cex.lab=1.2,las=1,cex.axis=1.2,main=' ')


#R^2 calculation (pg. 218 Zuur)
R2 <- 100*(poisson_model$null.deviance - poisson_model$deviance)/(poisson_model$null.deviance)


# GLM with negative binomial error structure
# pull in packages that I had used previously with Cocos project
require(glmmADMB)
require(MuMIn)

#CocosData$SiteCode <- as.factor(CocosData$SiteCode)
#CocosData$CurrentCode <- as.factor(CocosData$CurrentCode)
pop_info$Binomial <- as.factor(pop_info$Binomial)

#model <- glmmadmb(formula=min_time_for_power~gen_len+habitat + exploitation + invasive + mass ,data=pop_info,family="nbinom",zeroInflation=FALSE)
model <- glmmadmb(formula=min_time_for_power~ abs_overall_trend + autocorrelation  + gen_len ,data=pop_info,family="nbinom",zeroInflation=FALSE)

model_nb <- with(pop_info,glm.nb(formula=min_time_for_power~ abs_overall_trend + autocorrelation + variance + gen_len))

model_ranking <- dredge(model,rank="AIC")
