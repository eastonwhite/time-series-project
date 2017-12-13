


# Statistical analyses

#bring in data frame
test = test
pop_info_test=pop_info



# Check for highly correlated traits

# Simple GLM with poisson error structure
poisson_model <-glm(min_time_for_power~ abs(overall_trend) + coefficient_variation + litter_or_clutch_size_n  + gen_len + adult_body_mass_g + maximum_longevity_y  +incubation_d + trophic,family = 'poisson',data=LPI_pop_info)

# Plots of residuals
EP <- resid(poisson_model,type='pearson')
ED <- resid(poisson_model,type='deviance')
mu <- predict(poisson_model, type='response')
E <- LPI_pop_info$min_time_for_power - mu
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
LPI_pop_info$Binomial <- as.factor(LPI_pop_info$Binomial)

#model <- glmmadmb(formula=min_time_for_power~gen_len+habitat + exploitation + invasive + mass ,data=pop_info,family="nbinom",zeroInflation=FALSE)
model <- glmmadmb(formula = min_time_for_power ~ abs(overall_trend) + coefficient_variation + litter_or_clutch_size_n  + gen_len + adult_body_mass_g + maximum_longevity_y  +incubation_d ,data=LPI_pop_info,family="poisson",zeroInflation=FALSE)

model_nb <- with(pop_info,glm.nb(formula=min_time_for_power~ gen_len + litter_or_clutch_size_n + litters_or_clutches_per_y + adult_body_mass_g+ maximum_longevity_y + egg_mass_g +incubation_d))

model_ranking <- dredge(model,rank="AIC")
