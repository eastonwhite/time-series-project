# Created by Easton White
# Created on 17-Jan-2017
# Last edited on 18-Jan-2017

#Statistical analyses of minimum time needed and what it may correlate with
# Should probably do poisson or negative binomial with random effects (species or family)


# Pull in final data set here to be analyzed

# Run simple model
with(trial,summary(glm(min_time_for_power~gen_len+habitat + exploitation + invasive + mass + trophic,family = 'poisson')))

# pull in packages that I had used previously with Cocos project
require(glmmADMB)
require(MuMIn)

CocosData$SiteCode <- as.factor(CocosData$SiteCode)
CocosData$CurrentCode <- as.factor(CocosData$CurrentCode)
pop_info$Binomial <- as.factor(pop_info$Binomial)

model <- glmmadmb(formula=min_time_for_power~gen_len+habitat + exploitation + invasive + mass ,data=pop_info,family="nbinom",zeroInflation=FALSE)


model_ranking <- dredge(model,rank="AIC")
