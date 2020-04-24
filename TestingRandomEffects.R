###### How do raw data relate to RANEF/lme estimates ####
#
include(lme4)

NSubj = 100; NTrials = 100; NSims=1000
dat = expand.grid(subj = 1:NSubj, trial=1:NTrials)
dat$cond = factor(c("Hi", "Lo"))
contrasts(dat$cond)=c(-.5,.5)


### Q1: does LME "regress to the mean" on estimates of random effects? ####
# Q1a: intercepts only ####
Vars1a = data.frame(matrix(NA,nrow=NSims, ncol=3))
Cors1a = data.frame(matrix(NA,nrow=NSims, ncol=3))

for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  
  dat$ranInt = ranInts[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt)
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~1+(1|subj), dat)
  
  Vars1a[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors1a[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
               , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}
colnames(Vars1a)=c("true", "mod", "obs")
colnames(Cors1a)=c("trueVmod", "trueVobs", "modVobs")

plot(Vars1a$true, Vars1a$mod-Vars1a$true, col=ifelse(Vars1a$mod-Vars1a$true<0,1,2), cex=.2)
points(Vars1a$true, Vars1a$obs-Vars1a$true, col=ifelse(Vars1a$obs-Vars1a$true<0,3,4), alpha=.5, cex=.2)
summary(Cors1a)

# Q1b: ... what if there's a "main Effect" embedded, no random slope ####

Vars1b = data.frame(matrix(NA,nrow=NSims, ncol=3))
Cors1b = data.frame(matrix(NA,nrow=NSims, ncol=3))

for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  
  dat$ranInt = ranInts[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+.5*as.numeric(dat$cond))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~cond+(1|subj), dat)
  
  Vars1b[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors1b[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
               , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}
colnames(Vars1b)=c("true", "mod", "obs")
colnames(Cors1b)=c("trueVmod", "trueVobs", "modVobs")
plot(Vars1b$true, Vars1b$mod-Vars1b$true, col=ifelse(Vars1b$mod-Vars1b$true<0,1,2), cex=.2)
points(Vars1b$true, Vars1b$obs-Vars1b$true, col=ifelse(Vars1b$obs-Vars1b$true<0,3,4), alpha=.5, cex=.2)
summary(Cors1b)
# OK, that's actually really good - the model actually does a better job of estimating the true
# random intercepts than using the observed data did.

# Q1c: ... add an (unfitted) random slope that is not correlated with intercepts ####

Vars1c = data.frame(matrix(NA,nrow=NSims, ncol=3))
Cors1c = data.frame(matrix(NA,nrow=NSims, ncol=3))

for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  ranConds = rnorm(NSubj, mean = .5, sd = .2)
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*as.numeric(dat$cond))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~cond+(1|subj), dat)
  
  Vars1c[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors1c[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
               , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}

colnames(Vars1c)=c("true", "mod", "obs")
colnames(Cors1c)=c("trueVmod", "trueVobs", "modVobs")
plot(Vars1c$true, Vars1c$mod-Vars1c$true, col=ifelse(Vars1c$mod-Vars1c$true<0,1,2), cex=.2)
points(Vars1c$true, Vars1c$obs-Vars1c$true, col=ifelse(Vars1c$obs-Vars1c$true<0,3,4), alpha=.5, cex=.2)
summary(Cors1c)

# Q1d: ...and what if we include the random "slope" ####

varDevs = numeric(NSims)
rawVars = numeric(NSims)
intCors = numeric(NSims)
for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  rawVar = var(ranInts)
  ranConds = rnorm(NSubj, mean = .5, sd = .2)
  rawVars[i]=rawVar
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*as.numeric(dat$cond))
  dat.lme = lmer(dv~cond+(cond|subj), dat)
  
  varDevs[i]=var(ranef(dat.lme)$subj)-rawVar
  
  intCors[i] = cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
}; beepr::beep()

plot(rawVars, varDevs, col=ifelse(varDevs<0,1,2))
# Hm. Seems the regression to the mean does get a bit more pronounced.
hist(varDevs/rawVars) # a bit more spread
hist(intCors) # lower correlations
