###### How do raw data relate to RANEF/lme estimates ####
#
include(lme4)
include(mvtnorm)
include(statmod)

NSubj = 100; NTrials = 50; NSims=1000
dat = expand.grid(subj = 1:NSubj, trial=1:NTrials, cond=c("Hi", "Lo"))
dat$cond = factor(dat$cond)
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

# plot(Vars1a$true, Vars1a$mod-Vars1a$true, col=ifelse(Vars1a$mod-Vars1a$true<0,1,2), cex=.2)
# points(Vars1a$true, Vars1a$obs-Vars1a$true, col=ifelse(Vars1a$obs-Vars1a$true<0,3,4), cex=.2)
summary(Cors1a)

# Q1b: ... what if there's a "main Effect" embedded, no random slope ####

Vars1b = data.frame(matrix(NA,nrow=NSims, ncol=3))
Cors1b = data.frame(matrix(NA,nrow=NSims, ncol=3))

for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  
  dat$ranInt = ranInts[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+.5*if_else(dat$cond=="Hi", -.5, .5))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~cond+(1|subj), dat)
  
  Vars1b[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors1b[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}
colnames(Vars1b)=c("true", "mod", "obs")
colnames(Cors1b)=c("trueVmod", "trueVobs", "modVobs")
# plot(Vars1b$true, Vars1b$mod-Vars1b$true, col=ifelse(Vars1b$mod-Vars1b$true<0,1,2), cex=.2)
# points(Vars1b$true, Vars1b$obs-Vars1b$true, col=ifelse(Vars1b$obs-Vars1b$true<0,3,4), cex=.2)
summary(Cors1b)
# OK, so once again, the two produce basically identical values, just shifted slightly.

# Q1c: ... add an (unfitted) random slope that is not correlated with intercepts ####

Vars1c = data.frame(matrix(NA,nrow=NSims, ncol=3))
Cors1c = data.frame(matrix(NA,nrow=NSims, ncol=3))

for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  ranConds = rnorm(NSubj, mean = .5, sd = .2)
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*if_else(dat$cond=="Hi", -.5, .5))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~cond+(1|subj), dat)
  
  Vars1c[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors1c[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}

colnames(Vars1c)=c("true", "mod", "obs")
colnames(Cors1c)=c("trueVmod", "trueVobs", "modVobs")
# plot(Vars1c$true, Vars1c$mod-Vars1c$true, col=ifelse(Vars1c$mod-Vars1c$true<0,1,2), cex=.2)
# points(Vars1c$true, Vars1c$obs-Vars1c$true, col=ifelse(Vars1c$obs-Vars1c$true<0,3,4), cex=.2)
summary(Cors1c)
# Once again, identical measures.

# Q1d: ...and what if we include the random "slope" ####

Vars1d = data.frame(matrix(NA,nrow=NSims, ncol=6))
Cors1d = data.frame(matrix(NA,nrow=NSims, ncol=9))
# Cors1d2 = Cors1d
for(i in 1:NSims){
  ranInts = rnorm(NSubj)
  ranConds = rnorm(NSubj, mean = .5, sd = .2)
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*if_else(dat$cond=="Hi", -.5, .5))
  # dat$dv2 = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*if_else(dat$cond=="Hi", -.5, .5))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  # datInts2 = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv2))
  datConds = by(dat, INDICES = dat$subj
                , FUN = function(x) mean(x$dv[x$cond=="Lo"])-mean(x$dv[x$cond=="Hi"]))
  # datConds2 = by(dat, INDICES = dat$subj
  # , FUN = function(x) mean(x$dv2[x$cond=="Lo"])-mean(x$dv2[x$cond=="Hi"]))
  dat.lme = lmer(dv~cond+(cond|subj), dat)
  # dat.lme2 = lmer(dv2~cond+(cond|subj), dat)
  
  Vars1d[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts)
               , var(ranConds), var(ranef(dat.lme)$subj$cond1), var(datConds))
  
  Cors1d[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts)
                 , cor(ranef(dat.lme)$subj$`(Intercept)`, datInts)
                 , cor(ranConds, ranef(dat.lme)$subj$cond1)
                 , cor(ranConds, datConds)
                 , cor(ranef(dat.lme)$subj$cond1, datConds)
                 , cor(ranInts, ranConds)
                 , cor(ranef(dat.lme)$subj$`(Intercept)`, ranef(dat.lme)$subj$cond1)
                 , cor(datInts, datConds)
  )
  # Cors1d2[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
  #                , cor(ranInts, datInts)
  #                , cor(ranef(dat.lme)$subj$`(Intercept)`, datInts)
  #                , cor(ranConds, ranef(dat.lme)$subj$cond1)
  #                , cor(ranConds, datConds)
  #                , cor(ranef(dat.lme)$subj$cond1, datConds)
  #                , cor(ranInts, ranConds)
  #                , cor(ranef(dat.lme)$subj$`(Intercept)`, ranef(dat.lme2)$subj$cond1)
  #                , cor(datInts, datConds2)
  # )
}

colnames(Vars1d)=c("i.true", "i.mod", "i.obs", "s.true", "s.mod", "s.obs")
colnames(Cors1d)=c("i.trueVmod", "i.trueVobs", "i.modVobs"
                   , "s.trueVmod", "s.trueVobs", "s.modVobs"
                   , "is.true", "is.mod", "is.obs")
# colnames(Cors1d2)=c("i.trueVmod", "i.trueVobs", "i.modVobs"
#                    , "s.trueVmod", "s.trueVobs", "s.modVobs"
#                    , "is.true", "is.mod", "is.obs")
summary(Cors1d)

#Q1e ... if the slopes and effects are correlated
Vars1e = data.frame(matrix(NA,nrow=NSims, ncol=6))
Cors1e = data.frame(matrix(NA,nrow=NSims, ncol=9))

for(i in 1:NSims){
  ranStruct = rmvnorm(NSubj, mean=c(0,.5), sigma=matrix(c(1,.5,.5,1), nrow=2))
  ranInts = ranStruct[,1]
  ranConds = ranStruct[,2]
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*ifelse(dat$cond=="Hi", -.5, .5))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  datConds = by(dat, INDICES = dat$subj
                , FUN = function(x) mean(x$dv[x$cond=="Lo"])-mean(x$dv[x$cond=="Hi"]))
  dat.lme = lmer(dv~cond+(cond|subj), dat)
  
  Vars1e[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts)
               , var(ranConds), var(ranef(dat.lme)$subj$cond1), var(datConds))
  
  Cors1e[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts)
                 , cor(ranef(dat.lme)$subj$`(Intercept)`, datInts)
                 , cor(ranConds, ranef(dat.lme)$subj$cond1)
                 , cor(ranConds, datConds)
                 , cor(ranef(dat.lme)$subj$cond1, datConds)
                 , cor(ranInts, ranConds)
                 , cor(ranef(dat.lme)$subj$`(Intercept)`, ranef(dat.lme)$subj$cond1)
                 , cor(datInts, datConds)
  )
}

colnames(Vars1e)=c("i.true", "i.mod", "i.obs", "s.true", "s.mod", "s.obs")
colnames(Cors1e)=c("i.trueVmod", "i.trueVobs", "i.modVobs"
                   , "s.trueVmod", "s.trueVobs", "s.modVobs"
                   , "is.true", "is.mod", "is.obs")
summary(Cors1e)

# Q1f: OK, is the VarCorr table a good measure of the actual VarCorr structure ####
truCovs = data.frame(matrix(nrow=NSims, ncol=3, dimnames=list(NULL,c("VarInt", "Cov", "VarCond"))))
smpCovs = data.frame(matrix(nrow=NSims, ncol=3, dimnames=list(NULL,c("VarInt", "Cov", "VarCond"))))
obsCovs = data.frame(matrix(nrow=NSims, ncol=3, dimnames=list(NULL,c("VarInt", "Cov", "VarCond"))))
modCovs = data.frame(matrix(nrow=NSims, ncol=3, dimnames=list(NULL,c("VarInt", "Cov", "VarCond"))))
coefCovs = data.frame(matrix(nrow=NSims, ncol=3, dimnames=list(NULL,c("VarInt", "Cov", "VarCond"))))
for (i in 1:NSims) {
  trueVarInt = runif(1, .1, 100)
  trueVarCond = runif(1, .1, 100)
  trueCov = runif(1, -sqrt(trueVarInt*trueVarCond), sqrt(trueVarInt*trueVarCond))
  sigmaMat = matrix(c(trueVarInt, trueCov, trueCov, trueVarCond), nrow=2)
  truCovs[i,]=sigmaMat[lower.tri(sigmaMat, diag=T)]
  
  randCoefs = rmvnorm(NSubj, mean = c(0,0), sigma=sigmaMat)
  smpCovs[i,] = cov(randCoefs)[lower.tri(cov(randCoefs), diag=T)]
  
  dat$ranInt = randCoefs[dat$subj, 1]
  dat$ranCond = randCoefs[dat$subj, 2]
  dat$dv = rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*ifelse(dat$cond=="Hi", -.5, .5))
  obsInts = by(dat, dat$subj, function(x) mean(x$dv))
  obsConds = by(dat, dat$subj, function(x) mean(x$dv[x$cond=="Lo"])-mean(x$dv[x$cond=="Hi"]))
  obsCovs[i,] = c(var(obsInts), cov(obsInts, obsConds), var(obsConds))
  
  dat.lme = lmer(dv~cond+(cond|subj), dat)
  modCovs[i,]=as.data.frame(VarCorr(dat.lme))$vcov[c(1,3,2)]
  coefCovs[i,]=var(ranef(dat.lme)$subj)[lower.tri(var(ranef(dat.lme)$subj), diag=T)]
}

truCovs$Cor = with(truCovs, Cov/sqrt(VarInt*VarCond))
smpCovs$Cor = with(smpCovs, Cov/sqrt(VarInt*VarCond))
obsCovs$Cor = with(obsCovs, Cov/sqrt(VarInt*VarCond))
modCovs$Cor = with(modCovs, Cov/sqrt(VarInt*VarCond))
coefCovs$Cor = with(coefCovs, Cov/sqrt(VarInt*VarCond))

cor(cbind(truCovs$Cor,smpCovs$Cor, obsCovs$Cor, modCovs$Cor, coefCovs$Cor))
# Ok, so this leaves open how far the obs, true, and coef-based correlations are from each other
# Because in the case of correlation, we do actually care about the exact value.
CorDevs = data.frame(
  # Comparisons to population truth (what we really want)
  truVsmp = smpCovs$Cor-truCovs$Cor, truVobs = obsCovs$Cor-truCovs$Cor
  , truVmod = modCovs$Cor-truCovs$Cor, truVcoef = coefCovs$Cor-truCovs$Cor
  # Comparisons to Sample truth (what we can reasonably hope to achieve)
  , smpVobs = obsCovs$Cor-smpCovs$Cor, smpVmod = modCovs$Cor-smpCovs$Cor
  , smpVcoef=coefCovs$Cor-smpCovs$Cor
  # comparisons among ways of calculatin the correlation - the practical question of
  # does it matter?
  , obsVmod = modCovs$Cor - obsCovs$Cor, obsVcoef = coefCovs$Cor - obsCovs$Cor
  , modVcoef = coefCovs$Cor-modCovs$Cor)

ggplot(CorDevs)+
  geom_density(mapping=aes(x=smpVobs), col=1)+
  geom_density(mapping=aes(x=smpVmod), col=2)+
  geom_density(mapping=aes(x=smpVcoef), col=3)+
  
  # geom_density(mapping=aes(x=obsVmod), col=4)+
  # geom_density(mapping=aes(x=obsVcoef), col=5)+
  # geom_density(mapping=aes(x=modVcoef), col=6)+
  theme_bw()
### Q2: What happens when the data is skewed the way RT data often are? ####
# For this series, I'm going to assume the data are generated by an inverse gaussian dist'n (Wald)
# Q2a: intercepts only ####

Vars2a = data.frame(matrix(NA,nrow=NSims, ncol=3, dimnames=list(NULL, c("true", "mod", "obs"))))
Cors2a = data.frame(matrix(NA,nrow=NSims, ncol=3
                    , dimnames=list(NULL, c("trueVmod", "trueVobs", "modVobs"))))

for(i in 1:NSims){
  ranInts = abs(rnorm(NSubj))
  
  dat$ranInt = ranInts[dat$subj]
  dat$dv = rinvgauss(nrow(dat), mean=dat$ranInt, shape=dat$ranInt^3)
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~1+(1|subj), dat)
  
  Vars2a[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors2a[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}

summary(Cors2a)

# Q2b: ... what if there's a "main Effect" embedded, no random slope ####

Vars2b = data.frame(matrix(NA,nrow=NSims, ncol=3, dimnames=list(NULL, c("true", "mod", "obs"))))
Cors2b = data.frame(matrix(NA,nrow=NSims, ncol=3
                    , dimnames=list(NULL, c("trueVmod", "trueVobs", "modVobs"))))
for(i in 1:NSims){
  ranInts = abs(rnorm(NSubj))
  
  dat$ranInt = ranInts[dat$subj]
  dat$dv = rinvgauss(nrow(dat), mean=dat$ranInt+.5*as.numeric(dat$cond)
                     , shape=(dat$ranInt+.5*as.numeric(dat$cond))^3)
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~cond+(1|subj), dat)
  
  Vars2b[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors2b[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}
summary(Cors2b)

# Identical.

# Q2c: ... add an (unfitted) random slope that is not correlated with intercepts ####

Vars2c = data.frame(matrix(NA,nrow=NSims, ncol=3, dimnames=list(NULL, c("true", "mod", "obs"))))
Cors2c = data.frame(matrix(NA,nrow=NSims, ncol=3
                           , dimnames=list(NULL, c("trueVmod", "trueVobs", "modVobs"))))

for(i in 1:NSims){
  ranInts = abs(rnorm(NSubj))
  ranConds = abs(rnorm(NSubj, mean = .5, sd = .2))
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rinvgauss(nrow(dat), mean=dat$ranInt+dat$ranCond*as.numeric(dat$cond)
                     , shape=(dat$ranInt+dat$ranCond*as.numeric(dat$cond))^3)
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  dat.lme = lmer(dv~cond+(1|subj), dat)
  
  Vars2c[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors2c[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}

summary(Cors2c)

# Q2d: ...and what if we include the random "slope" ####

Vars2d = data.frame(matrix(NA,nrow=NSims, ncol=3, dimnames=list(NULL, c("true", "mod", "obs"))))
Cors2d = data.frame(matrix(NA,nrow=NSims, ncol=3
                           , dimnames=list(NULL, c("trueVmod", "trueVobs", "modVobs"))))

for(i in 1:NSims){
  ranInts = abs(rnorm(NSubj))
  ranConds = abs(rnorm(NSubj, mean = .5, sd = .2))
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = rinvgauss(nrow(dat), mean=dat$ranInt+dat$ranCond*as.numeric(dat$cond)
                     , shape=(dat$ranInt+dat$ranCond*as.numeric(dat$cond))^3)
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  ####
  dat.lme = lmer(dv~cond+(cond|subj), dat)
  
  Vars2d[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts))
  
  Cors2d[i,] = c(cor(ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(ranInts, datInts), cor(ranef(dat.lme)$subj$`(Intercept)`, datInts))
}

summary(Cors2d)

#Q2e ... if the slopes and effects are correlated####
include(mvtnorm)

Vars2e = data.frame(matrix(NA,nrow=NSims, ncol=6))
Cors2e = data.frame(matrix(NA,nrow=NSims, ncol=9))

for(i in 1:NSims){
  ranStruct = rmvnorm(NSubj, mean=c(0,.5), sigma=matrix(c(1,.5,.5,1), nrow=2))
  ranInts = ranStruct[,1]
  ranConds = ranStruct[,2]
  
  dat$ranInt = ranInts[dat$subj]
  dat$ranCond = ranConds[dat$subj]
  dat$dv = 1.75^rnorm(nrow(dat), mean=dat$ranInt+dat$ranCond*as.numeric(dat$cond))
  datInts = by(dat, INDICES = dat$subj, FUN=function(x) mean(x$dv))
  datConds = by(dat, INDICES = dat$subj
                , FUN = function(x) mean(x$dv[x$cond=="Lo"])-mean(x$dv[x$cond=="Hi"]))
  dat.lme = lmer(dv~cond+(cond|subj), dat)
  
  Vars2e[i,]=c(var(ranInts), var(ranef(dat.lme)$subj$`(Intercept)`), var(datInts)
               , var(ranConds), var(ranef(dat.lme)$subj$cond1), var(datConds))
  
  Cors2e[i,] = c(cor(1.75^ranInts, ranef(dat.lme)$subj$`(Intercept)`)
                 , cor(1.75^ranInts, datInts)
                 , cor(ranef(dat.lme)$subj$`(Intercept)`, datInts)
                 , cor(1.75^ranConds, ranef(dat.lme)$subj$cond1)
                 , cor(1.75^ranConds, datConds)
                 , cor(ranef(dat.lme)$subj$cond1, datConds)
                 , cor(1.75^ranInts, 1.75^ranConds)
                 , cor(ranef(dat.lme)$subj$`(Intercept)`, ranef(dat.lme)$subj$cond1)
                 , cor(datInts, datConds)
  )
}

colnames(Vars2e)=c("i.true", "i.mod", "i.obs", "s.true", "s.mod", "s.obs")
colnames(Cors2e)=c("i.trueVmod", "i.trueVobs", "i.modVobs"
                   , "s.trueVmod", "s.trueVobs", "s.modVobs"
                   , "is.true", "is.mod", "is.obs")
summary(Cors2e)
