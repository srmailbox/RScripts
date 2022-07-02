include(mvtnorm); include(tidyverse); include(MBESS);include(psych);
include(gdata)

baseMat = matrix(c(1,.9,.8, .6, .9, 1, .7, .6, .8, .7, 1, .5, .6, .6, .5, 1)
                 , nrow=4)

corMat = matrix(runif(64, .1,.3), nrow=8)
upperTriangle(corMat)=lowerTriangle(corMat, byrow=T)
corMat[1:4,1:4]=baseMat
corMat[5:8,5:8]=t(baseMat)

dat = rmvnorm(1000, sigma=corMat) %>% data.frame


dat = floor(dat)-min(floor(dat))
cor(dat)

omega = ci.reliability(dat)
alpha = psych::alpha(dat)

c(omega$est
,alpha$total$raw_alpha)

fa = factanal(dat, factors=4)

omegaA = ci.reliability(dat[,1:4])
omegaB = ci.reliability(dat[,1:4+4])
c(omegaA$est, omegaB$est)
alphaA = alpha(dat[,1:4])
alphaB = alpha(dat[,1:4+4])
c(alphaA$total$raw_alpha, alphaB$total$raw_alpha)
