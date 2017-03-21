# Testing Mike's "Split-half" reliability approach

# Assume we have 100 Ss, each of which has a very stable effect (but that the subjects vary in this effect size)
# Further assume that all RTs have a standard deviation of 100ms. Assume they see 200 trials, 100 of each kind.

Design = data.frame(Subject=factor(gl(100,200)), StimType = factor(1:2, labels=c("A1", "A2")))

set.seed(-1747154389)
# Assign each subject an effect size from a uniform distribution ranging between 0 and 100 ms

SubjEffects = data.frame(Subject=factor(1:100), Effect=runif(100,0,100), RawSpeed=runif(100,450,750)) # So effects have a mean of 50, and an SD of 30.

Design = merge(Design, SubjEffects)
Design$RT = Design$RawSpeed+Design$Effect*as.numeric(Design$StimType) + rnorm(20000, 0,100)

require(lme4)
# This is the model we know is the truth, and unsurprisingly it produces a very solid effect.
lmer.1 = lmer(RT~StimType+(StimType|Subject), data=Design)

AnovaData = aggregate(RT~Subject+StimType,data=Design, FUN=mean)

aov.1 = aov(RT~StimType+Error(Subject/StimType), data=Design)

# OK, now let's look at Lawrence's approach. If it captures the truth, the effect size is very, very stable within a subject
# so split-halves should produce highly correlated effects.
Cors = rep(NA,1000)
for (i in 1:1000) {
  Design$Half = sample(rep(1:2,10000))
  HalfAData = aggregate(RT~Subject+StimType+Half, data=Design, FUN=mean)
  HalfAEffect = reshape(HalfAData, direction="wide", timevar="StimType", idvar=c("Subject", "Half"))
  HalfAEffect$Eff = HalfAEffect$RT.A2-HalfAEffect$RT.A1
  HalfACor=reshape(HalfAEffect, direction="wide", timevar="Half", idvar=c("Subject"), drop=c("RT.A1", "RT.A2"))
  Cors[i]=cor(HalfACor$Eff.1, HalfACor$Eff.2)
}

hist(Cors)
