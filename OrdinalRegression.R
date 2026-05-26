#### Ordinal Regression
include(ordinal);include(VGAM)
# Cumulative models ####
set.seed(2022007)
dat = expand.grid(S=1:30, I=1:30) %>% 
  mutate(skill.alpha = c(10,12)[S%%2+1], beta=10
         , P = rbeta(n(), skill.alpha, beta)
         , skill=factor(skill.alpha, labels=c("low", "high"))
         , dv = factor(cut(P, breaks = c(0,.45,.6,1), labels=c("error", "partial", "correct"))
                       , ordered=T)
  )

contrasts(dat$skill)=contr.sum(levels(dat$skill))
summary((dat.clm=clm(dv~1, data=dat)))
# Threshold coefficients:
#     Estimate Std. Error z value
# 0|1  -1.0986     0.2309  -4.757
# 1|2   1.6582     0.2728   6.079

# table(dat$dv)
# Ok, so we have 18% of the data are 0, 76% are 1, and 6% are 2

# observe that log(.18/(1-.18))=log(.18/.82)=-1.5163 or the 0|1 Theta
# log(.94/.06)=2.751535, or the 1|2 Theta

# so it seems that the Threshold i|(i+1) here represent the logOdds of P(dv<=i)
# or the log(dv<=i/dv>i)
# in fact, plogis(Theta) gives the cumulative probabilities
#       0|1  1|2
# [1,] 0.25 0.84

#### Ok, so what about with a predictor

summary((dat.clm2=clm(dv~skill, data=dat)))

# Thetas are very different, but I think they represent Thetas for "low" skill?
# table(dat$skill, dat$dv)
#       0  1  2
# low  15 35  0
# high  3 41  6

# Ok, so this gets a bit messy and is no longer a perfect match it seems that
# p(dv<=1) is given by plogis(Theta i|i+1 - Beta)
# in this case, that gives cumulative probs of:

#          0    1
# low   .309 .987
# high  .047 .892

# "TRUTH"
#          0    1
# low   .300 1.00
# high  .060 .880

# So not quite right, but close. I suspect if we eliminated the "0" condition
# the estimates would be more accurate

# Tested with 50 Ppts and 50 Items, but still not a lot more accurate.

# Stopping ratio ####

# ordinal only allows cumulative models. Let's pull in VGAM and look at
# other approaches.

include(VGAM)

## First match the cumulative to ordinal::clm ####
summary((dat.vclm=vglm(dv~1, cumulative, data=dat)))
summary((dat.vclm2=vglm(dv~skill, cumulative, data=dat)))
# OK, so the default is equivalent to a "nominal" model from clm, which assumes
# that each skill gets its own thresholds, rather than assuming that "skill"
# shifts the thresholds a constant amount.
summary((dat.vclm3=vglm(dv~skill, cumulative(parallel=T), data=dat)))
# this will produce the default behaviour from clm.

summary((dat.clm3 = clm(dv~1, nominal=~skill, data=dat)))


## Now try sratio ####

summary((dat.vsr = vglm(dv~skill, sratio(parallel=T), data=dat)))
summary((dat.vcr = vglm(dv~skill, cratio(parallel=T), data=dat)))
summary((dat.vcum = vglm(dv~skill, cumulative(parallel=T), data=dat)))

## The main difference is in the following:
# cumulative:
# Names of linear predictors: logitlink(P[Y<=error]), logitlink(P[Y<=partial])
# stopping ratio:
# Names of linear predictors: logitlink(P[Y=error|Y>=error]), logitlink(P[Y=partial|Y>=partial])
# continuation ratio:
# Names of linear predictors: logitlink(P[Y>error|Y>=error]), logitlink(P[Y>partial|Y>=partial])
# Which are (essentially) identical for the first logitlink, but differ in the second.

#      error    partial    correct 
# 0.04777778 0.85888889 0.09333333 
# for cumulative: .047r/(1-.047r), and .906r/(1-.906r)
# for sratio: .047r/(1-.047r), and .858r/.093r [partial/correct]
# cratio is just sratio, but swaps the numerator and denominators swapped,
# so the output is the same except for the sign. (A a bit like just switching
# analyzing "correct" responses vs "errors" in a binomial model.)


emmeans::emmeans(dat.vsr, "skill")
