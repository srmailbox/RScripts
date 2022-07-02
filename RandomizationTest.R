### Randomization test example.

### Correlation

library(mvtnorm)

## create some data

set.seed(1)
dat = data.frame(
  rmvnorm(100, sigma=matrix(c(1, .3, .2, .3, 1, .1, .2, .1, 1), nrow=3))
)

cor(dat)

#### First, record the "observed value". For our purposes we'll use the
# correlation between X1 and X2

r12 = cor(dat)[1,2]

#### Then repeat, say 10000, random assignments of the two variables

rSim = numeric(1000)
for (i in 1:length(rSim)){
  # create 100 "simulated" observations by randomly pairing values with replacement
  # and calculate the resulting correlations.
  rSim[i]=cor(dat$X1[sample(100, 100, replace=T)], dat$X2[sample(100,100,replace=T)])
}

### Then check how often the simulated r values were larger than the observed r
# (This is the proportion of simulated correlations that were greater than the
# observed correlation (two-tailed))
mean(abs(rSim)>=abs(r12))
# p = .014

# (if this was close to .05, I might increase the number of simulations since
# different resamplings might shift things around from significant to 
# non-significant)

### consider X2 X3, since that effect is quite a bit smaller

# observed
r23 = cor(dat)[2,3]

#### Then repeat a 1000 random assignments of the two variables

rSim = numeric(1000)
for (i in 1:length(rSim)){
  # create 100 "simulated" observations by randomly pairing values up with 
  # replacement
  rSim[i]=cor(dat$X3[sample(100, 100, replace=T)], dat$X2[sample(100,100,replace=T)])
}

mean(abs(rSim)>=abs(r23))
# p = .446
## Now 446 simulated p-values were larger than the observed, so our p-value is
# .446


#### partial correlations?

# If, say, you are interested in the partial correlation of X2 and X3, 
# controlling for X1, you have a problem. You want to keep the correlation
# between X1 X2, and X1 X3, while randomly assigning the X2 and X3 values.

## For categorical control variable, this may be just a matter of randomizing
# within the group. Test this out.

# I'm going to use some slightly stronger correlations here
dat = data.frame(
  rmvnorm(100, sigma=matrix(c(1, .4, .3, .4, 1, .4, .3, .4, 1), nrow=3))
)

# use X1 to arbitrarily define 3 groups.
dat$Group = cut(dat$X1, breaks = 3, labels = c("Low", "Med", "Hi"))

### Calculate observed r23.1 using the RSS method

dat.full = lm(X2~Group+X3, dat)
dat.ctrl = lm(X2~Group, dat)

anv = anova(dat.full, dat.ctrl)
r23.1 = sqrt((max(anv$RSS)-min(anv$RSS))/max(anv$RSS))

#### OK, so now we need to randomize the X2,X3 pairings within the Groups.
# this is easier within the tidyverse
library(tidyverse)

rSim23.1 = numeric(length(rSim)*10)

# As above, run a bunch of sims. This is a lot slower because we have to fit
# all the regression models and anova()'s
for(i in 1:length(rSim23.1)) {
  simDat = dat %>% group_by(Group) %>% 
    mutate(X2 = sample(X2, n(), replace=T)
           , X3 = sample(X3, n(), replace=T)) %>% 
    ungroup()
  sim.full = lm(X2~Group+X3, simDat)
  sim.ctrl = lm(X2~Group, simDat)
  
  anvSim = anova(sim.full, sim.ctrl)
  rSim23.1[i] = sqrt((max(anvSim$RSS)-min(anvSim$RSS))/max(anvSim$RSS))
}

# This will give you the proportion of simulations that produce rSim >= r observed
mean(rSim23.1>=r23.1)
# with 1000 sims, the p value was .054 - this is pretty close to .05, so I redid
# it with 10000 just in case it was actually a bit lower or higher: p = .0558.

# Note that this is actually not terribly far from the p-value from the anova
# above (p = .06095). Which is nice, since this data *is* actually perfectly
# normal. I suspect if I randomized without replacement, this would be closer
# still.

altSim23.1 = numeric(length(rSim)*10)

for(i in 1:length(rSim23.1)) {
  simDat = dat %>% group_by(Group) %>% 
    mutate(X2 = sample(X2, n())
           , X3 = sample(X3, n())) %>% 
    ungroup()
  sim.full = lm(X2~Group+X3, simDat)
  sim.ctrl = lm(X2~Group, simDat)
  
  anvSim = anova(sim.full, sim.ctrl)
  altSim23.1[i] = sqrt((max(anvSim$RSS)-min(anvSim$RSS))/max(anvSim$RSS))
}

mean(altSim23.1>=r23.1)

# Yep. p = .0597 - sweet. (Would have to do a lot more simulations to prove
# this worked in more than just this single case - but I don't care that much.)