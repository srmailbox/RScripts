include(lavaan); include(semPlot)
include(tidyverse); include(mvtnorm)


# Fake data - there are small correlations between X1 and Y, a large correlation
# between X2 and Y, and a moderately large correlation between X1 & X2.
set.seed(1129337568)
dat = rmvnorm(1000, mean=c(0,0,3)
              , sigma=matrix(c(  3,         .5*sqrt(6),.1*sqrt(15)
                               ,.5*sqrt(6), 2,         .8*sqrt(10)
                               ,.1*sqrt(15),.8*sqrt(10),5
                               ), nrow=3)) %>% 
  data.frame
colnames(dat)=c("X1", "X2", "Y")

# Regression shows that after controlling for the correlation between X1 and X2,
# X2 is a large positive predictor of Y, while X1 is a significant but *negative*
# predictor of Y.
(dat.lm=lm(Y~X1+X2, dat)) %>% summary

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.98365    0.03370   88.53   <2e-16 ***
# X1          -0.49708    0.02261  -21.99   <2e-16 ***
# X2           1.55071    0.02700   57.43   <2e-16 ***

# We can do the exact same analysis in SEM, but we end up with slightly
# different format of result:
# Note: we're using lavaan (LAtent VAriable ANalysis) to do the modelling
sem.LM = 'Y~X1+X2' # This is how you define a simple regression with two
                   # predictors
# If we fit that using sem(sem.LM, dat), we can use semPlot::semPaths to draw
# the resulting diagram:
(dat.sem=sem(sem.LM, dat)) %>% semPaths(whatLabels="est", edge.label.cex=1)

# Notice that the two arrows from X1 and X2 to Y have the same values on them as
# the coefficients from the regression analysis.

# We also have a couple of other values:
# 1.14 is the covariance between X1 and X2
# 2.87 and 2.01 are the variances for X1 and X2
# 1.13 is the "residual variance" for Y after accounting for X1 and X2's influence.

# Some additional things to think about when looking at these diagrams:

# 1. We talk about the effects along those arrows (X1->Y and X2->Y) as "direct"
#    effects of X. on Y
# 2. But! you can also see that X1 has another effect on Y that is "mediated"
#    by X2 - and we can actually calculate that influence!
#    "indirect effect of X1 on Y through X2" = 1.14*1.55 = 1.767
# 3. That means we have a "total" effect of X1 on Y as:
#    direct + indirect effect = -.50 + 1.767 = 1.267

# You can do the same for X2's influence on Y.

# That kind of "mediation analysis" is not readily available from lm().

# What we've just done is called "Path Analysis"
# Here are the rules for figuring out paths from one variable to another:
# 1. Start at the first variable
# 2. You can leave that variable through either a "head (arrow)" or "tail"
# 3. When you are at a new variable, it's more complicated:
# 3a. If you arrived through a "tail", you can leave by either a head or a tail.
# 3b. If you arrived through a "head", you can *only* leave through a "tail".
#     (no double-heads!)
# 4. Keep doing that, until you arrive at the other variable. But there is one
#    other restriction:
# 4a. You can never return to a variable you have already visited as part of the
#     path.

# So we've already worked out the two paths from X1 to Y - the direct path,
# and the one that goes through X2.

# How many paths are there between X1 to X2?

# Note, this is pretty trivial here where the model is very simple. This gets
# *MUCH* more complex as the models get more elaborate. E.g., Using 
# McArthur et al. (RRQ)'s MCS model, try to find all of the paths from Anxiety
# at age 5 to Depression at Age 11 in the MCS model.

