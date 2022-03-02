### Partial correlations
# Demonstrating that you can either use the correlation matrix, or multiple
# regression, to calculate partial correlations. ppcor::pcor() uses the matrix
# but this means it is limited to continuous variables only. The regression
# approach will be more useful if you have factors (categorical variables).

library(mvtnorm); library(tidyverse)
# Just making up some data with three variables and some correlation structure.
# Notice that these are all continuous so that I can demonstrate that
# the ppcor::pcor package produces identical results to the regression approach

dat = rmvnorm(150, sigma=matrix(c(1, .9, .7, .9, 1, .6, .7, .6, 1),nrow=3)) %>% 
  data.frame
str(dat)
summary(dat)
cor(dat)

### PPCOR - or the matrix approach
ppcor::pcor(dat)$estimate
ppcor::pcor(dat)$p.value
# That gives us the partial correlation between 1 & 3, controlling for 2
### r_13.2 = .5270383, p = 5.024590e-12

### Regression approach - necessary for categorical variables which 
# ppcor doesn't handle

# we need a "full model" that includes control variables and the key variable
# (X1~X2+X3) or (X2~X1+X3)
# and a "partial model" that is *only the controls*
# (X1~X2) or (X2~X1)

X1.2 = lm(X1~X2, dat)
X1.23 = lm(X1~X2+X3, dat)

# There are two reasonably easy ways to get the partial from those - one uses
# the multiple R^2, and one uses the Residual Sums of Squares (RSS).

### Multiple R^2:
r2_1.2 = summary(X1.2)$r.squared
r2_1.23 = summary(X1.23)$r.squared

r_13.2 = sqrt((r2_1.23-r2_1.2)/(1-r2_1.2))
# r_13.2 = .5270383

### RSS approach
# The advantage here is that you can also get the p-value for the correlation
X1.anova = anova(X1.2, X1.23)

r_13.2 = sqrt((max(X1.anova$RSS)-min(X1.anova$RSS))/max(X1.anova$RSS))

# I use max() and min() here to avoid having to worry about what order
# I used for the anova() call - the "part" model will always have the larger
# RSS. If you want to be more explicit you can just use indices to make sure
# the line up:
# r_13.2 = sqrt((X1.anova$RSS[1]-X1.anova$RSS[2])/X1.anova$RSS[1])
# But you'll have to check that you got the [1]'s and[2]'s right

# the p-value is just the p-value of the anova test:
p.val = X1.anova$`Pr(>F)`[2]

# r_13.2 = .5270383, p = 5.02459e-12


### Using a categorical variable
# Note that we don't expect this to produce the same result as above, since
# the categorical version of X3 does not have the same information as the 
# continuous version did. (e.g., the correlation between X3 and X3c is < 1 )

dat = dat %>% mutate(X3c = factor(cut(X3, breaks=3), labels=c("a", "b", "c")))

X1.2 = lm(X1~X2, dat) # same as above, since the control variable is still cts.
X1.23c = lm(X1~X2+X3c, dat)

# I'll use the RSS approach just to get the p-value immediately
X1c.anova = anova(X1.2, X1.23c)

r_13c.2 = sqrt((max(X1c.anova$RSS)-min(X1c.anova$RSS))/max(X1c.anova$RSS))
# the p-value is just the p-value of the anova test:
p.valc = X1c.anova$`Pr(>F)`[2]

## r_13.2c = 0.4559489, p-value = 4.086616e-08

# So the correlation is weaker - which also nicely illustrates why it is a bad
# idea to artificially "categorize" a continuous variable. You usually lose
# power.
