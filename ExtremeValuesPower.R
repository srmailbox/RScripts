#### Power for T-tests vs Correlations ####
# GM noted that a medium size effect in a T-Test requires far less data than
# a Medium sized effect in a correlation.
# I know this is in part down to design differences - T-tests typically take
# advantage of extreme values sampling, but I wonder if we can find some kind
# of equivalency by saying something like "d = .5 means that r is around .8"

include(tidyverse)
include(mvtnorm)

set.seed(100)
trueCor = .3 # Cohen's so-called moderate effect size
dat = rmvnorm(1000000, sigma = matrix(c(1, trueCor, trueCor, 1), nrow=2)) %>% 
  data.frame %>% 
  rename(Y=X2, X=X1) %>% 
  mutate(Group = ifelse(pnorm(X)<.16, "Low", ifelse(pnorm(X)>.84, "High", "Mid")))

rho=cor(dat %>% select(X, Y))[1,2]
cohenDelta = aggregate(Y ~ Group, dat, function(x) c(mean(x), sd(x))) %>% 
  data.frame %>% mutate(Mean=Y[,1], SD = Y[,2]) %>% select(Group, Mean, SD)

cohenDelta = (cohenDelta[2,2]-cohenDelta[1,2])/sd(dat$Y[dat$Group !="Mid"])
c(rho=rho, Delta=cohenDelta)

EVSrho = cor(dat %>% filter(Group!="Mid") %>% select(X,Y))[1,2]
