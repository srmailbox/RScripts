# Simulate the Neale
# The Neale test for reading comprehension is a bit of a messy beast.
# The comprehension score is based on a series of 38 questions about 6 text passages.
# On the surface, this sounds like a Binomial model with 38 trials.
# However, there are two things that violate this model:
# 1) While reading the passages aloud, errors are noted. If a child makes 12-16 errors on
# a single passage, testing is stopped. This means that not all kids will get all 38 Qs.
# 2) Furthermore, the passages become more difficult as the test progresses. So even for
# a single kid, the probability of a "success" is diminishing as a function of the
# passages.

# One way to think of this is that the childs oral reading ability is going to predict how
# many items the kid sees. The child's reading comprehension, combined with the difficulty
# of the item, is going to predict how likely they are to get a Q right.

# Let's assume that reading ability and comprehension are correlated .7, but normally
# distributed.
include(mvtnorm)
p.lm = p.glm = numeric(1000)

dat = data.frame(expand.grid(ppt=1:875, itm = 1:38))
dat = merge(dat, itm.data)

strt=Sys.time()
for(i in 1:length(p.lm)){
  ppt.data = data.frame(rmvnorm(875, mean=c(-.5, -.5), sigma=matrix(c(1,.7,.7,1), nrow=2)))
  colnames(ppt.data) = c("ra", "rc")
  ppt.data$ppt = 1:875
  # Assume generally poor readers (younger kids) who are unlikely to read all 6 passages
  ppt.data$psgs = rbinom(nrow(ppt.data), size = 5, prob=p.odds(ppt.data$ra))+1
  # Let's assume that item difficulty is linearly distributed so that the probability
  # of a correct response for an average reader goes from 38/39 to 1/39
  
  itm.data = data.frame(itm = 1:38, psg = c(rep(1,7), rep(2,7), rep(3,6), rep(4,6)
                                            , rep(5,6), rep(6,6))
                        , ease=log((38:1/39/(1-38:1/39))))
  
  # dat.sim = merge(dat, ppt.data)
  # is this faster:
  dat.sim=dat
  dat.sim$ra = ppt.data$ra[dat.sim$ppt]
  dat.sim$rc = ppt.data$rc[dat.sim$ppt]
  dat.sim$psgs = ppt.data$psgs[dat.sim$ppt]
  dat.sim = dat.sim[dat.sim$psg<=dat.sim$psgs,]
  
  # A kid's probability of success of an item is 
  # exp(ppt.rc+item.ease)/(1+exp(ppt.rc+item.ease))
  
  dat.sim$corr = runif(nrow(dat.sim))<=p.odds(dat.sim$rc+dat.sim$ease)
  
  dat.ppt = aggregate(corr~ppt+rc+ra, dat.sim, sum)
  dat.ppt$x = rnorm(nrow(dat.ppt))
  p.lm[i]=summary(lm(corr~ra+x, dat.ppt))$coeff[3,4]
  p.glm[i]=summary(glm(cbind(corr,I(38))~ra+x, dat.ppt, family=binomial))$coeff[3,4]
}
message(paste("Time:", Sys.time()-strt))

mean(p.lm <= .05); mean(p.glm <= .05)
# [1] 0.054; [1] 0.055
# [1] 0.046; [1] 0.054
# [1] 0.055; [1] 0.055
# [1] 0.046; [1] 0.044
# [1] 0.047; [1] 0.049
