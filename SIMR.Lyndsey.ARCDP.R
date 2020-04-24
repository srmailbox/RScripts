#  Power analysis for Lyndsey's ARC discovery proposal.

if(!require(simr)){
  install.packages("simr")
  library(simr)
} 

subject = 1:40
semcond = c("High", "Low")
session = 1:4
item = 1:8

dat = expand.grid(subject=subject, session=session, item=item, semcond=semcond)
dat$itemID = paste(dat$semcond, dat$item, sep="")
# Fixed effects - assume moderate effects of semcond and session, and a small interaction
betas = c(0, .5, -.1, -.1)

# Assume a really simple diagonal variance/covariance matrix
Vsubject = diag(nrow=4)
Vitem = diag(nrow=2)

# Assume unit error variance
s = 1

testModel = makeLmer(y~semcond*session+(semcond*session|subject)+(session|itemID), fixef=betas, 
                     VarCorr=list(Vsubject, Vitem), data=dat, sigma=s)

(pSim=powerSim(testModel, nsim=1000, test="semcondLow:session"))
#(pSim=powerSim(fullmodel, test=compare(rt~semcond+session+(1|subj)), nsim=1000))


# Alternate way to test the model -----------------------------------------

fixef(fullmodel)[4]=10

powerSim(fullmodel, test=fcompare(rt~semcond+session), nsim=500)

# Find Effect Size, given N and Power -------------------------------------

# Ok, so since we know we have 40 subjects, and we want power of .8, how big would our training effect have
# to be?  To save time, I start out looking for an effect size that produces a conf interval around .8.
# Then we can use that too look more carefully in a range near the right value.
maxInteraction = 100
minInteraction = 0
newInteraction = 50
pwr = 0
strt=T
while( !(confint(pSim)[1]<=.8 && confint(pSim)[2]>=.8) | strt) {
  strt=F
  print(paste("Testing effect:", newInteraction, "msecs"))
  data.sim$rt = 1000+S.ints[as.numeric(data.sim$subj)]+ # Intercept
    trainEffect*data.sim$session + #training effect
    semEffect[as.numeric(data.sim$semcond)] + # semantic grp effect
    newInteraction*data.sim$session*as.numeric(data.sim$semcond)+ # semantic by training interaction
    # This last line addes some random error to the RTs. This is important. It is basically the amount
    # of noise that we can not explain with the above structure.
    # It turns out that the power analysis is very sensitive to this value...
    rnorm(nrow(data.sim),0,150)
  fullmodel = lmer(rt~(semcond+session)^2+(1|subj), data.sim)
  pSim=powerSim(fullmodel, test=fcompare(rt~semcond+session), nsim=500, progress=F)
  pwr = mean(pSim$pval<=.05)
  print(paste("Power:", pwr, "(",confint(pSim)[1], confint(pSim)[2],")"))
  lastInteraction = newInteraction
  if(pwr>.9) {
    newInteraction = minInteraction+ (newInteraction-minInteraction)/2
    maxInteraction = lastInteraction
  }
  if(pwr <.7){
    newInteraction = maxInteraction - (maxInteraction-newInteraction)/2
    minInteraction = lastInteraction
  }
}

