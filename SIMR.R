include(simr)

# This analysis uses data from Wang et al. (2016) to estimate the effect sizes

# from fixef(m15c), we have:
intrcpt=6.563844346; session2=-0.002339066; session3=-0.047016095;trainingctrl=-0.045145818

# we also have random intercept data to use for simulation.
rndIntI.sd = 0.04643
rndIntS.sd = 0.15733

# and the residual sd
resid.sd = sd(resid(m15c)) # around .205

####### 
# what we don't have is anything to use for the group effect, but since the kids are randomly assigned, 
# there's no reason to think one group will be faster than the other on the untrained items.
# Randomly assign a small effect to each group
semgrpLow = rnorm(1,0,.01)
semgrpHigh = rnorm(1,0,.01)


###### For this design, let's use the following
# We have 24 subjects in 3 semantic Groups, doing 3 sessions, with
# 20 items in each of 2 learning Conditions (learned or unlearned)
N = 24; semGroups=3; Sessions=3; Items=20; LearnConds=2

# assign the random intercepts
rndIntrcpts.I = rnorm(Items*LearnConds,0,rndIntI.sd)
rndIntrcpts.S = rnorm(N*semGroups,0,rndIntS.sd)

# set up the simulated data
# subjects see all of the items in each of three sessions
sleep.sim = data.frame(subj=gl(N*semGroups, Sessions*Items*LearnConds),
                       session = gl(Sessions,Items*LearnConds, labels=c("S1", "S2", "S3")),
                       item = gl(Items*LearnConds,1))
# counterbalance items across learning conditions
sleep.sim$learned = factor(ifelse(as.numeric(sleep.sim$subj) %% 2 == 0,
                           ifelse(as.numeric(sleep.sim$item) %% 2 ==0, "learned", "unlearned"),
                           ifelse(as.numeric(sleep.sim$item) %% 2 !=0, "learned", "unlearned")))

# assign subjects to semantic learning groups
sleep.sim$semgrp = factor(c(rep("No", N), rep("Low", N), rep("High", N))[sleep.sim$subj], 
                          levels=c("No", "Low", "High")) # Make sure "No semantics" is the baseline.

# now, generate the log RTs
sleep.sim$lRT = intrcpt + rndIntrcpts.S[as.numeric(sleep.sim$subj)] + rndIntrcpts.I[as.numeric(sleep.sim$item)]+ # intercept
  c(0,session2,session3)[as.numeric(sleep.sim$session)] + # session effects
  c(0,trainingctrl)[as.numeric(sleep.sim$learned)] + # training effect for unlearned items in the No group
  c(0,semgrpLow, semgrpHigh)[as.numeric(sleep.sim$semgrp)] # main effects of semantic group
  
# Note that because everything is a factor here (categorical variable), the baseline intercept represents
# the No semantics group, with unlearned items, in session 1.

# OK, so now we want to manipulate the size of the interaction between learned and semgrp.
# since the prediction is that the learning effect for No (baseline) < Low < High, we'll assume
# equal distances.

# and we can vary how big the interaction is relative the base learning effect for "No" semantics.
nSims=100
scale.ints = seq(.05, 1, .05)
semLrnIntDiffs= scale.ints*trainingctrl # size of differences to test
pwr.pvals = matrix(NA, nrow=length(semLrnIntDiffs), ncol=nSims) # p.values from each power analysis
pwr.cis = matrix(NA, nrow=length(semLrnIntDiffs), ncol=2)

for (i in 1:10){
  semLrnInt = c(0,semLrnIntDiffs[i], 2*semLrnIntDiffs[i])
  
  sleep.sim$lRT.sim = sleep.sim$lRT + 
    ifelse(sleep.sim$learned=="unlearned",
           semLrnInt[as.numeric(sleep.sim$semgrp)],0)+ # add the interaction
    rnorm(nrow(sleep.sim), 0, resid.sd) # generate random errors anew for each power analysis
  
  fullmodel = lmer(lRT.sim~(semgrp+session+learned)^2+(1|subj)+(1|item), sleep.sim)
  compmodel = lmer(lRT.sim~(semgrp+session+learned)^2-semgrp:learned+(1|subj)+(1|item), sleep.sim)                                   
  
  pwr=powerSim(fullmodel, test=compare(compmodel), nsim=nSims, progress=F)
  pwr.pvals[i,] = pwr$pval
  pwr.cis[i,] = confint(pwr)
}

pwr.pwr = apply(pwr.pvals,1,function(x) mean(x<=.05))

ggplot(data.frame(eff=scale.ints, pwr=pwr.pwr, lCI=pwr.cis[,1], uCI=pwr.cis[,2]), 
       aes(x=scale.ints, y=pwr, label=round(pwr,2)))+
  #geom_point()+
  geom_errorbar(aes(ymin=lCI, ymax=uCI))+APA$Theme+
  geom_label()+
  ggtitle("Power as a function of effect size using SIMR (+ 95%CI bars)", 
          subtitle = paste("N =",N,"per group,",semGroups,"groups,", nSims,"simulations"))+
  xlab("Size of interaction as a proportion of the base learning effect")+
  ylab("Power")

