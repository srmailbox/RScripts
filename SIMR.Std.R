include(simr)

# This analysis assumes a standardized situation so that effect sizes
# are related to SDs (e.g., unitless)

# We have 24 subjects in 3 semantic Groups, doing 3 sessions, with
# 20 items in each of 2 learning Conditions (learned or unlearned)
N = 24; semGroups=3; Sessions=3; Items=20; LearnConds=2

# First, let's assume only random intercepts
S.ints = rnorm(N*semGroups,0,.3)
I.ints = rnorm(Items*LearnConds, 0, .2)

# trainingEffect
trainEffect = -.3
# semantic effect, assume very small diffs if at all.
semEffect = rnorm(3,0,.01)
# trainingXsemantics
trainSemInt = -.1
trainEffxSem = c(1:semGroups-1)*trainSemInt
# session effect - assume subjects get faster with practice
sessEffect = -c(1:Sessions-1)*.2

# Alright, now set up the data frame.
# Note, items are not counterbalanced here, and it is assumed all groups
# see the same learned and unlearned items.
sleep.sim = data.frame(subj=gl(N*semGroups, Sessions*Items*LearnConds),
                       session = gl(Sessions,Items*LearnConds, labels=c("S1", "S2", "S3")),
                       item = gl(Items*LearnConds,1),
                       learned = gl(LearnConds,1), labels=c("learned", "unlearned"))

sleep.sim$semgrp = factor(c(rep("No", N), rep("Low", N), rep("High", N))[sleep.sim$subj])

# randomly generate reaction times that have the properties we've described.
sleep.sim$rt = S.ints[as.numeric(sleep.sim$subj)]+I.ints[as.numeric(sleep.sim$item)]+ # Intercept
  trainEffect*(as.numeric(sleep.sim$learned)-1) + #training effect
  semEffect[as.numeric(sleep.sim$semgrp)] + # semantic grp effect
  sessEffect[as.numeric(sleep.sim$session)]+ # session effect
  trainEffxSem[as.numeric(sleep.sim$semgrp)]*(as.numeric(sleep.sim$learned)-1)+ # semantic by training interaction
  rnorm(nrow(sleep.sim),0,1) # error

sleep.sim$rt = (sleep.sim$rt-mean(sleep.sim$rt))/sd(sleep.sim$rt)

fullmodel = lmer(rt~(semgrp+session+learned)^2+(1|subj)+(1|item), sleep.sim)
compmodel = lmer(rt~(semgrp+session+learned)^2-semgrp:learned+(1|subj)+(1|item), sleep.sim)

# tests power to detect the presence of a Group x Lexical Inhibitino effect
(pSim=powerSim(fullmodel, test=compare(nullmodel), nsim=10))
