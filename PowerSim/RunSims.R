### Create the design matrix
source('functionSources.R')
# Design of the study is a 2x3
X1Levels = c("A", "B") # 2-level factor
# Observations
X2Levels = c("A", "B", "C") # 3-level factor
N = 15 # Subjects
NTrials = 50 # Trials per condition - each subject sees 6*NTrials
# That's all we need to create the deign matrix
dat = data.frame(expand.grid(Subj=1:N, X1 = X1Levels, X2 = X2Levels, Trial = 1:NTrials))
dat$Item = with(dat, factor(Trial)) # Items are repeated, so the Item is actually

NTests = 100
options(warn=-1)
results = matrix(ncol=2, nrow=NTests)
for (i in 1:NTests){
  VarCorr = list(Subj=1, Item=1) #list(Subj=rnorm(1)^2+.001, Item=rnorm(1)^2+.001)
  sigma = 1 #sqrt(rnorm(1)^2+.001)
  # Randomly assign the "truth"
  fixef=rnorm(6, mean=.05, sd=.025)*c(100,rep(1,5))
  
  # Get the power for X2 from each method
  mod = makeLmer(y~X1*X2+(1|Subj)+(1|Item), data=design, fixef=fixef, sigma=sigma, VarCorr=VarCorr)
  results[i,1]=powerSim2x3(mod, dat, VarCorr, fixef, sigma, NSims = 100)$power["pX1"]
  results[i,2]=mean(
    powerSim.simr(mod, design=dat, VarCorr=VarCorr
                  , fixef=fixef, sigma=sigma, NSims=100)$pval<=.05
    )
}
