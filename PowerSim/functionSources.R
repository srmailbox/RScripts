# Functions that will take a set of parameters and do the power calculation for X1 using my method
# and the simr package.

# Assume 2x3 design, where you want the power for the 3 level main effect. (anova)

# These are the parameters along with their default values.
powerSim2x3 = function(mod,
  design, VarCorr, fixef, sigma
  , NSims = 1000, alpha = .05)
{ # Simulation stuff
  # lmerTest is about 3-5 times slower, so I just get the FValues and convert them
  # to p-values myself.
  library(lmerTest)
  
  baseY = fixef[1]
  effectX1 = c(0, fixef[2])
  effectX2 = c(0, fixef[3:4])
  intX1X2 = c(0, fixef[5:6])
  # Run the Sims ####
  # From the information gathered, I can randomly generate some DVs, run the model,
  # and store the statistics
  fVals = matrix(NA, nrow=NSims, ncol=3, dimnames=list(NULL, c("fX1", "fX2", "fX1X2")))

  for(i in 1:NSims){
    # assign random intercepts
    # subjInts = rnorm(length(unique(design$Subj)), sd=sqrt(VarCorr$Subj))
    # itemInts = rnorm(length(unique(design$Item)), sd=sqrt(VarCorr$Item))
    # Assign the DV based on the "effect" sizes and variance
    
    design$Y = simulate(model, newparams=list(beta=fixef(model), theta=getME(model, "theta"), sigma=1)
                        , simData=getData(model), family=family(model), weights=1, simOpts=list())$sim_1
    # design$Y = with(design
    #                 , baseY+subjInts[Subj]+itemInts[as.numeric(Item)]+ # "intercept"
    #                   effectX1[as.numeric(X1)]+effectX2[as.numeric(X2)]+ # main effects
    #                   +intX1X2[as.numeric(X2)]+ # interaction
    #                   rnorm(nrow(design), sd=sigma) # residuals
    # )
    
    # fit the lme
    fit.lme = lmer(Y~X1*X2+(1|Subj)+(1|Item), design)
    # Store the F-statistics
    pVals[i,]=anova(fit.lme)[,6]
  }
  # This will arrive at the same DF that lmerTest uses to estimate the p-values
  # denDF = 4431 #length(unique(dat$Item))-length(X1Levels)*length(X2Levels)
  # numDF = c(length(levels(design$X1))-1, length(levels(design$X2))-1
  #           , (length(levels(design$X2))-1)*length(levels(design$X1))-1
  # )
  # # Use the numDF and denDF to calculate the p-values
  # pVals = data.frame(pX1 = 1-pf(fVals[,1], numDF[1], denDF),
  #                    pX2 = 1-pf(fVals[,2], numDF[1], denDF),
  #                    pX1X2 = 1-pf(fVals[,3], numDF[1], denDF)
  # )
  # 
  return(
    list(
      pVals = pVals, numDF = numDF, denDF = denDF
      
      , fixef=fixef              
      , parameters = list(NSims = NSims, alpha = alpha
                          , SubjectInterceptVariance = VarCorr$Subj
                          , ItemInterceptVariance = VarCorr$Item
                          , Sigma = sigma)
      
      , power = unlist(apply(pVals, 2, function(x) mean(x<=alpha)))
    )
  )
}

powerSim.simr = function(mod, design, VarCorr, fixef, sigma, NSims=100) {
  
  # mod = makeLmer(y~X1*X2+(1|Subj)+(1|Item), data=design, fixef=fixef, sigma=sigma, VarCorr=VarCorr)
  powerSim(mod, nsim=NSims, test=fixed("X1", "anova"), progress=F)
  
}
