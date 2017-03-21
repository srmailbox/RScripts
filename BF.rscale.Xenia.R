# effSize is the "population" effect size.
# rVals are the r values you want to test (the default value, .707, is always tested)
effSize = 4
rVals = c(.1, .5, 1, 1.414)

df =  data.frame(factorA = gl(2,25))
df$DV = rnorm(50, mean=-as.numeric(df$factorA)*effSize)
# I negate the effSize just because of how as.numeric works. Ignore it.

library(BayesFactor)
HDIs = "95% HDIs:\n.707:\t" # Just a string to store the HDIs
# test the default, .707
tBF = ttestBF(form=DV~factorA, data=df, posterior=TRUE, iterations=100000)
HDIs = paste0(HDIs, round(quantile(tBF[,"beta (1 - 2)"], probs=.025), 3), " ", 
              round(quantile(tBF[,"beta (1 - 2)"], probs=.975), 3), "\n")
#Plot the posteriors for the difference between groups using the default "rscale", in black
plot(density(tBF[,"beta (1 - 2)"]))

# used to vary the colour of the lines
cl=2

for (r in 1:length(rVals)) {
  # cycle through the rVals and collect posteriors.
  tBF=ttestBF(form=DV~factorA, data=df, posterior=TRUE, iterations=100000, rscale=rVals[r])
  lines(density(tBF[,"beta (1 - 2)"]), col=cl)
  cl = cl+1
  HDIs = paste0(HDIs, rVals[r], ":\t", round(quantile(tBF[,"beta (1 - 2)"], probs=.025), 3), " ", 
                round(quantile(tBF[,"beta (1 - 2)"], probs=.975), 3), "\n")
  
}
cat(paste0(HDIs, "True value is ", effSize, "\nSample mean is ", -coef(lm(DV~factorA, df))[2]))
