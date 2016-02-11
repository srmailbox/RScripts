############ RUN THIS CODE FIRST

# load the predictSE function
if(! "AICcmodavg" %in% .packages(all=T)) install.packages("AICcmodavg")
library(AICcmodavg)



##################################

# Example use:

# create a data frame that represents the predictors at every level that I want to predict
# (in this case jsut for the two values of Congruency)
library(lme4)
lme.model = [g]lmer(formula... etc...)
pred.data = data.frame(training = c("trained", "trained", "untrained", "untrained"),
                       predict = c("predictable", "unpredictable"))

# For log transformed data (e.g., log(RT)) do:
predictSE.SR(lme.model, newdata=pred.data, link="log")

# For poisson data (e.g., counts of regressions) do:
predictSE.SR(lme.model, newdata=pred.data)

# This produces cell means ($fit) and standard errors of those predictions ($se.fit)
# Sample output:

# $fit
#         1         2 
# -1.649741 -1.648650 
# 
# $se.fit
#          1          2 
# 0.03934370 0.03934375 