require(lme4)
lme.model = lmer(iRT~Congruency+cPrevRT + (1|Subject) + (1|Colour), data=Dataset)

# this is just inverse RT predicted by congruency with a covariate (cPrevRT)
# A few notes - 
# 1. if using lmerTest, must run class(lme.model)="lmerMod" first
# 2. no abbreviations of variable names (e.g., Cong).

# load the predictSE function
require(AICcmodavg)

# create a data frame that represents the predictors at every level that I want to predict
# (in this case jsut for the two values of Congruency)
pred.data = data.frame(Congruency=c("Congruent", "Incongruent"), cPrevRT=0)

predictSE(lme.model, newdata=pred.data)

# This produces cell means ($fit) and standard errors of those predictions ($se.fit)
# Sample output:

# $fit
#         1         2 
# -1.649741 -1.648650 
# 
# $se.fit
#          1          2 
# 0.03934370 0.03934375 