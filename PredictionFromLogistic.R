### Prediction from Logistic regression

## When there are covariates included in a model, the predicted cell proportions
# from logistic regression, do not appear to match the raw probabilities
# (with scaled covariates)
include(dplyr)

### Simple case ####

d1 = expand.grid(trial=1:20, condition=1:2) %>% data.frame %>% 
  mutate(covariate =scale(rnorm(n()))[,1]
         , dv = rbinom(n(), 1, prob=exp(1+.2*condition+.2*covariate)/(1+exp(1+.2*condition+.2*covariate)))
         , condition = factor(condition))

### no covariates
d1.cond = glm(dv~condition, d1, family="binomial")
summary(d1.cond)

merge(data.frame(condition = factor(1:2)
                 , pred = predict(d1.cond, newdata=data.frame(condition=factor(1:2)), type="response"))
      , aggregate(dv~condition, d1, mean))

### add covariates
d1.cov = glm(dv~condition+covariate, d1, family="binomial")
summary(d1.cov)

merge(data.frame(condition = factor(1:2)
                 , pred = predict(d1.cov, newdata=data.frame(condition=factor(1:2), covariate=0), type="response"))
      , aggregate(dv~condition, d1, mean))

merge(data.frame(condition = factor(1:2)
                 , pred = predict(d1.cov, newdata=data.frame(condition=factor(1:2), covariate=aggregate(covariate~condition, d1, mean)$covari), type="response"))
      , aggregate(dv~condition, d1, mean))
