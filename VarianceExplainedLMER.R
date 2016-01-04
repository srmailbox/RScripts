d=data.frame(X1=rnorm(1000), X2=rexp(1000), subj=gl(50,20))
d$Y = .1*d$X1+.2*d$X2+rgamma(1000, .5)+5
d$Y[sample(1000,5)]=NA
require(lme4)
d.X1X2 = glmer(Y~X1*X2+(1|subj), d, family=Gamma("identity"))
d.X1X2.gauss = lmer(Y~X1*X2+(1|subj), d)
d.X1.X2= update(d.X1X2, .~.-X1:X2)
d.X1= update(d.X1.X2, .~.-X2)
d.X2= update(d.X1.X2, .~.-X1)
d.nofix = update(d.X1, .~.-X1) # Random only
d.null = glm(Y~1, d, family=Gamma("identity")) # no random or fixed - "true" null

v.X1X2=var(d.X1X2@frame$Y-predict(d.X1X2)) # Full Model
v.nofix=var(d.nofix@frame$Y-predict(d.nofix)) # No fixed effects
v.null = var(d.null$y-predict(d.null)) # No fixed or random effects
v.X1 = var(d.X1@frame$Y-predict(d.X1)) # 
v.X2 = var(d.X2@frame$Y-predict(d.X2)) # 
v.X1.X2 = var(d.X1.X2@frame$Y-predict(d.X1.X2)) # 

# how much "additional variance" is explained by the full model over various submodels
1-(v.X1X2/c(v.X1X2, v.X1.X2, v.X1, v.X2, v.nofix, v.null))
# How much variance does each model explain
1-c(v.nofix, v.X2, v.X1, v.X1.X2, v.X1X2)/v.null

r2 = cor(predict(d.X1X2), d.X1X2@frame$Y)^2


