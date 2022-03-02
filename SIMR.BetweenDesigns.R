include(dplyr);include(simr);include(lme4)
dat = expand.grid(code=1:80, item=1:12) %>% data.frame %>% 
  mutate(group = factor(code%%4)
         , dv = rnorm(n())+ifelse(code==1,.1,0)+
           rnorm(80, sd=.3)[code]+rnorm(12, sd=.1)[item])


model.base = lmer(dv~group+(1|code)+(1|item), dat)
model.N = extend(model.base, along="code", n=800)
fixef(model.N)["group1"]=.1
# would produce about 200 ppts per group.

pc20 = powerCurve(model.N, along="code", breaks=seq(10, 200, 10)*4, nsim=20)

# Looks like we need more than 800 to be certain.
model.N = extend(model.base, along="code", n=1000)
fixef(model.N)["group1"]=.1
pc100 = powerCurve(model.N, along="code", breaks=c(180,190,200,210)*4, nsim=1000)
