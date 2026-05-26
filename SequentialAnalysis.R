### Simulating the results from mjgrayling's Sequential Analysis calculator
# https://mjgrayling.shinyapps.io/optgs/
#
# These are the "default" settings:
# effect size .2, sd's both 1, power = .8 (beta=.2), alpha = .05
# 2 stages, near-optimal, nul-optimal, integer sample sizes

Nsims = 500000
H0.ts=H0.ps = matrix(nrow=Nsims, ncol=3)

signAlphas = 1-pt(c(2.574,1.556), df=c(346, 694))
ftltAlphas = 1-pt(c(0.732,1.556), df = c(346, 694))
for (i in 1:Nsims){
  dat = data.frame(g1 = rnorm(696), g2=rnorm(696))
  H0.ps[i,]=c(with(dat[1:232,], t.test(g1, g2, var.equal=T, alternative="greater")$p.value)
           , with(dat[1:348,], t.test(g1, g2, var.equal=T, alternative="greater")$p.value)
           , with(dat, t.test(g1, g2, var.equal=T, alternative="greater")$p.value))
}

H0.ts[,1]=-qt(H0.ps[,1], 230)
H0.ts[,2]=-qt(H0.ps[,1], 346)
H0.ts[,3]=-qt(H0.ps[,2], 694)

H0.result.33 = ifelse(H0.ps[,1]<=signAlphas[1], "E1"
                , ifelse(H0.ps[,1]>=ftltAlphas[1], "F1"
                         , ifelse(H0.ps[,3]<=signAlphas[2], "E2", "F2")
                         )
                )

H0.result.5 = ifelse(H0.ps[,2]<=signAlphas[1], "E1"
                   , ifelse(H0.ps[,2]>=ftltAlphas[1], "F1"
                            , ifelse(H0.ps[,3]<=signAlphas[2], "E2", "F2")
                   )
)

table(H0.result.5)/length(H0.result.5)
table(H0.result.33)/length(H0.result.33)
# E1       E2       F1       F2 
# 0.005122 0.044908 0.767080 0.182890 
# E1       E2       F1       F2 
# 0.005342 0.038140 0.767890 0.188628 

# So alpha is maintained in both cases, but a bit more aggressively in the
# second case.

####
H1.ts=H1.ps = matrix(nrow=Nsims, ncol=3)

# signAlphas = 1-pt(c(2.574,1.556), df=c(346, 694))
# ftltAlphas = 1-pt(c(0.732,1.556), df = c(346, 694))
for (i in 1:Nsims){
  dat = data.frame(g1 = rnorm(696, mean=.2), g2=rnorm(696))
  H1.ps[i,]=c(with(dat[1:232,], t.test(g1, g2, var.equal=T, alternative="greater")$p.value)
              , with(dat[1:348,], t.test(g1, g2, var.equal=T, alternative="greater")$p.value)
              , with(dat, t.test(g1, g2, var.equal=T, alternative="greater")$p.value))
}

H1.ts[,1]=-qt(H1.ps[,1], 230)
H1.ts[,2]=-qt(H1.ps[,1], 346)
H1.ts[,3]=-qt(H1.ps[,2], 694)

H1.result.33 = ifelse(H1.ps[,1]<=signAlphas[1], "E1"
                      , ifelse(H1.ps[,1]>=ftltAlphas[1], "F1"
                               , ifelse(H1.ps[,3]<=signAlphas[2], "E2", "F2")
                      )
)

H1.result.5 = ifelse(H1.ps[,2]<=signAlphas[1], "E1"
                     , ifelse(H1.ps[,2]>=ftltAlphas[1], "F1"
                              , ifelse(H1.ps[,3]<=signAlphas[2], "E2", "F2")
                     )
)

table(H1.result.5)/length(H1.result.5)
table(H1.result.33)/length(H1.result.33)

# E1       E2       F1       F2 
# 0.528728 0.434740 0.028290 0.008242 
# E1       E2       F1       F2 
# 0.338754 0.576588 0.077572 0.007086 

# Power is well higher than the .8 we were aiming for in both cases.


### Packages for sequential designs###

include(rpact)
include(SMARTAR)
