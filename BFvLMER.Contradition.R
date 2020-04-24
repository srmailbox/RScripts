##### How do multi-level factors work with lmer vs BF

# The question I'm wanting to answer is, if I have three levels and two of them differ by a lot,
# could the BF fail to pick that up if I consider all of the levels at once?

subj = data.frame(int = rnorm(10,0,100), 
                  factA1 = rnorm(10,0,10), 
                  factA2=rnorm(10,0,10), 
                  factA3=rnorm(10,0,10))

if(!"d" %in% load("BFvLMER.Contradiction.d.RDat")){
  d = data.frame(factA = gl(3,100), subj = factor(1:10))
  d$dv = 500+subj$int[d$subj]+
    c(0+subj$factA1[d$subj],10+subj$factA2[d$subj],10+subj$factA3[d$subj])[d$factA]+
    rnorm(300,0,100)
  save(d, subj, file="BFvLMER.Contradition.d.RDat")
}
include(lme4)
d.F.S = lmer(dv~1+(1|subj), d) # intercept only
d.Ff.S = lmer(dv~factA+(1|subj), d) # add factA
d.Ff.Sf = lmer(dv~factA+(factA|subj), d) # add random slopes
anova(d.F.S, d.Ff.S, d.Ff.Sf)

# produces a highly significant chi-sq.
summary(d.Ff.Sf) # A2>A1=A3
d$factA=relevel(d$factA,ref=2)
d.Ff.Sf = lmer(dv~factA+(factA|subj), d) # add random slopes
summary(d.Ff.Sf) # A3>(A1=A2)

include(BayesFactor)
d.F.S.bf = lmBF(dv~1+subj, whichRandom = "subj", d)
d.Ff.S.bf = lmBF(dv~factA+subj, whichRandom = "subj", d)
#d.Ff.Sf.bf = lmBF(dv~factA*subj, whichRandom = c("subj", "factA:subj"), d)

d.Ff.S.bf/d.F.S.bf
#c(d.Ff.Sf.bf,d.Ff.S.bf)/d.F.S.bf
