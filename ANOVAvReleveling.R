include(tidyverse)
include(multcomp)
include(car)

dat = expand.grid(N=1:20, f = letters[1:6]) %>% data.frame

cntrsts = matrix(c(0, 1, 0, 0, 0, 0,
                   0, 0, 1, 0, 0, 0,
                   0, 0, 0, 1, 0, 0,
                   0, 0, 0, 0, 1, 0,
                   0, 0, 0, 0, 0, 1,
                   0, 1,-1, 0, 0, 0,
                   0, 1, 0,-1, 0, 0,
                   0, 1, 0, 0,-1, 0,
                   0, 1, 0, 0, 0,-1,
                   0, 0, 1,-1, 0, 0,
                   0, 0, 1, 0,-1, 0,
                   0, 0, 1, 0, 0,-1,
                   0, 0, 0, 1,-1, 0,
                   0, 0, 0, 1, 0,-1,
                   0, 0, 0, 0, 1,-1
), nrow=15, byrow=T)
rownames(cntrsts)=paste0("Test",1:15)

f.p = ts.p = ts.c = numeric(10000)
for(i in 1:length(f.p)){
  dat.lm = lm(dv~f, dat %>% 
                mutate(dv = rnorm(nrow(.))))
  
  dat.glht = summary(glht(dat.lm, linfct=cntrsts), test=adjusted("none"))
  ts.p[i] = min(dat.glht$test$pvalues)
  ts.c[i] = min(p.adjust(dat.glht$test$pvalues, method="holm"))
  f.p[i] = Anova(dat.lm, type="III")[2,4]
  if(!i %% 100) message(".", appendLF=F)
}

plot(f.p, ts.p, cex=.5, col=2^(as.numeric(f.p<=.05))+4^as.numeric(ts.p<=.05)-1)
abline(h=.05, v=.05, col="grey")
table(f.p<=.05, ts.p<=.05)

plot(f.p, ts.c, cex=.5, col=2^(as.numeric(f.p<=.05))+4^as.numeric(ts.c<=.05)-1)
abline(h=.05, v=.05, col="grey")
table(f.p<=.05, ts.c<=.05)
