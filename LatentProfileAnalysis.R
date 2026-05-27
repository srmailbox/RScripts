include(mvtnorm)
set.seed(-1566880030)
centres = matrix(runif(6, min=-2, max=2), nrow=3)
vs = data.frame(matrix(runif(6), nrow=3)) %>% 
  mutate(X2 = X2+1.5, cov=runif(3,min=-X1*X2, max=X1*X2))
sgm = matrix(c(vs$X1[1], vs$cov[1], vs$cov[1], vs$X2[1]), nrow=2)
G1 = rmvnorm(100, mean = centres[1,], sigma=sgm)

sgm = matrix(c(vs$X1[2], vs$cov[2], vs$cov[2], vs$X2[2]), nrow=2)
G2 = rmvnorm(100, mean = centres[2,], sigma=sgm)

sgm = matrix(c(vs$X1[3], vs$cov[3], vs$cov[3], vs$X2[3]), nrow=2)
G3 = rmvnorm(100, mean = centres[3,], sigma=sgm)

dat = data.frame(rbind(G1, G2, G3), Grp = gl(3,nrow(G1))) %>% 
  mutate(ID = rep(1:nrow(G1), 3))

ggplot(dat, aes(x=X1, y=X2, col=Grp))+
  # facet_grid(.~Grp)+
  geom_point(alpha=.8)+theme_bw()+
  geom_smooth(method="lm", se=F)

### do the mclust

include(mclust)
dat.BIC = mclustBIC(dat %>% select(-Grp))

apply(dat.BIC, 2, which.max)

# First, use a single group to show what each "slot" is doing:
mdls = c("EII", "EEI", "EEE")
par(mfrow=c(1,3))
for(i in mdls)
{mdl = i
dat.mclst = Mclust(dat %>% filter(Grp!=2) %>% select(-Grp) , model=mdl, G=1)
plot(dat.mclst, what="classification")
title(main=mdl)
}


# Demonstrates What happens when "size" varies (EXX vs VXX)
# and when "variances" are varied (XIX, XEX, XVX)
mdls=c("EII", "EEI", "EVI", "VII", "VEI", "VVI")
# demonstrates what happens when variances are varied (XIX, XEX, XVX)
# and when "rotation" varies (XXI, XXE, XXV)
mdls=c("EEI", "EEE", "EEV", "EVI", "EVE", "EVV")
par(mfrow=c(2,3))
for(i in mdls)
{mdl = i
dat.mclst = Mclust(dat %>% select(-Grp), model=mdl, G=3, x=dat.BIC)
plot(dat.mclst, what="classification")
title(main=mdl)
}


# plot(dat.BIC)
# dat.BIC
