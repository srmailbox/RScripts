# Playing around with the "Precision" parameter in BayesLogit::logit()

dat=data.frame(Y=sample(c(0,1), 500, replace=T), X=gl(2,250))
contrasts(dat$X)=contr.helmert(2)/2

dat.P0 = logit(dat$Y, dat$X, samp=100000)
dat.P1 = logit(dat$Y, dat$X, samp=100000, P0=diag(1))
dat.P5 = logit(dat$Y, dat$X, samp=100000, P0=diag(1)*5)
dat.P100 = logit(dat$Y, dat$X, samp=100000, P0=diag(1)*100)
dat.Pneg100 = logit(dat$Y, dat$X, samp=100000, P0=-100*diag(1))

par(mfrow=c(1,1))
plot(density(dat.P0$beta), ylim=c(0, max(density(dat.P100$beta)$y)), xlim=c(-.3, .3));abline(v=0)
lines(density(dat.P1$beta), col=2);abline(v=0)
lines(density(dat.P5$beta), col=3);abline(v=0)
lines(density(dat.P100$beta), col=4);abline(v=0)
lines(density(dat.Pneg100$beta), col=5);abline(v=0)
