collin = seq(-.99,.99,.02)
pvals = matrix(0,ncol=2, nrow=length(collin))
power = matrix(0,ncol=2, nrow=length(collin))
fx = matrix(0,ncol=2, nrow=length(collin))
VIF = numeric(length(collin))
NSim=1000
for (s in 1:NSim)
  for(ci in 1:length(collin)){
    Xs = mvtnorm::rmvnorm(32, sigma=matrix(c(1,collin[ci],collin[ci],1), nrow=2))
    VIF[ci] = VIF[ci]+1/(1-cor(Xs)[1,2]^2)/NSim
    Ys = Xs %*% c(1,.5)+rnorm(nrow(Xs),sd=1)
    pvals[ci,]=pvals[ci,]+summary(lm(Ys~Xs))$coefficients[2:3,4]/NSim
    power[ci,]=power[ci,]+(summary(lm(Ys~Xs))$coefficients[2:3,4]<=.05)
    fx[ci,]=fx[ci,]+summary(lm(Ys~Xs))$coefficients[2:3,1]
  }
par(mfrow=c(3,1))
plot(VIF, pvals[,1], type="l", ylab="Mean p-value",sub=paste(NSim,"simulations"))
abline(h=.05, col="grey")
lines(VIF, pvals[,2], type="l", col=2)

plot(VIF, power[,1]/NSim, type="l", ylab="Power",sub=paste(NSim,"simulations"))
lines(VIF, power[,2]/NSim, type="l", col=2)

plot(collin, fx[,1]/NSim, type="l", ylab="Estimates", xlab="X1 X2 Correlation",
     sub=paste(NSim,"simulations"), ylim=c(min(fx), max(fx))/NSim)
lines(collin, fx[,2]/NSim, type="l", col=2)

beepr::beep()
