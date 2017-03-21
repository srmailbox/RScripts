
par(mfrow=c(5,1), mar=c(2,4,1,1))
popn = ifelse(rnorm(10000)<1, rnorm(10000, 4, 1), rnorm(10000, 84, 10))

rng = c(0,120)
temp=hist(popn, main="Distribution of Population", xlim=rng)
mltplr = max(temp$count)/max((dn=dnorm((qs=seq(min(popn), max(popn), .5)),mean(popn), sd(popn))))
lines(x=qs, y=dn*mltplr, col=2)


sMeans = numeric(10000)

for (i in 1:10000) sMeans[i]=mean(sample(popn, (N=10), replace=TRUE))
temp=hist(sMeans, main=paste("Distribution of means, when N =", N), xlim=rng)
mltplr = max(temp$count)/max((dn=dnorm((qs=seq(min(sMeans), max(sMeans), .01)),mean(sMeans), sd(sMeans))))
lines(x=qs, y=dn*mltplr, col=2)

for (i in 1:10000) sMeans[i]=mean(sample(popn, (N=20), replace=TRUE))
temp=hist(sMeans, main=paste("Distribution of means, when N =", N), xlim=rng)
mltplr = max(temp$count)/max((dn=dnorm((qs=seq(min(sMeans), max(sMeans), .01)),mean(sMeans), sd(sMeans))))
lines(x=qs, y=dn*mltplr, col=2)

for (i in 1:10000) sMeans[i]=mean(sample(popn, (N=50), replace=TRUE))
temp=hist(sMeans, main=paste("Distribution of means, when N =", N), xlim=rng)
mltplr = max(temp$count)/max((dn=dnorm((qs=seq(min(sMeans), max(sMeans), .01)),mean(sMeans), sd(sMeans))))
lines(x=qs, y=dn*mltplr, col=2)

for (i in 1:10000) sMeans[i]=mean(sample(popn, (N=1000), replace=TRUE))
temp=hist(sMeans, main=paste("Distribution of means, when N =", N), xlim=rng)
mltplr = max(temp$count)/max((dn=dnorm((qs=seq(min(sMeans), max(sMeans), .01)),mean(sMeans), sd(sMeans))))
lines(x=qs, y=dn*mltplr, col=2)
