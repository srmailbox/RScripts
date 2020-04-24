pwr.seq = seq(.1, .9, .01)
n.seq = 2:100
d.seq = seq(.1, 1, .01)

# First, let's calculate the N needed for different D's to get Power = .8

Ns = c()
for (d in d.seq) {
  Ns =c(Ns, power.t.test(delta=d, power=.8)$n)
}
Ns = ceiling(Ns)

plot(d.seq, Ns, type="l", ylim=c(0,120), 
     ylab="N per group", 
     xlab="effect size", 
     main="Sample size by effect size for Power=.8")

# Ok, now let's calculate Power for different D's, using N per group = 40

Pwr = c()
for (d in d.seq) {
  Pwr =c(Pwr, power.t.test(delta=d, n=40)$power)
}

plot(d.seq, Pwr, type="l", 
     ylab="Power", 
     xlab="effect size", 
     main="Power by effect size for N=40 per Group")

d.8 = power.t.test(n=40, delta=.8)$power
d.5 = power.t.test(n=40, delta=.5)$power
d.2 = power.t.test(n=40, delta=.2)$power 
lines(c(0, .8), c(d.8,d.8), lty=2)
lines(c(.8,.8), c(0,d.8), lty=2)
text(.1,1, round(d.8,2))

lines(c(0, .5), c(d.5,d.5), lty=2)
lines(c(.5,.5), c(0,d.5), lty=2)
text(.1,.65, round(d.5, 2))

lines(c(0, .2), c(d.2,d.2), lty=2)
lines(c(.2,.2), c(0,d.2), lty=2)
text(.1,.2, round(d.2, 2))

p.8 = power.t.test(n=40, power=.8)$delta
lines(c(0, p.8), c(.8, .8), lty=3)
lines(c(p.8,p.8), c(0,.8), lty=3)
text(.67,.06, round(p.8, 2))
