N=1000
d = data.frame(factA = gl(2,N/2), factB = factor(c(1,2)))
d$RT = 50*as.numeric(d$factA) + 50 * as.numeric(d$factB)-10*as.numeric(d$factA)*as.numeric(d$factB)

lambda = 400; mu = 400
v = rnorm(N,0, 1)^2

temp = mu+mu^2*v/(2*lambda)-mu/(2*lambda)*sqrt(4*mu*lambda*v+mu^2*v^2)

z = rnorm(N,0,1)

invGaussErrs = ifelse(z<=mu/(mu+temp), temp, mu^2/temp)

d$RT = d$RT + invGaussErrs

d.RT = lm(RT~factA*factB, d)
d.invRT = lm(1/RT~factA*factB, d)
d.RT.inv = glm(RT~factA*factB, d, family=inverse.gaussian(link="identity"))
d.RT.inv.can = glm(RT~factA*factB, d, family=inverse.gaussian)

par(mfrow=c(2,2))

qqnorm(resid(d.RT), main="raw N(0,sig2)")
qqnorm(resid(d.invRT), main="1/RT N(0,sig2)")
qqnorm(resid(d.RT.inv), main="RT IG(mu,lamda), link=identity")
qqnorm(resid(d.RT.inv.can), main="RT IG(mu,lamda), link=1/mu^2")

