###############
# Testing whether fitting GAMS to subjects, destroys individual differences that are real.
#
ID1 = rnorm(50)
data = data.frame(
  S = factor(gl(50,200)), #Subjects
  F1 = factor(rep(c("F1a", "F1b")[gl(2,100)], 50)), # First factor, two levels
  F2 = factor(rep(c("F2a", "F2b")[gl(2,50)], 100)),
  ID1 = ID1[gl(50,200)] # First individual difference
)

data$ID2 = rnorm(ID1, 50)[gl(50,200)]
# Highly correlated ID1 and ID2

data$Y = rnorm(10000,sd=50,mean=300+as.numeric(data$F1)*20+as.numeric(data$F2)*5-as.numeric(data$F1)*as.numeric(data$F2)*10)
data$Y = data$Y + rnorm(10000, mean=data$ID2*25+data$ID1*15+data$ID1*data$ID2*10)
data.agg = aggregate(Y~S+F1+F2+ID1+ID2, data=data, FUN=mean)

data.aov = aov(Y~F1+F2+ID1+ID2+F1:F2+Error(S/(F1*F2)), data=data.agg)
require(lme4)
data.lme = lmer(Y~F1+F2+ID1+ID2+F1:F2+(1|S), data=data)
data.lme2 = lmer(Y~F1+F2+ID1+ID2+F1:F2+((F1+F2)^2|S), data=data)
require(gam);require(mgcv)
data.gam = gam(Y~(F1+F2)^2+s(ID1)+s(ID2)+te(ID1,ID2)+s(S, bs="re"), data=data)