## Using emmeans on "poly" fitted objects

### Creating some artificial data with an categorical (Fact) and continuous (X)
# variable, and a DV with main effects of Fact and X, but no interactions.
# Also the data does not have any actual curvilinear relationship.
dat = data.frame(Fact=rep(c("A", "B"), 1000), X = rnorm(2000)) %>% 
  mutate(dv = ifelse(Fact=="A", 0,.2)+.5*X+rnorm(n()))

dat.lm = lm(dv~Fact*poly(X,2), dat)

# Turns out this is not possible. There just does not seem to be
# any way to tell emmeans/trends to treat the X variable as a polynomial

include(emmeans)
emtrends(dat.lm, ~X+Fact, var="X")

# Workaround: Instead of using poly() in the call to lm as above, create those
# variables in the dataset separately, and specify them explicitly in the model:

dat = data.frame(dat, poly(dat$X, 2))
#There are now two additional columns representing the orthogonal polynomials:
colnames(dat) # X1 and X2 are the new vars. I'll rename them
colnames(dat)[4:5]=c("X.linear", "X.quadratic")

dat.lm2 = lm(dv~Fact*(X.linear+X.quadratic), dat)

# You can now compare them and find that the results are identical:

# poly in the formula
summary(dat.lm)
# poly created in the dataset
summary(dat.lm2)

## now we can use emmeans with the dat.lm2 model and specify the linear and
# quadratic components

dat.emt = emtrends(dat.lm2, specs=~Fact, var="X.linear")
pairs(dat.emt)

dat.emt = emtrends(dat.lm2, specs=~Fact, var="X.quadratic")
pairs(dat.emt)


## To get data for plotting, you can use the predict/predictSE functions

# first create the data you'd like to predict:
predData = expand.grid(Fact=c("A","B"), X = seq(min(dat$X), max(dat$X), .1))

predData$dv = predict(dat.lm, newdata=predData)

ggplot(predData, aes(x=X, y=dv, colour=Fact))+geom_line()

# NOte that here I'm using the fitted model with poly() in the formula so that I can
# plot X in it's original units, not in the units of the "modified poly() 
# orthogonal" versions of X.

# You could also use the version we used in emtrends by doing the following:
# but this is not perfectly mapped

predData2 = data.frame(Fact=c("A","B")
                       , poly(unique(dat$X), 2))

# (Names have to match those in the model)
colnames(predData2)=c("Fact", "X.linear", "X.quadratic")

predData2$dv = predict(dat.lm2, newdata=predData2)

ggplot(predData2, aes(x=X.linear, y=dv, colour=Fact))+geom_line()

