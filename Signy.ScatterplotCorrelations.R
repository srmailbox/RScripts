# 1) read in the data as dat
dat = read.csv('~/Downloads/ET_Target_data.csv')

# 2) Aggregate the dataset so that you have the average fixation times by: 
#     Subject, their nonword scores, and the variables you're interested in.

dat.agg = aggregate(IA_FIRST_FIXATION_DURATION~sid+distn+training+predict, data=dat, FUN=mean)
# to save typing, I'm going to rename the IA_FIRST_FIXATION_DURATION variable to just "FF"
colnames(dat.agg)[ncol(dat.agg)]="FF"

# 3) what we want are separate columns of "predictable", and "unpredictable". (Right now
#     they are in rows.)
#     There are a few ways to do this. I'll show you two:

# 3A) use reshape, which is specifically designed to do this sort of thing.

dat.wide = reshape(direction="wide", data=dat.agg, v.names="FF", 
                  timevar="predict", idvar=c("sid", "distn", "training"))

# 3B) split the data into the two sets, and then merge them together.
prd = dat.agg[dat.agg$predict=="pred",]
unprd = dat.agg[dat.agg$predict=="unpred",]

dat.wide = merge(prd, unprd, by=c("sid", "distn", "training"), timevar="predict", 
                all=TRUE, suffixes=c(".pred", ".unpred"))
                 
# 4) calculate the difference score (predEff)

dat.wide$predEff = dat.wide$FF.unpred-dat.wide$FF.pred

# 5) calculate/test the correlations

with(dat.wide[dat.wide$training=="trained",], cor.test(distn, predEff))
with(dat.wide[dat.wide$training=="untrained",], cor.test(distn, predEff))

# 6) generate scatter plots

par(mfrow=c(2,1)) # This will put two plots on a single graph, one above the other
plot(dat.wide$distn[dat.wide$training=="trained"], dat.wide$predEff[dat.wide$training=="trained"], 
     pch=2, xlab="Nonword Score", ylab="Predicatability Effect",
     main="Scatterplot of Effects by Nonword Reading (trained)")
# this next line is optional, but will fit a "line of best fit"
abline(lm(predEff~distn, data=dat.wide[dat.wide$training=="trained",]))

plot(dat.wide$distn[dat.wide$training=="untrained"], dat.wide$predEff[dat.wide$training=="untrained"], 
     pch=3, xlab="Nonword Score", ylab="Predicatability Effect",
     main="Scatterplot of Effects by Nonword Reading (untrained)")
# this next line is optional, but will fit a "line of best fit"
abline(lm(predEff~distn, data=dat.wide[dat.wide$training=="untrained",]))

#### This last bit I was just curious about: notice that the Predictability effects for trained, and untrained
# items are completely uncorrelated.

par(mfrow=c(1,1))
plot(dat.wide$predEff[dat.wide$training=="untrained"], dat.wide$predEff[dat.wide$training=="trained"], 
     pch=3, xlab="untrained Predictability Effect", ylab="trained Predicatability Effect",
     main="Scatterplot of trained vs untrained predictability effects")
# this next line is optional, but will fit a "line of best fit"
abline(lm(dat.wide$predEff[dat.wide$training=="trained"]~dat.wide$predEff[dat.wide$training=="untrained"]))

cor.test(dat.wide$predEff[dat.wide$training=="untrained"], dat.wide$predEff[dat.wide$training=="trained"])
