# Continuous Case -----------------------------------------------------------

#### Continuous DV, with factor predictors.

# I'm setting up some random data here, with 
# - one factor (A) with two levels, 
# - 20 items (I) per level, and 
# - 50 subjects (S).
# The dv is "rt" which is just a normal with an average of 500 for level A1 and 550 for level A2,
# but with random intercepts and slopes by subject.

d = data.frame(S = gl(50, 40, labels = paste("S", 1:50, sep="")), A=gl(2, 20), I = factor(1:40))
# Define the random effects
s.rnd = data.frame(S=factor(1:50, labels = paste("S", 1:50, sep="")), 
                   intercept=rnorm(50, 500,50), slope = rnorm(50,50,10))

# Now set up the RTs

d$rt = s.rnd$intercept[d$S]+ifelse(d$A==2,s.rnd$slope[d$S],0)+rnorm(nrow(d),0,100)

# Fit a simple LME
if(!require(lme4)) install.packages("lme4")
d.lm = lmer(rt~A + (A|S), d)

# The effects package will only plot the "fixed" effects. There are a couple of ways you could plot the 
# random effects. 

# Here I do two things - first I do your standard bar plot of the two conditions, then superimpose lines
# for the subjects to show how the subjects varied. In the second plot, I use a box plot to depict the
# size of the factor effect, and then superimpose the factor effect differences by subject.


##### BAR PLOT WITH INDIVIDUAL DIFFERENCES
#
# The advantage here is that you really see exactly how the subjects differed, both in their overall
# speed (the intercept), and in the "slope" (size of effect).

# Get the main effects from the model for the box plot
mainConds = cbind(A1=fixef(d.lm)[1], A2=fixef(d.lm)[1]+fixef(d.lm)[2])
# Get the individual subject effects (this is just the main effects, + the deviations due to random
# effects)
subjConds = mainConds + cbind(A1=ranef(d.lm)$S[1], A2=ranef(d.lm)$S[1]+ranef(d.lm)$S[2])

# Plot the main effect
# Note, I need to widen the y-axis to make sure there will be space for the subject data.
barplot(mainConds, col="white", ylim=c(0, max(subjConds)))

# Add the random effects.
for(i in 1:50)
  lines(x=c(0.5,2), subjConds[i,], col=rgb(0,0,0,.3))
# I tend to draw the subject stuff as 'transparent' so that overlapping data is easier to see, but also
# so that the individual subject data don't completely overwhelm the mean performance.


#### BOXPLOT OF DIFFERENCE SCORES
# Another way to do it is to simply plot the difference scores as a box plot:

boxplot(fixef(d.lm)[2]+ranef(d.lm)$S[2])
points(cbind(jitter(rep(1,50), factor=3), fixef(d.lm)[2]+ranef(d.lm)$S[2]), col=rgb(0,0,0,.3), pch=20)

# I use jitter here just so that the data don't fall on a straight line down the middle.



# Binomial Case -----------------------------------------------------------

#### Binomial DV, with continuous predictor.

# I'm setting up some random data here, with 
# - one continuous predictor (F) ranging from 0 to log(1000), to mimic frequencies from 1-1000 
# - 20 items (I) per level, and 
# - 50 subjects (S).
# The dv is "accuracy" which is binomial

# Note that binomial data is a bit more complicated to simulate, since you are no longer fitting the DV
# itself, but rather fitting the "logistic" link function: log(accuracy/(1-accuracy))
#
# The catch is that the relationship between accuracy and the logistic link function is non-linear:

plot(x=seq(-5,5,length.out=1001), exp(seq(-5,5,length.out=1001))/(1+exp(seq(-5,5,length.out=1001))),
     type="l", xlab="logistic value", ylab="proportion", main="Relationship between logistic values and proportions")

# This means that you either plot the results in terms of the logistic function, or you have to 
# convert back into percentages.

# In this next section I set up the random data.
# If you don't care about all of this stuff, just skip to the next part where I do the analysis and
# plotting

d = data.frame(S = gl(50, 40, labels = paste("S", 1:50, sep="")), Freq=runif(20, 0, log(1000)), I = factor(1:40))
# Define the random effects
s.rnd = data.frame(S=factor(1:50, labels = paste("S", 1:50, sep="")), 
                   intercept=runif(50, -1.4, 1.4), # Random intercepts (or accuracy for 0 frequency items)
                   # those are equivalent to .2 to .8 or so, but in the logistic form
                   slope = runif(50, -.5, 1)) # effect of frequency on the logistic link function

# for each "trial" this is the "expected" accuracy of response based on the above, but in the logistic form
d$fit =  s.rnd$intercept[d$S] + d$Freq*s.rnd$slope[d$S]

# trial accuracy distribution: hist(exp(d$fit)/(1+exp(d$fit)))
# OK, now this is the "expected" probability of success on each trial
d$p = exp(d$fit)/(1+exp(d$fit))

# Now randomly generate the actual DV outcome variable.
d$acc = rbinom(nrow(d), 1, prob=d$p)

##### OK, now we have data set up, let's fit it

if(!require(lme4)) install.packages("lme4")
d.glm = glmer(acc~Freq+(Freq|S), d, family=binomial)

##### PLOT WITH INDIVIDUAL DIFFERENCES IN THE SLOPES
#
# 1. There are several ways to do show the individual differences. The easiest is to simply do a box plot
# of the coefficients.

subjCoefs = fixef(d.glm)+ranef(d.glm)$S
# head(subjCoefs)

#    (Intercept)         Freq
# S1   0.7021089 -0.214605258
# S2  -1.1246503  0.094498361
# S3  -0.7727643  0.006162669
# S4  -0.6065374 -0.093254355
# S5  -0.0970412 -0.111657095
# S6  -0.1285255 -0.178065898
# ...

boxplot(subjCoefs$Freq, title="Individual Frequency Coefs by Subject")
points(cbind(jitter(rep(1,50), factor=0.5), jitter(subjCoefs$Freq)), col=rgb(0,0,0,.3), # make them mostly transparent
       pch=20)

# 2. A more complicated way is to draw the individual regression lines for each subject, but 
# leave it as the logistic transformed variable. This shows both the "intercepts" and "slopes"
# for each subject, but in units that aren't readily interpretable to most researchers.

# Get the data we need to generate lines for individual subjects
# Get the range of data you want to plot
xrng=range(d$Freq)
# This is the range of Frequency Values

# Since we're drawing a straight line, we just need the regression estimates for the most extreme
# values of Freq:
estPts = rbind(c(1,1), xrng) # Note that the coefficient for the intercept is always 1

# We can use matrix multiplication to get the relevant values
subjPts = as.matrix(subjCoefs) %*% as.matrix(estPts)
yrng = range(subjPts)

# We can do the same thing for the overall main effect
mainEffect = fixef(d.glm)%*%as.matrix(estPts)

# First plot the "main" effect
plot(x=xrng, y=mainEffect, type="l", lwd=2, xlim=xrng, ylim=yrng, xlab="Freq", ylab="Logistic Value")

# Add the random effects.
for(i in 1:50)
  lines(x=xrng, subjPts[i,], col=rgb(0,0,0, .3))
# I tend to draw the subject stuff as 'transparent' so that overlapping data is easier to see, but also
# so that the mean (fixed) regression is still clear.

# 3. The most complicated version is if you'd like to show the curves as Proportions rather than the 
# logistic function values. This is exactly like 2, but instead of the predicted "Logistic" value,
# you're plotting the predicted accuracy as a proportion.

# I won't get into the details, but if "l" is the logistic value, then it turns out the associated
# proportion is given by exp(l)/(1+exp(l)) where exp() is the natural exponential.

# If you want to show the values in terms of "proportion accuracy", we're going to be producing curves.
# This means we need to fit more "Freq" values.
estPts = rbind(rep(1, 100), # Predicted intercept values (always 1)
               seq(xrng[1], xrng[2], length.out=100)) # Generate 100 equally spaced values between the
# min and max Frequency values

# Now we get the Logistic values for all of those points
subjPts = as.matrix(subjCoefs) %*% as.matrix(estPts)

# Do the same thing for the overall main effect
mainEffect = fixef(d.glm)%*%as.matrix(estPts)

# So far this is identical to the previous version, except instead of just "predicting" the ends of the
# range of Frequencies, we're "predicting" 100 points along the Frequency range.

# The tricky bit is that now we have to reverse the logistic function for all of the "predicted" data:
subjPts = exp(subjPts)/(1+exp(subjPts))
mainEffect = exp(mainEffect)/(1+exp(mainEffect))

yrng=c(0,1) # Y is now proportion correct, so the range is 0 to 1.

# First plot the "main" effect
plot(x=seq(xrng[1], xrng[2], length.out=100), y=mainEffect, type="l", lwd=2, xlim=xrng, ylim=yrng, xlab="Freq", ylab="Proportion Accurate")

# Add the random effects.
for(i in 1:50)
  lines(x=estPts[2,], subjPts[i,], col=rgb(0,0,0, .3))

# Here you can see that despite a very flat overall average, the individual subjects 
# have very different curves.