# This challenge: https://fivethirtyeight.com/features/can-you-win-this-hot-new-game-show/

A1 = score 1
A2 = score 2
B1 = opponent score 1
B2 = opponent score 2

c = change threshold (change if A1 < c) [P(Change) = c]
d = opponent change threshold (oChange if B1 < d) [P(oChange) = d],
c and d are independent of each other

P(A2>A1) = 1-c
P(B2>B1) = 1-d

P(Win) = P(Win|Stay, oStay) P(Stay & oStay) + P(Win|Stay, oChange) P(Stay & oChange) +
          P(Win|Change, oStay) P(Change & oStay) + P(Win|Change, oChange) P(Change & oChange)
       = P(Win|Stay, oStay) (1-c)(1-d) + P(Win|Stay, oChange) (1-c)*d +
          P(Win|Change, oStay) c*(1-d) + P(Win|Change, oChange) c*d

# AHHHHH... The trick is that if you kept A1, we know that A1 lies at c or above
# Similarly, if the opponent kept B1, then it must be above d.
P(Win | Stay, oStay) = P(A1 > B1) = P(U[c, 1]>U[d,1])
P(Win | Stay, oChange) = P(U[c, 1]>U[0,1])
P(Win | Change, oStay) = P(U[0,1]>U[d,1])
P(Win | Change, oChange) = P(U[0,1]>U[0,1])=.5

# When both stay, the problem is complicated because the solution depends on whether c<d or not
P(Win | Stay, oStay)  = P(U[c, 1]>U[d,1]) has two cases. When c < d, or c > d
Assume c<d,
                      = P(A1>B1|A1 from U[c,d], but B1 from U[d,1])*P(A1 in U[c,d]|A1 in U[c,1]) +
                        P(A1>B1|A1 and B1 from U[d,1])*P(A1 in U[d,1]|A1 in U[c,1])

# First term has to be zero, since (c,d) is always less than d-1
P(A1 in U[d,1]|A1 in U[c,1]) = (1-d)/(1-c)
P(A1>B1|A1 & B1 from U[d,1]) = .5

# OK, quite confident about this.
P(Win | S, oS, c<d) = 0*(d-c)/(1-c)+.5(1-d)/(1-c) = (.5-.5d)/(1-c)

Assume c>d, # Feel pretty good here
                = P(A1>B1 | B1 came from U[d,c] & A1 from U[c,1]) * P(B1 came from U[d,c]| B1 in U[d,1]) + 
                  P(A1>B1 | B1 & A1 from U[c,1])*P(B1 from U[c,1]| B1 in U[d,1])
                = 1*(c-d)/(1-d) + .5*(1-c)/(1-d)
                = (c-d+.5-.5c)/(1-d) = (.5+.5c-d)/(1-d)

# the other cases should be easier since they rely on only one of c or d.

P(Win | S, oC)  = P(A1>B2 | A1 in [c,1], B2 in [0,1]) = 1*P(B2 in U[0,c] | B2 in U[0,1])+
                                      .5*P(B2 in U[c,1] | B2 in U[0,1])
                = c+.5(1-c) = .5+.5c

P(Win | C, oS) = P(A2>B1|A1 in [0,1], B1 in [d,1]) = .5* P(A2 in U[d,1]| A2 in U[0,1]) = .5(1-d)

P(Win | C, oC) = P(U[0,1]>U[0,1])=.5

# OK, now back to P(Win)

# if c < d
P(Win)  = P(Win|Stay, oStay) (1-c)(1-d) + P(Win|Stay, oChange) (1-c)*d +
          P(Win|Change, oStay) c*(1-d) + P(Win|Change, oChange) c*d
        = (.5-.5d)/(1-c)(1-c)(1-d) + (.5+.5c)(1-c)*d + .5(1-d)c*(1-d) + .5 c*d
        = (.5-.5d)(1-d) + (.5+.5c)(1-c)*d + .5(1-d)c*(1-d) + .5 c*d
        = .5(1-d)^2 + .5d(1-c)(1+c) +.5c(1-d)^2+.5cd
        =[(1+c)(1-d)^2+d(1-c^2)+cd]/2

# This one gets integrated over the interval d=[c,1] wrt to d

# Let's try Ryacas - nope, need yacas installed
# include(Ryacas)
# Integrate(((1+c)*(1-d)^2+d*(1-c^2)+c*d)/2, "d")

# if c > d
P(Win) = (.5+.5c-d)/(1-d)(1-c)(1-d) + (.5+.5c)(1-c)*d + .5(1-d)c*(1-d) + .5 c*d
       = [(1+c-2d)(1-c) + (1-c^2)*d + c(1-d)^2 + c*d]/2

# This one gets integrated over the interval d=[0,c] wrt to d

# OK, once we have those integrals, we can take the derivatives wrt c and solve for 0
# Hopefully they are equal, otherwise we need to worry about P(c>d), which if we're agnostic about
# d, is 1-c

#c<d
Integral((1+c)(1-d)^2) = Int((1+c)(1-2d+d^2))) = Int(1-2d+d^2+c-2cd+cd^2)
              = d+cd-d^2-cd^2+d^3/3+cd^3/3
over (c,1) = (1+c-1-c+1/3+c/3)-(c+c^2-c^2-c^3+c^3/3+c^4/3)
           = (1/3+c/3-c+c^3-c^3/3-c^4/3)
           = (1+c-3c+3c^3-c^3-c^4)/3
           = [1-2c+2c^3-c^4]/3

To that we need to add
Integral(d(1-c^2)+cd) = Int(d-dc^2+cd) = d^2/2(1-c^2)+cd^2/2
            = d^2/2*(1-c^2+c)
over (c,1) =[(1-c^2+c) - (c^2-c^4+c^3)]/2
          = [1-c^2+c-c^2+c^4-c^3]/2
          = [1+c-2c^2+c^4-c^3]/2

# Add together and divide by two
Integral[P(Win|c<d)] = .5*{[1-2c+2c^3-c^4]/3+[1+c-2c^2+c^4-c^3]/2}

D(expression((1-2*c+2*c^3-c^4)/6+(1+c-2*c^2+c^4-c^3)/4), "c")


(6c^2-2-4c^3)/6+(1-4c+4c^3-3c^2)/4
Derivative wrt c:
        = .5[(-2+6c^2-4c^3)/3+(1-4c+4c^3-3c^2)/2]

Simplifying if we can
        = .5(-4+12c^2-8c^3+3-12c+12c^3-9c^2)/6
        = (-4+3-12c+c^2(12-9)+c^3(-8+12))/12
        = (-1-12c+3c^2+4c^3)/12

solving for c

(-1-12c+3c^2+4c^3)/12 = 0

f = function(c) {
  (1-2*c+2*c^3-c^4)/6+(1+c-2*c^2+c^4-c^3)/4
  #.5*((1-2*c+4*c^3-c^4)/3+(1-c+c^4-c^3)/2)
}

fprime = function(c) {
  (-1-12*c+3*c^2+4*c^3)/12
  }
# Ok, I can't be bothered to solve this, but let's plot the curve first

par(mar=c(4,4,0,0)+.1)
plot(x=seq(0,1,.001),f(seq(.0, 1, .001)), type="l", ylab="P(Win|c, c<d)", xlab="Criterion (c)")

# There's something wrong with this - If I'm either never changing, or always changing, my results
# ought to be precisely the same (and less than 50%)

optimal = optimize(f=function(x){abs(fprime(x))}, interval=c(0,1))
abline(h=f(optimal$minimum), v=optimal$minimum, col=rgb(0,0,0,.25))

# Hm, so if c < d I know c is going to be less than d, I shouldn't ever change - though my chance of success is
# only 41.6666%. That seems unlikely. It should be possible to arrive at 50%. Otherwise both of us using
# the optimal strategy we would each only win 83.3% of games. Who wins the other 16.7%?

# Put another way, if I'm winning 42%, then the opponent is winning 58%, so there's a better strategy.

# Unless -- 42% is the best I can do *if I assume the other player's choice is random*.

# Never Change sim --------------------------------------------------------


# Let's run a simulation. I'll adopt a "never change" rule. My opponent will change according to "d" which I'll
# vary over the range.

c = 0
winPercentages = c()
NGames = 10000
for(d in seq(0,1,.01)) {
  #play 1000 times at each value
  pWin = 0
    for (i in 1:NGames) {
      A1 = runif(1)
      if((C1=runif(1))<d) C1=runif(1)
      pWin = pWin + as.numeric(A1>C1)
    }
  winPercentages = c(winPercentages, pWin/NGames)
}


# Full set of values ------------------------------------------------------

# Let's allow both c and d to vary
criterion = seq(0, 1, .01)
NSamples = 4000
results = matrix(0, nrow=length(criterion), ncol=length(criterion))
ties = 0
for (i in 1:length(criterion))
  for (j in 1:length(criterion)) {
    c = criterion[i]; d=criterion[j]
    for (k in 1:NSamples){
      if ((A=runif(1))<c) A=runif(1)
      if ((B=runif(1))<d) B=runif(1)
      results[i,j]=results[i,j]+as.numeric(A>B)
      ties = ties + as.numeric(A==B)
    }
  }

results = results/NSamples

if(file.exists("GameShowResults.RData")) load("GameShowResults.RData")
if(exists("resultsStore")){ 
  storeWt = resultsStore$NSamples/(resultsStore$NSamples+NSamples)
  resultsWt = 1-storeWt
  resultsStore$results = storeWt*resultsStore$NSamples+resultsWt*results
  resultsStore$NSamples = resultsStore$NSamples + NSamples
} else
  resultsStore = list(NSamples = NSamples, results=results)

save(resultsStore, file="GameShowResults.RData")
include(lattice)
levelplot(x=resultsStore$results, at=seq(.35, .65, .05))
abline(a=0, b=1)
# Yeah, ok, that makes sense. The real "best" point is just a touch over .55 which allows up to about
# 55% success.