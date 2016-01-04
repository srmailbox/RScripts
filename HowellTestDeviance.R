# Analysis of contingency tables conditional on the row totals

#read in the data

origdata <- matrix(c(35,9,60,41), nrow = 2, byrow = TRUE)   #Civil Union example
# Alternative data sets follow--You must set the default directory
#origdata <- read.table("AlteredVote.dat", header = TRUE, nrows = 2)
#origdata <- read.table("Visintainer.dat", nrows = 2, header = TRUE)  #2 x 3 example

result <- chisq.test(origdata, correct = F)
obtchisq <- result$statistic
expdata=matrix(c(0,0,0,0), nrow=2)
for (i in 1:nrow(origdata))
  for(j in 1:ncol(origdata))
    expdata[i,j] = rowSums(origdata)[i]*colSums(origdata)[j]/sum(origdata)
obtdev = sum((expdata-origdata)^2)
dfs <- result$parameter
p <- result$p.value
N <- sum(origdata)
rowmarg <- rowSums(origdata)
nrow <- length(rowmarg)
colmarg <- colSums(origdata)
ncol <- length(colmarg)
colprob <- colmarg/N


nreps <- 10000
chisquare <- numeric(nreps)
results <- numeric(nreps)
deviance = numeric(nreps)
extreme <- 0
# Get cumulative proportions over columns
# We might find that 60% of the obs are in col1, 25% in col2, and 15% in col3
# Then draw as many numbers as the row marginal, and assign to col based on
# those probabilities. That does not mean that we will always have 60, 25, and
# 15%, because assignment depends on whether the random number exceeds some value.
cum <- numeric()
cum[1] <- colprob[1]
for (j in 2:ncol) {cum[j] <- cum[j-1] + colprob[j]}

# Now resample 10,000 times 
count <- c()
countx <- c()
for (i in 1:nreps) {
  
  randomtable <- c()
  for (j in 1:nrow) {
    randnum <- runif(rowmarg[j],0,1) 
    for (j in 1:ncol) {count[j] <- length(randnum[randnum <= cum[j]])}
    countx[1] <- count[1]
    for (k in 2:ncol) {countx[k] <- count[k] - count[k-1]} 
    randomtable <- rbind(randomtable, countx)
  }
  chisquare[i] <- chisq.test(randomtable, correct = FALSE)$statistic
  deviance[i] <- sum((randomtable-origdata)^2)
  
}
chisignif <- length(chisquare[chisquare >= obtchisq]) /nreps
devsignif <- length(deviance[deviance <= obtdev]) /nreps

table(chisquare>=obtchisq, deviance<=obtdev)
par(mfrow=c(2,2))
hist(chisquare, breaks = 50, main = "Distribution of chi-square under null")
hist(deviance, breaks = 50, main = "Distribution of deviance under null")
library(car)
qqPlot(chisquare, dist = "chisq", df = dfs)
qqPlot(deviance, dist="chisq", df=dfs)
qqPlot(deviance, dist="f", df1=3, df2=144)
cat("Pearson's chi-square statistic is ",obtchisq,"\n")
cat("That chi-square has a p value of ",p, "\n")
cat("\n")
cat("The deviance statistic is ",obtdev,"\n")
cat("That deviance has a p value of ",pt(sqrt(obtdev), df=144), "\n")

cat("The proportion of chi-square values greater than the obtained chi-square is  ", chisignif, "\n")
cat("The proportion of deviance values less than the obtained deviance is  ", devsignif, "\n")


# > source("C:/Documents and Settings/David Howell/My Documents/Professional/Chi Square/rowfixed.r")
#Pearson's chi-square statistic is  5.502327 
#That chi-square has a p value of  0.01899118 
#
#The proportion of chi-square values greater than the obtained chi-square is   0.0189 
#

###########
#   likelihood ratio chi-sq

LRchisq <- function(data) {
  test <- chisq.test(data)
  E <- test$expected
  O <- test$observed
  df <- test$parameter
  sigma = 0
  nrows <- nrow(data)
  ncols <- ncol(data)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      sigma <- sigma + O[i,j]*log(O[i,j]/E[i,j])
    }
  }
  Gsquare <- 2*sigma
  
  
}
Gsquare <- LRchisq(origdata)

cat("The likelihood ratio chi-square statistic is ",Gsquare,"\n")
pLR <- pchisq(Gsquare, dfs, lower.tail = FALSE)
cat("That chi-square has a p value of ",pLR, "\n")


# Artificial Data Point ---------------------------------------------------

##### Try a version where 0 column or row totals are replaced with 1.
# Analysis of contingency tables conditional on the row totals

#read in the data
# OK, what if we just add one to a row.
origdata <- matrix(c(1,0,60,41), nrow = 2, byrow = TRUE)   #Civil Union example
#origdata <- matrix(c(1+61/102, 41/102, 60, 41), nrow=2, byrow=TRUE)
# Alternative data sets follow--You must set the default directory
#origdata <- read.table("AlteredVote.dat", header = TRUE, nrows = 2)
#origdata <- read.table("Visintainer.dat", nrows = 2, header = TRUE)  #2 x 3 example

(result <- chisq.test(origdata, correct = F))
#(result <- chisq.test(altdata, correct = F))
obtchisq <- result$statistic
dfs <- result$parameter
p <- result$p.value
N <- sum(origdata)
rowmarg <- rowSums(origdata)
nrow <- length(rowmarg)
colmarg <- colSums(origdata)
ncol <- length(colmarg)
colprob <- colmarg/N


nreps <- 10000
chisquare <- numeric(nreps)
results <- numeric(nreps)
extreme <- 0
# Get cumulative proportions over columns
# We might find that 60% of the obs are in col1, 25% in col2, and 15% in col3
# Then draw as many numbers as the row marginal, and assign to col based on
# those probabilities. That does not mean that we will always have 60, 25, and
# 15%, because assignment depends on whether the random number exceeds some value.
cum <- numeric()
cum[1] <- colprob[1]
for (j in 2:ncol) {cum[j] <- cum[j-1] + colprob[j]}

# Now resample 10,000 times 
count <- c()
countx <- c()
for (i in 1:nreps) {
  
  randomtable <- c()
  for (j in 1:nrow) {
    randnum <- runif(rowmarg[j],0,1) 
    for (j in 1:ncol) {count[j] <- length(randnum[randnum <= cum[j]])}
    countx[1] <- count[1]
    for (k in 2:ncol) {countx[k] <- count[k] - count[k-1]} 
    randomtable <- rbind(randomtable, countx)
  }
  
  chisquare[i] <- chisq.test(randomtable, correct = FALSE)$statistic

}

chisignif <- length(chisquare[chisquare >= obtchisq]) /nreps

Gsquare <- LRchisq(origdata)

cat("The likelihood ratio chi-square statistic is ",Gsquare,"\n")
pLR <- pchisq(Gsquare, dfs, lower.tail = FALSE)
cat("That chi-square has a p value of ",pLR, "\n")

par(mfrow=c(2,2))
hist(chisquare, breaks = 50, main = "Distribution of chi-square under null")
library(car)
qqPlot(chisquare, dist = "chisq", df = dfs)
cat("Pearson's chi-square statistic is ",obtchisq,"\n")
cat("That chi-square has a p value of ",p, "\n")
cat("\n")

cat("The proportion of chi-square values greater than the obtained chi-square is  ", chisignif, "\n")


# Shields Heeler ----------------------------------------------------------


####### try shields heeler

ShieldsHeelerLRTest=function(data) {
  test = chisq.test(data)
  O = test$observed
  E = round(test$expected)
  # One problem here is that E can have a different sample size, which is not really permissible.
  while(sum(E)!=sum(O)) {
     # need to find rows and columns that have the wrong totals
     rowDiffs = rowSums(E)-rowSums(O)
     colDiffs = colSums(E)-colSums(O)
     
     # Starting with the most egregious cell (the cell with the largest difference)
     row = order(abs(rowDiffs), decreasing=T)
     col = order(abs(colDiffs), decreasing=T)

     # adjust that cell up or down depending on the direction of the problem
     E[row[1], col[1]]=E[row[1], col[1]]+sign(sum(O)-sum(E))
   }
  
  dfs = test$parameter - sum(E<=1)/2
  logO = sum(log(factorial(O)))
  logE = sum(log(factorial(E)))
  stat = 2*(logO-logE)
  return(list(statistic=2*(logO-logE), expected=E, observed=O, p=1-pchisq(stat, df=dfs), df=dfs))

}
shdata = matrix(c(3,1,11,17,4,25,6,5,16,10,6,2), nrow=3)
shsparse=shdata
shsparse[2,]=c(0,1,0,0)

result <- ShieldsHeelerLRTest(shdata)
obtsh <- result
result <- chisq.test(altdata, correct = F)
obtchisq = result$statistic
dfs <- (nrow(shsparse)-1)*(ncol(shsparse)-1)
N <- sum(shsparse)
rowmarg <- rowSums(shsparse)
nrow <- length(rowmarg)
colmarg <- colSums(shsparse)
ncol <- length(colmarg)
colprob <- colmarg/N

nreps <- 10000
shchis <- numeric(nreps)
results <- numeric(nreps)
extreme <- 0
# Get cumulative proportions over columns
# We might find that 60% of the obs are in col1, 25% in col2, and 15% in col3
# Then draw as many numbers as the row marginal, and assign to col based on
# those probabilities. That does not mean that we will always have 60, 25, and
# 15%, because assignment depends on whether the random number exceeds some value.
cum <- numeric()
cum[1] <- colprob[1]
for (j in 2:ncol) {cum[j] <- cum[j-1] + colprob[j]}

# Now resample 10,000 times 
count <- c()
countx <- c()
for (i in 1:nreps) {
  
  randomtable <- c()
  for (j in 1:nrow) {
    randnum <- runif(rowmarg[j],0,1) 
    for (j in 1:ncol) {count[j] <- length(randnum[randnum <= cum[j]])}
    countx[1] <- count[1]
    for (k in 2:ncol) {countx[k] <- count[k] - count[k-1]} 
    randomtable <- rbind(randomtable, countx)
  }
  
  chisquare[i] <- chisq.test(randomtable, correct = FALSE)$statistic
  shchis[i] <- ShieldsHeelerLRTest(randomtable)
  
}

chisignif <- length(chisquare[chisquare >= obtchisq]) /nreps
shsignif <- length(shchis[shchis>= obtsh]) /nreps


maxdata = matrix(c(6,5,1,7,1,5,3,0,1,0,2,0,1,3,0), nrow=3, byrow=T)
maxfull=maxdata
maxfull[3,]=c(20,0,6)
maxfull[5,]=c(20,0,0)

#maxdata=maxfull
result <- ShieldsHeelerLRTest(maxdata)
obtsh <- result$statistic
result <- chisq.test(maxfull, correct = F)
obtchisq = result$statistic
dfs <- (nrow(maxdata)-1)*(ncol(maxdata)-1)
N <- sum(maxdata)
rowmarg <- rowSums(maxdata)
nrow <- length(rowmarg)
colmarg <- colSums(maxdata)
ncol <- length(colmarg)
colprob <- colmarg/N

nreps <- 10000
chisquare=numeric(nreps)
shchis <- numeric(nreps)
results <- numeric(nreps)
extreme <- 0
# Get cumulative proportions over columns
# We might find that 60% of the obs are in col1, 25% in col2, and 15% in col3
# Then draw as many numbers as the row marginal, and assign to col based on
# those probabilities. That does not mean that we will always have 60, 25, and
# 15%, because assignment depends on whether the random number exceeds some value.
cum <- numeric()
cum[1] <- colprob[1]
for (j in 2:ncol) {cum[j] <- cum[j-1] + colprob[j]}

# Now resample 10,000 times 
count <- c()
countx <- c()
rtables = list(nreps)
etables = list(nreps)
shdfs = numeric(nreps)
for (i in 1:nreps) {
  
  randomtable <- c()
  for (r in 1:nrow) {
    randnum <- runif(rowmarg[r],0,1) 
    for (j in 1:ncol) {count[j] <- length(randnum[randnum <= cum[j]])}
    countx[1] <- count[1]
    for (k in 2:ncol) {countx[k] <- count[k] - count[k-1]} 
    randomtable <- rbind(randomtable, countx)
  }
  rtables[[i]] = randomtable
  
  chisquare[i] <- chisq.test(randomtable, correct = FALSE)$statistic
  shchis[i] <- ShieldsHeelerLRTest(randomtable)$statistic
  etables[[i]] = ShieldsHeelerLRTest(randomtable)$expected
  shdfs[i] =  ShieldsHeelerLRTest(randomtable)$df
}

chisignif <- length(chisquare[chisquare >= obtchisq]) /nreps
shsignif <- length(shchis[shchis>= obtsh]) /nreps

negIndex=c(1:10000)[shchis<0]

Ns = c()
for (i in 1:10000) {
  Ns = rbind(Ns, c(sum(rtables[[i]]), sum(etables[[i]])))
}