# Random Slope Gibbs.r 
# Random Slope Time for UNBALANCED Longitudinal design
# Uses Sparse Matrix (SPAM) Routines to invert random effect covariance
# March 30, 2010
##################################

# Required Packages
library(mvtnorm)
library(lme4)
library(MCMCpack) # Iwish for udpating Sigma.b
library(spam)

###################
# GENERATE DATA  #
###################

set.seed(033010)

n<-100					# sample size
nis<-sample(1:10,n,replace=T) 	# number obs per subject 
id<-rep(1:n,nis)
N<-length(id)
k<-2						# Number of parms (including intercept)

############################################
# Covariate = Time of visit (1 to 40 days) #
############################################
t<-rep(0,N)
for (i in 1:n) t[id==i]<-sort(sample(1:40,nis[i]))
X<-cbind(rep(1,N),t)

##############
# Parameters #
##############
beta<-c(1,2.25)				# Fixed effects
sigma2<-2					# within-subj error variance
Sigma.b<-matrix(c(2,1,1,2),2,2)	# random effect cov

#############
# Random Fx #
#############
b<-rmvnorm(n,sigma=Sigma.b)		# Random effects
b1<-rep(b[,1],nis)			# Concatenate to length N
b2<-rep(b[,2],nis)

############
# Response #
############
y<-X%*%beta+b1+b2*X[,2]+rnorm(N,0,sqrt(sigma2)) 

fit<-lmer(y~X-1+(t|id))  		# RMLE Fit

###########
# Priors  #
###########
beta0<-rep(0,k)			# Prior Mean for beta
prec0<-diag(.01,k)		# Prior Precision Matrix of beta (vague), independent
d0<-g0<-.001			    # Hyperpriors for tau
nu0<-3				        # DF for Wishart prior on Sigma.b
c0<-diag(2)			    # Scale matrix for IW Prior on Sigma.b

#########
# Inits #
#########
tau<-1				      # Error precision = 1/sigma2
b<-rep(0,2*n)			# Random effects (int and slope)
Taub<-diag(2)			# Random Effects Prec Matrix
beta<-rep(0,k)			# Posterior mean and var of beta
Z<-matrix(0,N,2*n)	# Random effect design used for updating b
for (i in 1:n) Z[id==i,(2*(i-1)+1):(2*i)]<-X[id==i,]

#################
# Store Results #
#################
nsim<-1000 
Betas<-matrix(0,nsim,k)	# Fixed Effects
taus<-rep(0,nsim)		# Error Precision Parms
Sigma.bs<-matrix(0,nsim,4)	# Random effect Cov

###############################
# Fixed Posterior Hyperparms 	#
#    for tau and taub		#
###############################
d<-d0+N/2
nu<-nu0+n				# NOTE: nu0 + n NOT n/2 as in gamma distbn

###################
# GIBBS SAMPLER	#
################### 
tmp<-proc.time()
for (i in 1:nsim) {
  # Update Beta 
  vbeta<-solve(prec0+tau*crossprod(X,X))
  bstar<-b-mean(b)			# Center b to reduce correlation with beta 
  # and improve mixing
  mbeta<-vbeta%*%(prec0%*%beta0 + tau*crossprod(X,y-Z%*%bstar))
  Betas[i,]<-beta<-c(rmvnorm(1,mbeta,vbeta))
  
  # Update b
  precb<-diag(n)%x%Taub+tau*crossprod(Z,Z)  # Posterior Precision
  mb<-tau*crossprod(Z,y-X%*%beta)           # Likelihood Contribution to Post. Mean
  b<- rmvnorm.canonical(1,mb,precb)[1,]     # Update without inverting 
  bmat<-matrix(b,ncol=2,byrow=T)  			     # Put in n x 2 matrix form for updating taub
  
  # Update tau
  zb<-Z%*%b		
  g<-g0+crossprod(y-X%*%beta-zb,y-X%*%beta-zb)/2
  taus[i]<-tau<-rgamma(1,d,g)
  
  # Update Taub 
  Sigma.b<-riwish(nu,c0+crossprod(bmat,bmat))
  Sigma.bs[i,]<-c(Sigma.b)  				# Store Sigma.b values
  Taub<-solve(Sigma.b)
  
  if (i%%50==0) print(i) 
} 
proc.time()-tmp
###########
# Results #
###########
mbeta<-apply(Betas[501:nsim,],2,mean)
msigma.e2<-mean(1/taus[510:nsim])
msigma.b2<-apply(Sigma.bs[501:nsim,],2,mean)
cat(c("REML Fit","\n"))
fit
cat(c("Posterior Mean of Beta = ",mbeta,"\n"))
cat(c("Posterior Mean of Sigma.e^2 = ",msigma.e2,"\n"))
cat(c("Posterior Mean of Sigma.b^2 = ",msigma.b2,"\n"))

par(mfrow=c(2,1), mar=c(5,4,2,2)+.1)
plot(501:nsim,Betas[501:nsim,1], type="l",col="lightgreen") 
abline(h=mbeta[1],col=4)
plot(501:nsim,Betas[501:nsim,2], type="l",col="lightgreen") 
abline(h=mbeta[2],col=4)

