
##################################################################
# Test_230817
# Data Generation
##################################################################
library(BayesLogit)   # For rpg function -- install from ZIP file
library(mvtnorm)	
# library(MCMCpack)     # For Iwish update of Sigmab
library(msm)          # For tnorm function
library(spam)         # For sparse Matrix
library(splines)
library(boot)
# source("Library_v3.R")

##################################################################
# Data Generation
##################################################################

##################
# Adjacency Info #
##################
# Adjacency matrix
county.adjacency <- read.csv("county_adjacency2010.csv", header = TRUE)
USAcities = read.csv("uscities.csv")
IAcities = USAcities %>% filter(state_id=="IA")
countyname = unique(IAcities$county_name)
A = get_adj_mat(county.adjacency,countyname,c("IA"))
m<-apply(A,1,sum)	  # No. neighbors for each county

##################
# Generate Data  #
##################
set.seed(091017)
n<-nrow(A)			    # Number of spatial units
nis<-rep(24,n) 		# Number of individuals per county; here it's balanced -- 50 per county per year
# Note: may need to lower proposal variance, s, below as n_i increases 
sid<-rep(1:n,nis)
tid<-rep(1:nis[1],n)
N<-length(sid) 		  # Total number of observations

########################
#  Spatial Effects     #
########################
kappa<-.999999			  	            # Spatial dependence parameter ~ 1 for intrinsic CAR
cov<-matrix(c(.5,.10,.10,-.10,
              .10,.15,.10,.10,
              .10,.10,.5,.10,
              -.10,.10,.10,.15),4,4)# Conditional Cov of phi1 and phi2 given phi_{-i} 
Q<-as.spam(diag(m))-kappa*as.spam(A)			     
covphi<-solve(Q)%x%cov			        # 3n x 3n covariance of phis
phi<-rmvnorm(1,sigma=covphi)		    # 3n vector of spatial effects
phitmp<-matrix(phi,ncol=4,byrow=T)  # n x 3 matrix of spatial effects

true.phi1<-phi1<-phitmp[,1]-mean(phitmp[,1])   # n x 1 phi1 vector -- Centered
true.phi2<-phi2<-phitmp[,2]-mean(phitmp[,2])   # n x 1 phi2 vector, etc.
true.phi3<-phi3<-phitmp[,3]-mean(phitmp[,3])
true.phi4<-phi4<-phitmp[,4]-mean(phitmp[,4])
Phi1<-rep(phi1,nis)
Phi2<-rep(phi2,nis)
Phi3<-rep(phi3,nis)
Phi4<-rep(phi4,nis)
true.phi<-cbind(true.phi1,true.phi2,true.phi3,true.phi4)

#################
# Fixed Effects #
#################
# x <- sample(seq(0,4),N,replace=T)     # x = year (0,1,2,3,4) -- if you relabel, need to relabel below
# This is okay but not exactly balanced by year. Should be x[1:13600]<-0, x[13601:27200]<-2, etc Need to update
 
tpop = USAcities %>% filter(state_id=="IA") %>% group_by(county_fips) %>% summarise(tpop=sum(population)) %>% .[,2] %>% unlist %>% as.numeric
x = rep(log(tpop),nis)
x = (x-mean(x))/sd(x)
# set.seed(2023); x = rnorm(N)
t <- tid / max(tid)
X <-cbind(1,x)                       # Design matrix, can add additional covariates (e.g., race, age, gender)
Xtilde <-cbind(1,x,sin(t))
p <- ncol(Xtilde)

# Binomial part
true.alpha<-rep(0.1,p)
true.alpha[1:2]<-c(-0.25,0.25)
# true.alpha <- c(-0.25,0.25,-0.5,0.25)
eta1<-Xtilde%*%true.alpha+Phi1+Phi2*t
pi<-inv.logit(eta1)        # 1-pr("structural zero")
u<-rbinom(N,1,pi)                  # at-risk indicator
N1<-sum(u)          
pstruct0<-1-mean(u)                # Proportion of structural zeros

# Count Part 
true.beta <- rep(0.1,p)
true.beta[1:2]<-c(.5,-.25)
# true.beta<- c(-0.5,-0.25,0.5,-0.25)
tmp<-u
tmp[u==0]<-0                       # If in structural group then set tmp to 0
nis1<-tapply(tmp,sid,sum)           # Number of at risk observations in each county

eta2<-Xtilde[u==1,]%*%true.beta+Phi3[u==1]+Phi4[u==1]*t[u==1] # Linear predictor for count part
true.r<-1.25                               # NB dispersion 
psi<-exp(eta2)/(1+exp(eta2))       # Prob of success
mu<-true.r*psi/(1-psi)                  # NB mean

y<-rep(0,N)                        # Response
y[u==1]<-rnbinom(N1,true.r,mu=mu)       # If at risk, draw from NB
pzero<-length(y[y==0])/N           # Proportion of zeros
# pzero
# pstruct0/pzero
# table(y)
# mean(mu)
# quantile(tapply(u,id,sum))         # Number of at-risk observations per county *for all 5 years*
# quantile(y)

tmp<-table(y)/N*100  # convert to %s (divide by N multiply by 100)
# barplot(tmp, ylab="Percent",xlab="Count",col="lightblue")
