library(dplyr)
library(reshape)
library(spam)
library(BayesLogit)   # For rpg function -- install from ZIP file
library(mvtnorm)	
library(MCMCpack)     # For Iwish update of Sigmab
library(msm)          # For tnorm function
library(spam)         # For sparse Matrix
library(splines)
library(usmap)        # For generating US maps
library(viridis)      # For generating colorscheme
library(ggplot2)      # For creating map graphics
library(HDInterval)   # For computing credibility interval
library(bayesplot)    # For generating trace plot
library(boot)
library("pscl")
library("MASS")
# library(NBZIMM)
library("lme4")
library(matrixcalc)
library(gtsummary)
library(gt)


get_adj_mat = function(county.adjacency,Countyvec,Statevec){
  
  county.names1 <- trimws(unlist(lapply(county.adjacency$Countyname, function(x) unlist(strsplit(as.character(x), ","))[1])))
  county.names2 <- trimws(unlist(lapply(county.adjacency$neighborname, function(x) unlist(strsplit(as.character(x), ","))[1])))
  state.abbs1 <- trimws(unlist(lapply(county.adjacency$Countyname, 
                                      function(x) tail(unlist(strsplit(as.character(x), ",")),n=1))))
  state.abbs2 <- trimws(unlist(lapply(county.adjacency$neighborname, 
                                      function(x) tail(unlist(strsplit(as.character(x), ",")),n=1))))
  
  county.names1.full <- unlist(lapply(1:length(county.names1), function(x) paste(county.names1[x], state.abbs1[x], sep="-")))
  county.names2.full <- unlist(lapply(1:length(county.names2), function(x) paste(county.names2[x], state.abbs2[x], sep="-")))
  
  county.list <- paste(Countyvec,Statevec,sep="-")
  n.county <- length(county.list)
  
  county.adj.mat <- matrix(0, nrow=n.county, ncol=n.county)
  for (i in 1:dim(county.adjacency)[1]) {
    inx.county <- which(county.list == county.names1.full[i])
    inx.neighbor.county <- which(county.list == county.names2.full[i])
    county.adj.mat[inx.county, inx.neighbor.county] = 1
    county.adj.mat[inx.neighbor.county, inx.county] = 1
  }
  county.adj.mat
}

BSTP = function(y, X, A, nt, oind = NULL, nchain=3, nsim=100, nburn=0, nthin=1){
  
  n<-nrow(A)			    # Number of spatial units
  nis<-rep(nt,n) 		# Number of individuals per county; here it's balanced -- 50 per county per year
  # Note: may need to lower proposal variance, s, below as n_i increases 
  sid<-rep(1:n,nis)
  tid<-rep(1:nis[1],n)
  N<-length(sid) 		  # Total number of observations
  p = ncol(X)
  
  ##########   
  # Priors #
  ##########
  beta0<-rep(0,p)
  T0a<-diag(.01,p)
  T0b<-diag(.01,p)         # Uniform or Gamma(0.01,0.01) prior for r depending on MH or Gibbs
  s<-0.0003                # Proposal variance  -- NOTE: may need to lower this as n_i increases
  kappa<-.999999	
  Q<-as.spam(diag(apply(A,1,sum)))-kappa*as.spam(A)
  
  ############
  # Num Sims #
  ############
  lastit<-(nsim-nburn)/nthin	# Last stored value
  
  ############
  # Store #
  ############
  Beta<-array(0,c(lastit,p,nchain))
  colnames(Beta) <- colnames(X)
  R<-R2<-matrix(0,lastit,nchain)
  Sigphi<-array(0,c(lastit,4,nchain))
  PHI3<-PHI4<-array(0,c(lastit,n,nchain))
  Eta<-array(0,c(lastit,N,nchain))
  
  for(chain in 1:3){
    
    #########   
    # Inits #
    #########
    set.seed(2023+chain); beta<-rnorm(p)
    phi_init<-rmvnorm(1,sigma=diag(.1,2*n))	  # Random effects
    phi_init<-matrix(phi_init,ncol=2,byrow=T)  # n x 3 matrix of spatial effects
    phi3<-phi_init[,1]
    phi4<-phi_init[,2]
    phi3<-phi3-mean(phi3)
    phi4<-phi4-mean(phi4)
    Phi3<-rep(phi3,nis)
    Phi4<-rep(phi4,nis)
    
    phimat<-cbind(phi3,phi4)
    Sigmaphi<-cov(phimat)
    r<-1
    Acc<-0  
    N0<-length(y[y==0])      # Number of observed 0's
    q<-rep(.5,N)             # 1-p=1/(1+exp(X%*%alpha)), used for updating y1
    
    
    ########
    # MCMC #
    ########
    
    for (i in 1:nsim){
      
      # Fix r
      r <- 1
      r2 <- 1
      
      # Update beta
      eta<-X%*%beta+Phi3+Phi4*t
      w<-rpg(N,y+r,eta)                               # Polya weights
      z<-(y-r)/(2*w)                                      
      v<-solve(crossprod(X*sqrt(w))+T0b)               
      m<-v%*%(T0b%*%beta0+t(sqrt(w)*X)%*%(sqrt(w)*(z-Phi3-Phi4*t)))  
      beta<-c(rmvnorm(1,m,v))
      
      # Update phi3
      priorprec<-as.numeric(1/(Sigmaphi[1,1]-Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1])%*%Sigmaphi[-1,1]))*Q # Prior Prec of phi3|phi1,phi2,phi4
      priormean<-diag(n)%x%(Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1]))%*%c(t(phimat[,-1]))      # Prior mean of phi3|phi1,phi2,phi4
      prec<-priorprec+as.spam(diag(tapply(w,sid,sum),n,n))
      tmp<-tapply(w*(z-X%*%beta-Phi4*t),sid,sum)
      m<-c(priorprec%*%priormean)+tmp
      if(is.positive.definite(prec%>%as.matrix)) phi3<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi3<-phi3-mean(phi3)
      Phi3<-rep(phi3,nis)
      
      # Update phi4
      priorprec<-as.numeric(1/(Sigmaphi[2,2]-Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2])%*%Sigmaphi[-2,2]))*Q # Prior Prec of phi4|phi1,phi2,phi3
      priormean<-diag(n)%x%(Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2]))%*%c(t(phimat[,-2]))      # Prior mean of phi4|phi1,phi2,phi3
      
      prec<-priorprec+as.spam(diag(tapply(w*t^2,sid,sum),n,n))
      tmp<-tapply(t*w*(z-X%*%beta-Phi3),sid,sum)
      m<-c(priorprec%*%priormean)+tmp
      if(is.positive.definite(prec%>%as.matrix)) phi4<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi4<-phi4-mean(phi4)
      Phi4<-rep(phi4,nis)
      
      # Update Sigma.phi
      phimat<-cbind(phi3,phi4)
      try({
        Sigmaphi<-riwish(2+n-1,diag(2)+t(phimat)%*%Q%*%phimat)  
      })
      
      # Store
      if (i> nburn & i%%nthin==0) {
        j<-(i-nburn)/nthin
        Beta[j,,chain]<-beta
        R[j,chain]<-r		
        R2[j,chain]<-r2	
        Sigphi[j,,chain]<-c(Sigmaphi)
        PHI3[j,,chain]<-phi3
        PHI4[j,,chain]<-phi4
        Eta[j,,chain]<-eta
      }
      
      # if (i%%10==0) print(paste(chain, "/", nchain,"chain | ",round(i/nsim*100,2),"% completed"))
      if (i%%10==0) print(paste(chain, "/", nchain,"chain | ",round(i/nsim*100,2),"% completed |","Test:",conv.test(R[,chain])))
    }  
  }
  
  list.params = list(Beta=Beta, Sigphi=Sigphi, R=R, PHI3=PHI3, PHI4=PHI4, Eta=Eta)
  return(list.params)
}

BSTNB = function(y, X, A, nt, oind = NULL, nchain=3, nsim=100, nburn=0, nthin=1){
  
  n<-nrow(A)			    # Number of spatial units
  nis<-rep(nt,n) 		# Number of individuals per county; here it's balanced -- 50 per county per year
  # Note: may need to lower proposal variance, s, below as n_i increases 
  sid<-rep(1:n,nis)
  tid<-rep(1:nis[1],n)
  t <- tid / max(tid)
  N<-length(sid) 		  # Total number of observations
  p = ncol(X)
  
  ##########   
  # Priors #
  ##########
  beta0<-rep(0,p)
  T0a<-diag(.01,p)
  T0b<-diag(.01,p)         # Uniform or Gamma(0.01,0.01) prior for r depending on MH or Gibbs
  s<-0.0003                # Proposal variance  -- NOTE: may need to lower this as n_i increases
  kappa<-.999999
  Q<-as.spam(diag(apply(A,1,sum)))-kappa*as.spam(A)
  
  ############
  # Num Sims #
  ############
  lastit<-(nsim-nburn)/nthin	# Last stored value
  
  ############
  # Store #
  ############
  Beta<-array(0,c(lastit,p,nchain))
  colnames(Beta) <- colnames(X)
  R<-R2<-matrix(0,lastit,nchain)
  Sigphi<-array(0,c(lastit,4,nchain))
  PHI3<-PHI4<-array(0,c(lastit,n,nchain))
  Eta<-array(0,c(lastit,N,nchain))
  
  for(chain in 1:3){
    
    #########   
    # Inits #
    #########
    set.seed(2023+chain); beta<-rnorm(p)
    phi_init<-rmvnorm(1,sigma=diag(.1,2*n))	  # Random effects
    phi_init<-matrix(phi_init,ncol=2,byrow=T)  # n x 3 matrix of spatial effects
    phi3<-phi_init[,1]
    phi4<-phi_init[,2]
    phi3<-phi3-mean(phi3)
    phi4<-phi4-mean(phi4)
    Phi3<-rep(phi3,nis)
    Phi4<-rep(phi4,nis)
    
    phimat<-cbind(phi3,phi4)
    Sigmaphi<-cov(phimat)
    r<-1
    Acc<-0  
    N0<-length(y[y==0])      # Number of observed 0's
    q<-rep(.5,N)             # 1-p=1/(1+exp(X%*%alpha)), used for updating y1
    
    
    ########
    # MCMC #
    ########
    
    for (i in 1:nsim){
      
      # Update r
      rnew<-rtnorm(1,r,sqrt(s),lower=0)       # Treat r as continuous
      ratio<-sum(dnbinom(y,rnew,q,log=T))-sum(dnbinom(y,r,q,log=T))+   
        dtnorm(r,rnew,sqrt(s),0,log=T) - dtnorm(rnew,r,sqrt(s),0,log=T)   # Uniform Prior for R 
      # Proposal not symmetric 
      if (log(runif(1))<ratio) {
        r<-rnew
        Acc<-Acc+1
      }
      
      # Update r2 using Gibbs as in Dadaneh et al and Zhou and Carin
      # Update latent counts, l        
      l<-rep(0,N)
      ytmp<-y
      for(j in 1:N) l[j]<-sum(rbinom(ytmp[j],1,round(r/(r+1:ytmp[j]-1),6))) # Could try to avoid loop; rounding avoids numerical stability
      
      # Update r from conjugate gamma distribution given l and psi
      # psi<-exp(eta2)/(1+exp(eta2))
      # r2<-rgamma(1,0.01+sum(l),0.01-sum(log(1-psi)))
      
      # Update beta
      eta<-X%*%beta+Phi3+Phi4*t
      w<-rpg(N,y+r,eta)                               # Polya weights
      z<-(y-r)/(2*w)                                      
      v<-solve(crossprod(X*sqrt(w))+T0b)               
      m<-v%*%(T0b%*%beta0+t(sqrt(w)*X)%*%(sqrt(w)*(z-Phi3-Phi4*t)))  
      beta<-c(rmvnorm(1,m,v))
      
      # Update phi3
      priorprec<-as.numeric(1/(Sigmaphi[1,1]-Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1])%*%Sigmaphi[-1,1]))*Q # Prior Prec of phi3|phi1,phi2,phi4
      priormean<-diag(n)%x%(Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1]))%*%c(t(phimat[,-1]))      # Prior mean of phi3|phi1,phi2,phi4
      prec<-priorprec+as.spam(diag(tapply(w,sid,sum),n,n))
      tmp<-tapply(w*(z-X%*%beta-Phi4*t),sid,sum)
      m<-c(priorprec%*%priormean)+tmp
      if(is.positive.definite(prec%>%as.matrix)) phi3<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi3<-phi3-mean(phi3)
      Phi3<-rep(phi3,nis)
      
      # Update phi4
      priorprec<-as.numeric(1/(Sigmaphi[2,2]-Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2])%*%Sigmaphi[-2,2]))*Q # Prior Prec of phi4|phi1,phi2,phi3
      priormean<-diag(n)%x%(Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2]))%*%c(t(phimat[,-2]))      # Prior mean of phi4|phi1,phi2,phi3
      
      prec<-priorprec+as.spam(diag(tapply(w*t^2,sid,sum),n,n))
      tmp<-tapply(t*w*(z-X%*%beta-Phi3),sid,sum)
      m<-c(priorprec%*%priormean)+tmp
      if(is.positive.definite(prec%>%as.matrix)) phi4<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi4<-phi4-mean(phi4)
      Phi4<-rep(phi4,nis)
      
      # Update Sigma.phi
      phimat<-cbind(phi3,phi4)
      try({
        Sigmaphi<-riwish(2+n-1,diag(2)+t(phimat)%*%Q%*%phimat)  
      })
      
      # Store
      if (i> nburn & i%%nthin==0) {
        j<-(i-nburn)/nthin
        Beta[j,,chain]<-beta
        R[j,chain]<-r		
        # R2[j,chain]<-r2	
        Sigphi[j,,chain]<-c(Sigmaphi)
        PHI3[j,,chain]<-phi3
        PHI4[j,,chain]<-phi4
        Eta[j,,chain]<-eta
      }
      
      # if (i%%10==0) print(paste(chain, "/", nchain,"chain | ",round(i/nsim*100,2),"% completed"))
      if (i%%10==0) print(paste(chain, "/", nchain,"chain | ",round(i/nsim*100,2),"% completed |","Test:",conv.test(R[,chain])))
      
    }  
  }
  
  list.params = list(Beta=Beta, R=R, Sigphi=Sigphi, PHI3=PHI3, PHI4=PHI4, Eta=Eta)
  return(list.params)
}

BSTZINB = function(y, X, A, nt, oind = NULL, LinearT = TRUE, nchain=3, nsim=100, nburn=0, nthin=1){
  
  N = length(y)
  n <- nrow(A)			    # Number of spatial units
  nt <- N/n
  nis<-rep(nt,n) 		# Number of individuals per county; here it's balanced -- 50 per county per year
  # Note: may need to lower proposal variance, s, below as n_i increases 
  sid<-rep(1:n,nis)
  tid<-rep(1:nis[1],n)
  t <- tid / max(tid)
  if(LinearT==T){
    Xtilde = cbind(X,t)
  }else{
    Tbs = bs(t,df=7)
    colnames(Tbs) <- paste0("t",colnames(Tbs))
    Xtilde = cbind(X,Tbs)
  }
  p = ncol(Xtilde)
  
  ##########   
  # Priors #
  ##########
  alpha0<-beta0<-rep(0,p)
  T0a<-diag(.01,p)
  T0b<-diag(.01,p)         # Uniform or Gamma(0.01,0.01) prior for r depending on MH or Gibbs
  s<-0.0003                # Proposal variance  -- NOTE: may need to lower this as n_i increases
  kappa<-.999999
  Q<-as.spam(diag(apply(A,1,sum)))-kappa*as.spam(A)
  
  ############
  # Num Sims #
  ############
  lastit<-(nsim-nburn)/nthin	# Last stored value
  
  ############
  # Store #
  ############
  Beta<-Alpha<-array(0,c(lastit,p,nchain))
  colnames(Beta) <- colnames(Alpha) <- colnames(Xtilde)
  R<-R2<-matrix(0,lastit,nchain)
  Sigphi<-array(0,c(lastit,16,nchain))
  PHI1<-PHI2<-PHI3<-PHI4<-array(0,c(lastit,n,nchain))
  I<-Eta1<-Eta2<-array(0,c(lastit,N,nchain))
  
  for(chain in 1:3){
    
    #########   
    # Inits #
    #########
    
    set.seed(2023+chain); beta<-alpha<-rnorm(p)
    phi_init<-rmvnorm(1,sigma=diag(.1,16*n))	  # Random effects
    phi_init<-matrix(phi_init,ncol=16,byrow=T)  # n x 3 matrix of spatial effects
    phi1<-phi_init[,1]
    phi2<-phi_init[,2]
    phi3<-phi_init[,3]
    phi4<-phi_init[,4]
    phi1<-phi1-mean(phi1)
    phi2<-phi2-mean(phi2)
    phi3<-phi3-mean(phi3)
    phi4<-phi4-mean(phi4)
    Phi1<-rep(phi1,nis)
    Phi2<-rep(phi2,nis)
    Phi3<-rep(phi3,nis)
    Phi4<-rep(phi4,nis)
    
    phimat<-cbind(phi1,phi2,phi3,phi4)
    Sigmaphi<-cov(phimat)
    
    r<-1
    Acc<-0  
    y1<-rep(0,N)             # At risk indicator (this is W in paper)
    y1[y>0]<-1               # If y>0, then at risk w.p. 1
    N0<-length(y[y==0])      # Number of observed 0's
    q<-rep(.5,N)             # 1-p=1/(1+exp(X%*%alpha)), used for updating y1
    
    ########
    # MCMC #
    ########
    
    for (i in 1:nsim){
      
      # Update alpha  
      mu<-Xtilde%*%alpha+Phi1+Phi2*t
      w<-rpg(N,1,mu)
      z<-(y1-1/2)/w           
      v<-solve(crossprod(sqrt(w)*Xtilde)+T0a)  
      m<-v%*%(T0a%*%alpha0+t(sqrt(w)*Xtilde)%*%(sqrt(w)*(z-Phi1-Phi2*t)))
      alpha<-c(rmvnorm(1,m,v))
      
      # Update phi1
      priorprec<-as.numeric(1/(Sigmaphi[1,1]-Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1])%*%Sigmaphi[-1,1]))*Q # Prior Prec of phi1|phi2,phi3,phi4
      priormean<-diag(n)%x%(Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1]))%*%c(t(phimat[,-1]))      # Prior mean of phi1|phi2,phi3,phi4
      prec<-priorprec+as.spam(diag(tapply(w,sid,sum),n,n))	
      m<-c(priorprec%*%priormean)+tapply(w*(z-Xtilde%*%alpha-Phi2*t),sid,sum)
      if(is.positive.definite(prec%>%as.matrix)) phi1<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi1<-phi1-mean(phi1)
      Phi1<-rep(phi1,nis)
      
      # Update phi2
      priorprec<-as.numeric(1/(Sigmaphi[2,2]-Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2])%*%Sigmaphi[-2,2]))*Q # Prior Prec of phi2|phi1,phi3,phi4
      priormean<-diag(n)%x%(Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2]))%*%c(t(phimat[,-2]))      # Prior mean of phi2|phi1,phi3,phi4
      prec<-priorprec+as.spam(diag(tapply(w*t^2,sid,sum),n,n))	
      m<-c(priorprec%*%priormean)+tapply(t*w*(z-Xtilde%*%alpha-Phi1),sid,sum)
      if(is.positive.definite(prec%>%as.matrix)) phi2<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi2<-phi2-mean(phi2)
      Phi2<-rep(phi2,nis)
      
      # Update r
      rnew<-rtnorm(1,r,sqrt(s),lower=0)       # Treat r as continuous
      ratio<-sum(dnbinom(y[y1==1],rnew,q[y1==1],log=T))-sum(dnbinom(y[y1==1],r,q[y1==1],log=T))+   
        dtnorm(r,rnew,sqrt(s),0,log=T) - dtnorm(rnew,r,sqrt(s),0,log=T)   # Uniform Prior for R 
      # Proposal not symmetric 
      if (log(runif(1))<ratio) {
        r<-rnew
        Acc<-Acc+1
      }
      
      # Update at-risk indicator y1 (W in paper)
      eta1<-Xtilde%*%alpha+Phi1+Phi2*t
      eta2<-Xtilde%*%beta+Phi3+Phi4*t              # Use all n observations
      pi<-pmax(0.01,pmin(0.99,inv.logit(eta1)))  # at-risk probability
      q<-pmax(0.01,pmin(0.99,1/(1+exp(eta2))))                      # Pr(y=0|y1=1)
      theta<-pi*(q^r)/(pi*(q^r)+1-pi)         # Conditional prob that y1=1 given y=0 -- i.e. Pr(chance zero|observed zero)
      y1[y==0]<-rbinom(N0,1,theta[y==0])      # If y=0, then draw a "chance zero" w.p. theta, otherwise y1=1
      N1<-sum(y1)
      nis1<-tapply(y1,sid,sum)
      
      # Update beta
      eta<-Xtilde[y1==1,]%*%beta+Phi3[y1==1]+Phi4[y1==1]*t[y1==1]
      w<-rpg(N1,y[y1==1]+r,eta)                               # Polya weights
      z<-(y[y1==1]-r)/(2*w)                                      
      v<-solve(crossprod(Xtilde[y1==1,]*sqrt(w))+T0b)               
      m<-v%*%(T0b%*%beta0+t(sqrt(w)*Xtilde[y1==1,])%*%(sqrt(w)*(z-Phi3[y1==1]-Phi4[y1==1]*t[y1==1])))  
      beta<-c(rmvnorm(1,m,v))
      
      # Update phi3
      n1<-length(nis1[nis1>0])
      
      priorprec<-as.numeric(1/(Sigmaphi[3,3]-Sigmaphi[3,-3]%*%solve(Sigmaphi[-3,-3])%*%Sigmaphi[-3,3]))*Q # Prior Prec of phi3|phi1,phi2,phi4
      priormean<-diag(n)%x%(Sigmaphi[3,-3]%*%solve(Sigmaphi[-3,-3]))%*%c(t(phimat[,-3]))      # Prior mean of phi3|phi1,phi2,phi4
      prec<-priorprec+as.spam(diag(tapply(w,sid[y1==1],sum),n,n))
      tmp<-rep(0,n)                       # Account empty blocks
      tmp[nis1>0]<-tapply(w*(z-Xtilde[y1==1,]%*%beta-Phi4[y1==1]*t[y1==1]),sid[y1==1],sum)
      m<-c(priorprec%*%priormean)+tmp
      if(is.positive.definite(prec%>%as.matrix)) phi3<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi3<-phi3-mean(phi3)
      Phi3<-rep(phi3,nis)
      
      # Update phi4
      priorprec<-as.numeric(1/(Sigmaphi[4,4]-Sigmaphi[4,-4]%*%solve(Sigmaphi[-4,-4])%*%Sigmaphi[-4,4]))*Q # Prior Prec of phi4|phi1,phi2,phi3
      priormean<-diag(n)%x%(Sigmaphi[4,-4]%*%solve(Sigmaphi[-4,-4]))%*%c(t(phimat[,-4]))      # Prior mean of phi4|phi1,phi2,phi3
      
      prec<-priorprec+as.spam(diag(tapply(w*t[y1==1]^2,sid[y1==1],sum),n,n))
      tmp<-rep(0,n)                       # Account for empty counties
      tmp[nis1>0]<-tapply(t[y1==1]*w*(z-Xtilde[y1==1,]%*%beta-Phi3[y1==1]),sid[y1==1],sum)
      m<-c(priorprec%*%priormean)+tmp
      if(is.positive.definite(prec%>%as.matrix)) phi4<-rmvnorm.canonical(1, m, prec)[1,]
      
      # Center
      phi4<-phi4-mean(phi4)
      Phi4<-rep(phi4,nis)
      
      # Update r2 using Gibbs as in Dadaneh et al and Zhou and Carin
      # Update latent counts, l        
      l<-rep(0,N1)
      ytmp<-y[y1==1]
      for(j in 1:N1) l[j]<-sum(rbinom(ytmp[j],1,round(r/(r+1:ytmp[j]-1),6))) # Could try to avoid loop; rounding avoids numerical stability
      
      # Update r from conjugate gamma distribution given l and psi
      psi<-exp(eta2[y1==1])/(1+exp(eta2[y1==1]))
      r2<-rgamma(1,0.01+sum(l),0.01-sum(log(1-psi)))
      
      
      # Update Sigma.phi
      phimat<-cbind(phi1,phi2,phi3,phi4)
      try({
        Sigmaphi<-riwish(4+n-1,diag(4)+t(phimat)%*%Q%*%phimat)  
      })
      
      # Store
      if (i> nburn & i%%nthin==0) {
        j<-(i-nburn)/nthin
        Alpha[j,,chain]<-alpha
        Beta[j,,chain]<-beta
        R[j,chain]<-r		
        R2[j,chain]<-r2	
        Sigphi[j,,chain]<-c(Sigmaphi)
        PHI1[j,,chain]<-phi1
        PHI2[j,,chain]<-phi2
        PHI3[j,,chain]<-phi3
        PHI4[j,,chain]<-phi4
        Eta1[j,,chain]<-eta1
        Eta2[j,,chain]<-eta2
        I[j,,chain]<-y1
      }
      
      # if (i%%10==0) print(paste(chain, "/", nchain,"chain | ",round(i/nsim*100,2),"% completed"))
      if (i%%10==0) print(paste(chain, "/", nchain,"chain | ",round(i/nsim*100,2),"% completed |","Test:",conv.test(R[,chain])))
      
    }  
  }
  
  list.params = list(Alpha=Alpha, Beta=Beta, R=R, R2=R2, Sigphi=Sigphi,
                     PHI1=PHI1, PHI2=PHI2, PHI3=PHI3, PHI4=PHI4,
                     Eta1=Eta1, Eta2=Eta2, I=I)
  return(list.params)
  
}

USDmapCount = function(state.sel,dat,scol,tcol=NULL,tsel=NULL,cname,uplim=NULL){
  
  if(is.null(uplim)){
    ulim = max(dat$y)
  }else{
    ulim = uplim
  } 
  library(usmap)
  all.st <- usmap::us_map(regions="counties")
  state.map <- all.st[all.st$abbr %in% state.sel,]
  sid = names(dat) %>% .[scol]
  tid = names(dat) %>% .[tcol]
  if(is.null(tsel)){
    zinb.summary     = dat %>% group_by(sid) %>% summarise(cnt=mean(y))  
  }else{
    zinb.summary     = dat %>% group_by(sid) %>% filter(tid==tsel)
    zinb.summary$cnt = zinb.summary$y
    zinb.summary = zinb.summary %>% dplyr::select(sid,cnt) 
  }
  
  zinb.summary$cid = paste(cname[zinb.summary$sid],"County")
  map.df  <- left_join(state.map, zinb.summary, by = c("county"="cid"))
  p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
    geom_polygon(aes(fill = cnt), color="#FF7E00", size=0.2) + 
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill"
      # limits=c(0,ulim)
    ) +
    theme_bw() +
    labs(fill= "",
         title = "", x="", y="")
  p
  
}

USVisualSTREMap = function(state.set,stfit,inputCounty,inputState){
  library(usmap)
  all.st <- usmap::us_map(regions="counties")
  state.map <- all.st[all.st$abbr %in% state.set,]
  
  temp = matrix(colMeans(stfit$Eta1),nrow=111,ncol=10)
  temp2 = sapply(colMeans(stfit$PHI1),function(w) rep(w,10)) %>% t
  temp3 = sapply(colMeans(stfit$PHI2),function(w) rep(w,10)) %>% t
  tt <- rep(0:9, times=111); tt <- tt / 10
  temp4 = matrix(tt,nrow=111,ncol=10,byrow=TRUE)
  inputdat <-  temp2 + temp3 * temp4
  
  if(is.null(ncol(inputdat))){
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set])
  }else{
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set,] %>% rowMeans)
  }
  map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
  
  p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
    geom_polygon(aes(fill = cnt), color="#FF7E00", size=0.2) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill"
    ) +
    theme_bw() +
    labs(fill= "",
         title = "", x="", y="")
  
  require(grid)
  grob1 <- grobTree(textGrob("Estimated spatio-temporal random effect for probability at risk(q) of in logit scale ", x=0.05,  y=0.975, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  grob2 <- grobTree(textGrob("Brighter color means higher disease at risk probability ", x=0.1,  y=0.025, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  
  return(p + annotation_custom(grob1)+ annotation_custom(grob2))
}

USVisualFEMap = function(state.set,stfit,inputCounty,inputState){
  library(usmap)
  all.st <- usmap::us_map(regions="counties")
  state.map <- all.st[all.st$abbr %in% state.set,]
  
  temp = matrix(colMeans(stfit$Eta1),nrow=111,ncol=10)
  temp2 = sapply(colMeans(stfit$PHI1),function(w) rep(w,10)) %>% t
  temp3 = sapply(colMeans(stfit$PHI2),function(w) rep(w,10)) %>% t
  tt <- rep(0:9, times=111); tt <- tt / 10
  temp4 = matrix(tt,nrow=111,ncol=10,byrow=TRUE)
  inputdat <- temp - (temp2 + temp3 * temp4)
  
  if(is.null(ncol(inputdat))){
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set])
  }else{
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set,] %>% rowMeans)
  }
  map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
  
  p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
    geom_polygon(aes(fill = cnt), color="#FF7E00", size=0.2) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill"
    ) +
    theme_bw() +
    labs(fill= "",
         title = "", x="", y="")
  
  require(grid)
  grob1 <- grobTree(textGrob("Estimated fixed effect for probability at risk(q) of in logit scale ", x=0.05,  y=0.975, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  grob2 <- grobTree(textGrob("Darker color means higher disease at risk probability ", x=0.1,  y=0.025, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  
  return(p + annotation_custom(grob1)+ annotation_custom(grob2))
}

USVisualQavgMap = function(state.set,stfit,inputCounty,inputState){
  
  require(usmap)
  all.st <- usmap::us_map(regions="counties")
  state.map <- all.st[all.st$abbr %in% state.set,]
  
  temp = matrix(colMeans(stfit$Eta1),nrow=111,ncol=10)
  temp2 = sapply(colMeans(stfit$PHI1),function(w) rep(w,10)) %>% t
  temp3 = sapply(colMeans(stfit$PHI2),function(w) rep(w,10)) %>% t
  tt <- rep(0:9, times=111); tt <- tt / 10
  temp4 = matrix(tt,nrow=111,ncol=10,byrow=TRUE)
  inputdat <- inv.logit(temp)
  
  if(is.null(ncol(inputdat))){
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set])
  }else{
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set,] %>% rowMeans)
  }
  map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
  
  p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
    geom_polygon(aes(fill = cnt), color="#FF7E00", size=0.2) + 
    theme_bw() +
    labs(fill= "",
         title = "", x="", y="")
  
  require(grid)
  grob1 <- grobTree(textGrob("Estimated probability at risk(q) in logit scale ", x=0.05,  y=0.975, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  grob2 <- grobTree(textGrob("Brighter color means higher disease at risk probability ", x=0.1,  y=0.025, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  
  return(p + annotation_custom(grob1)+ annotation_custom(grob2))
}

USVisualQMap = function(state.set,stfit,inputCounty,inputState,tind){
  
  require(usmap)
  all.st <- usmap::us_map(regions="counties")
  state.map <- all.st[all.st$abbr %in% state.set,]
  
  temp = matrix(colMeans(stfit$Eta1),nrow=111,ncol=10)
  temp2 = sapply(colMeans(stfit$PHI1),function(w) rep(w,10)) %>% t
  temp3 = sapply(colMeans(stfit$PHI2),function(w) rep(w,10)) %>% t
  tt <- rep(0:9, times=111); tt <- tt / 10
  temp4 = matrix(tt,nrow=111,ncol=10,byrow=TRUE)
  inputdat <- inv.logit(temp)
  
  if(is.null(ncol(inputdat))){
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set])
  }else{
    zinb.summary <- data.frame(County=paste(inputCounty,"County"), 
                               cnt = inputdat %>% .[inputState %in% state.set,] %>% .[,tind])
  }
  map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
  
  
  p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
    geom_polygon(aes(fill = cnt), color="#FF7E00", size=0.2) + 
    theme_bw() +
    labs(fill= "",
         title = "", x="", y="")
  
  require(grid)
  grob1 <- grobTree(textGrob("Estimated probability at risk(q) in logit scale ", x=0.05,  y=0.975, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  grob2 <- grobTree(textGrob("Brighter color means higher disease at risk probability ", x=0.1,  y=0.025, hjust=0,
                             gp=gpar(col="blue", fontsize=13, fontface="italic")))
  
  return(p + annotation_custom(grob1)+ annotation_custom(grob2))
  
  
}

qRankPar = function(state.set,ns,nt,cname,stfit=res4,vn=12){
  qij.mat <- matrix(inv.logit(apply(stfit$Eta1,2,mean)),nrow=ns)
  zinb.summary <- data.frame(County=cname, m = rowMeans(qij.mat))
  zinb.summary <- zinb.summary[order(zinb.summary$m),]
  zinb.summary$County = factor(zinb.summary$County)
  zinb.summary.sample = zinb.summary[floor(seq(1,ns,length.out=vn)),]
  
  par(mfrow=c(1,1),margin(3,5,1,1))
  p <- ggplot(zinb.summary.sample,aes(x=reorder(County, m), y=m, fill=County)) + geom_bar(alpha=0.8,stat="identity") +
    xlab("") + ylab("Probability at risk") + ylim(0,1) + theme_bw() + theme(legend.position = "")
  p + coord_flip()+theme(axis.title.x = element_text(size=24),
                         axis.text.x = element_text(size=24),
                         axis.text.y = element_text(size=24))
}

qRankParTop = function(state.set,ns,nt,cname,stfit=res4,vn=12){
  qij.mat <- matrix(inv.logit(apply(stfit$Eta1,2,mean)),nrow=ns)
  zinb.summary <- data.frame(County=cname, m = rowMeans(qij.mat))
  zinb.summary <- zinb.summary[order(zinb.summary$m,decreasing = TRUE),]
  zinb.summary$County = factor(zinb.summary$County)
  zinb.summary.sample = zinb.summary[c(1:vn),]
  
  par(mfrow=c(1,1),margin(3,5,1,1))
  p <- ggplot(zinb.summary.sample,aes(x=reorder(County, m), y=m, fill=County)) + geom_bar(alpha=0.8,stat="identity") +
    xlab("") + ylab("Probability at risk") + ylim(0,1) + theme_bw() + theme(legend.position = "")
  p + coord_flip()+theme(axis.title.x = element_text(size=24),
                         axis.text.x = element_text(size=24),
                         axis.text.y = element_text(size=24))
}

TimetrendCurve = function(stfit,ns,nt,countyname,vn=5,smooth.mode=TRUE){
  time = c(1:nt)
  df   = data.frame(matrix(apply(stfit$Eta1,2,mean),nrow=ns)); rownames(df) <- factor(countyname)
  df2 = data.frame(time,t(df[seq(1,ns,length.out=vn),]))
  if(smooth.mode){
    library(splines)
    time = spline(df2[,1],df2[,2])$x 
    df2 <- data.frame(time,apply(df2[,-1],2,function(w) {spline(df2[,1],w)$y}))
  }
  library(reshape2)
  dd = melt(df2,c("time"))
  
  spd.plot <- ggplot(dd, aes(x=time,y=value)) + 
    geom_line(aes(colour = variable, group = variable),cex=1.2)+
    geom_line(aes(time,0),color="red",cex=1.2,lty=2) + ylab("") + xlab("") +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.title.x = element_text(size=20))
  
  spd.plot
  
}

ResultTableSummary = function(bstfit){
  
  alphamat = apply(bstfit$Alpha,c(1,2),mean)
  betamat  = apply(bstfit$Beta,c(1,2),mean)
  temp <- data.frame(cbind(alphamat,betamat))
  # colnames(temp) <- c(paste0("a",(c(1:ncol(alphamat))-1)),paste0("b",(c(1:ncol(betamat))-1)))
  colnames(temp) <- c(paste("a",colnames(alphamat),sep="."),paste("b",colnames(betamat),sep="."))
  
  temp %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({p5}, {p95})", all_categorical() ~ "{n} ({p}%)"),
                       digits = list(all_continuous() ~ c(3,3))) %>%
    modify_header(label = "**Coefficients**",
                  stat_0 = '**Estimates**') %>%
  modify_caption("**BSTZINB Model Coefficients**") %>%
    bold_labels() %>%
    modify_footnote(
      all_stat_cols() ~ "Point estimates (90% credible intervals)")
}

ResultTableSummary2 = function(y, X, A, nt, oind = NULL, nchain=3, nsim=10, nburn=0, nthin=1){
  
  stfit4 = BSTZINB(y, X, A, nt, oind, LinearT=FALSE, nchain, nsim, nburn, nthin)
  alphamat = apply(stfit4$Alpha,c(1,2),mean)
  betamat  = apply(stfit4$Beta,c(1,2),mean)
  temp4 <- data.frame(matrix(NA,nrow(alphamat),ncol(alphamat)+ncol(betamat)+2))
  # colnames(temp4) <- c(paste0("a",(c(1:ncol(alphamat))-1)),paste0("b",(c(1:ncol(betamat))-1)))
  colnames(temp4) <- c("a.t",paste("a",colnames(alphamat),sep="."),"b.t",paste("b",colnames(betamat),sep="."))
  temp4[,paste("a",colnames(alphamat),sep=".")] <- alphamat
  temp4[,paste("b",colnames(betamat),sep=".")]  <- betamat
  DIC4 = compute.ZINB.DIC(y,stfit4,(nsim-nburn)/nthin,nchain)
  ind4 = rep(NA,ncol(temp4)); names(ind4) <-  colnames(temp4)
  ind4[paste("a",colnames(alphamat),sep=".")] <- conv.test(stfit4$Alpha)
  ind4[paste("b",colnames(betamat),sep=".")] <- conv.test(stfit4$Beta)
  
  stfit3 = BSTZINB(y, X, A, nt, oind, LinearT=TRUE, nchain, nsim, nburn, nthin)
  alphamat = apply(stfit3$Alpha,c(1,2),mean)
  betamat  = apply(stfit3$Beta,c(1,2),mean)
  temp3 <- data.frame(matrix(NA,nrow(temp4),ncol(temp4)))
  colnames(temp3) <- colnames(temp4)
  temp3[,paste("a",colnames(alphamat),sep=".")] <- alphamat
  temp3[,paste("b",colnames(betamat),sep=".")]  <- betamat
  DIC3 = compute.ZINB.DIC(y,stfit3,(nsim-nburn)/nthin,nchain)
  ind3 = rep(NA,length(ind4)); names(ind3) <- names(ind4)
  ind3[paste("a",colnames(alphamat),sep=".")] <- conv.test(stfit3$Alpha)
  ind3[paste("b",colnames(betamat),sep=".")] <- conv.test(stfit3$Beta)
  
  stfit2 = BSTNB(y, X, A, nt, oind, nchain, nsim, nburn, nthin)
  betamat  = apply(stfit2$Beta,c(1,2),mean)
  temp2 <- data.frame(matrix(NA,nrow(temp4),ncol(temp4)))
  colnames(temp2) <- colnames(temp4)
  temp2[,paste("b",colnames(betamat),sep=".")] <- betamat
  DIC2 = compute.NB.DIC(y,stfit2,(nsim-nburn)/nthin,nchain)
  ind2 = rep(NA,length(ind4)); names(ind2) <- names(ind4)
  ind2[paste("b",colnames(betamat),sep=".")] <- conv.test(stfit2$Beta)
  
  stfit1 = BSTP(y, X, A, nt, oind, nchain, nsim, nburn, nthin)
  betamat  = apply(stfit1$Beta,c(1,2),mean)
  temp1 <- data.frame(matrix(NA,nrow(temp4),ncol(temp4)))
  colnames(temp1) <- colnames(temp4)
  temp1[,paste("b",colnames(betamat),sep=".")] <- betamat
  DIC1 = compute.NB.DIC(y,stfit1,(nsim-nburn)/nthin,nchain)
  ind1 = rep(NA,length(ind4)); names(ind1) <- names(ind4)
  ind1[paste("b",colnames(betamat),sep=".")] <- conv.test(stfit1$Beta)
  
  tabout <- data.frame(rbind(temp4,temp3,temp2,temp1))
  tabout$var <- c(rep("4BSTZINB(NLT)",nrow(temp4)),
                  rep("3BSTZINB(LT)",nrow(temp3)),
                  rep("2BSTNB",nrow(temp2)),
                  rep("1BSTP",nrow(temp1)))
  
  table <- tabout%>% tbl_summary(by = var,missing = "no",
                                 digits = list(all_continuous() ~ c(3,3)))%>%
    modify_header(label = "**Coefficients**",
                  stat_1 = '**BSTP**',
                  stat_2 = '**BSTNB**',
                  stat_3 = '**BSTZINB(LT)**',
                  stat_4 = '**BSTZINB(NLT)**') %>%
    modify_footnote(
      all_stat_cols() ~ paste("Point estimates (90% credible intervals)",
                              "DIC1 = ",round(DIC1,1),";",
                              "DIC2 = ",round(DIC2,1),";",
                              "DIC3 = ",round(DIC3,1),";",
                              "DIC4 = ",round(DIC4,1)))
  
  table$table_body$stat_1[table$table_body$stat_1 == "NA (NA, NA)"] <- NA
  table$table_body$stat_2[table$table_body$stat_2 == "NA (NA, NA)"] <- NA
  table$table_body$stat_3[table$table_body$stat_3 == "NA (NA, NA)"] <- NA
  table$table_body$stat_4[table$table_body$stat_4 == "NA (NA, NA)"] <- NA
  
  
  
  table.fin <- table %>% as_gt %>%
    tab_style(
      style = list(cell_text(color = "darkred")),
      locations = cells_body(columns=stat_4,rows=(ind4==FALSE))
    ) %>%
    tab_style(
      style = list(cell_text(color = "darkred")),
      locations = cells_body(columns=stat_3,rows=(ind3==FALSE))
    )%>%
    tab_style(
      style = list(cell_text(color = "darkred")),
      locations = cells_body(columns=stat_2,rows=(ind2==FALSE))
    )%>%
    tab_style(
      style = list(cell_text(color = "darkred")),
      locations = cells_body(columns=stat_1,rows=(ind1==FALSE))
    )
  
  return(table.fin)
  
}
# ResultTableSummary2(y, X, A, nt)

compute.ZINB.DIC = function(y,fit,lastit,nchain){
  
  computeD.avg = function(fit){
    eta1.mean <- apply(fit$Eta1,2,mean)
    eta2.mean <- apply(fit$Eta2,2,mean)
    r.mean    <- mean(fit$R)
    pi = gtools::inv.logit(eta1.mean)
    q <- pmax(0.01,pmin(0.99,1/(1+exp(eta2.mean))))
    I = fit$I[dim(fit$I)[1],,nchain]
    dNB = dnbinom(y[I==1],r.mean,q[I==1],log=T)
    comp1 = log(1-pi[I==0])
    comp2 = (log(pi[I==1])+dNB)
    # (-2)*(sum(comp1)+sum(comp2))
    (-2)*sum(comp2)
  }
  computeD.indiv = function(fit,iter,chain){
    eta1 <- fit$Eta1[iter,,chain]
    eta2 <- fit$Eta2[iter,,chain]
    r    <- fit$R[iter,chain]
    pi = gtools::inv.logit(eta1)
    q <- pmax(0.01,pmin(0.99,1/(1+exp(eta2))))
    I = fit$I[iter,,chain]
    dNB = dnbinom(y[I==1],r,q[I==1],log=T)
    comp1 = log(1-pi[I==0])
    comp2 = (log(pi[I==1])+dNB)
    # (-2)*(sum(comp1)+sum(comp2))
    (-2)*sum(comp2)
  }
  Dmat = matrix(0,lastit,nchain)
  for(iter in 1:lastit){
    for(chain in 1:nchain){
      Dmat[iter,chain] = computeD.indiv(fit,iter,chain)
    }
  }
  
  comp1 = computeD.avg(fit)
  comp2 = mean(Dmat)
  DIC = comp1 + 2*( comp2 - comp1)
  return(DIC)
}

compute.NB.DIC = function(y,fit,lastit,nchain){
  
  computeD.avg = function(fit){
    eta.mean <- apply(fit$Eta,2,mean)
    r.mean   <- mean(fit$R)
    q <- pmax(0.01,pmin(0.99,1/(1+exp(eta.mean))))
    dNB = dnbinom(y,r.mean,q,log=T)
    (-2)*sum(dNB)
  }
  computeD.indiv = function(fit,iter,chain){
    eta  <- fit$Eta[iter,,chain]
    r    <- fit$R[iter,chain]
    q <- pmax(0.01,pmin(0.99,1/(1+exp(eta))))
    dNB = dnbinom(y,r,q,log=T)
    (-2)*sum(dNB)
  }
  Dmat = matrix(0,lastit,nchain)
  for(iter in 1:lastit){
    for(chain in 1:nchain){
      Dmat[iter,chain] = computeD.indiv(fit,iter,chain)
    }
  }
  
  comp1 = computeD.avg(fit)
  comp2 = mean(Dmat)
  DIC = comp1 + 2*( comp2 - comp1)
  return(DIC)
  
}

conv.test = function(params,nchain=3,thshold=1.96){
  
  if(length(dim(params))==3){
    p = dim(params)[2]
    out = rep(NA,p)
    for(colid in 1:p){
      mcmc.temp = mcmc(params[,colid,])
      testout = geweke.diag(mcmc.temp) %>% unlist
      out[colid] <- as.logical(prod(abs(testout[1:nchain])<thshold))  
    }
    return(out)  
  }else{
    mcmc.temp = mcmc(params)
    testout = geweke.diag(mcmc.temp) %>% unlist
    as.logical(prod(abs(testout[1:nchain])<thshold))  
  }
  
}
