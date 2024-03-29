### THIS CODE IS FOR COUNTY WISE FOR A SINGLE STATE # 

library(BayesLogit)   # For rpg function -- install from ZIP file
library(mvtnorm)	
library(MCMCpack)     # For Iwish update of Sigmab
library(msm)          # For tnorm function
library(spam)         # For sparse Matrix

library(usmap)        # For generating US maps
library(viridis)      # For generating colorscheme
library(ggplot2)      # For creating map graphics
library(HDInterval)   # For computing credibility interval
library(bayesplot)    # For generating trace plot
library(dplyr)

nb_cross_county_corona <- function (state, adj.mat, aggregate.state, positives.county,
                                    cumulative, start.date, cross.date, offset=FALSE, 
                                    nsim=nsim, burn=burn, chain.seed=c(20000,50000,80000),
                                    results.file=results.file) {
  
  cross.day <- as.Date(cross.date, format="%Y-%m-%d") - as.Date(start.date, format="%Y-%m-%d") + 1
  
  ##################
  # Adjacency Info #
  ##################
  
  # A <- state.adj.mat
  A <- adj.mat
  # A <- A[-c(30,31), -c(30,31)]
  m <- apply(A, 1, sum)
  
  ##################
  # Generate Data  #
  ##################
  
  set.seed(091017)
  n.dates <- dim(positives.county)[2]
  n<-nrow(A)			    # Number of spatial units
  # nis<-rep(dim(positives.county)[2],n) 		# Number of individuals per county; here it's balanced -- 50 per county per year
  nis <- 1
  #nis<-rep(6,n)
  # Note: may need to lower proposal variance, s, below as n_i increases 
  id<-rep(1:n,nis)  
  N<-length(id) 		  # Total number of observations (50 subjects per county)
  
  ########################
  #  Spatial Effects     #
  ########################
  
  kappa<-.999999			  	            # Spatial dependence parameter ~ 1 for intrinsic CAR
  Q<-as.spam(diag(m))-kappa*as.spam(A)			     
  
  #################
  # Fixed Effects #
  #################
  # tt <- c(0:(n.dates-1)); tt <- tt / n.dates
  x0 <- scale(aggregate.state$population) # Population size as offset
  x1 <- aggregate.state$mean_pm25
  x2 <- scale(aggregate.state$poverty)
  # x3 <- scale(aggregate.state$popdensity)
  x3 <- factor(aggregate.state$q_popdensity)
  # Dummy variable
  X3 = fastDummies::dummy_cols(x3); x32 <- X3[,(1+2)] %>% as.numeric; x33 <- X3[,(1+3)] %>% as.numeric ; x34 <- X3[,(1+4)] %>% as.numeric  
  x4 <- scale(log(aggregate.state$medianhousevalue))
  x5 <- scale(log(aggregate.state$medhouseholdincome))
  x6 <- scale(aggregate.state$pct_owner_occ)
  x7 <- scale(aggregate.state$hispanic)
  x8 <- scale(aggregate.state$education)
  x9 <- scale(aggregate.state$pct_blk)
  x10 <- scale(aggregate.state$older_pecent)
  x11 <- scale(aggregate.state$totalTestResults/aggregate.state$population)
  x12 <- scale(aggregate.state$beds/aggregate.state$population); x12[is.na(x12)] <- 0
  x13 <- scale(aggregate.state$mean_bmi); x13[is.na(x13)] <- 0
  x14 <- scale(aggregate.state$smoke_rate); x14[is.na(x14)] <- 0
  
  X <- cbind(1, x0, x1, x2, x32, x33, x34, x4, x5, x6, x7, x8, x9, x10, x12, x14); p<-ncol(X)
  y <- array(t(positives.county[, cross.day]))
  
  idx <- sample(1:N)
  id <- id[idx]
  y <- y[idx]
  X <- X[idx,]
  
  ##########   
  # Priors #
  ##########
  alpha0<-beta0<-rep(0,p)
  T0a<-diag(.01,p)
  T0b<-diag(.01,p)         # Uniform or Gamma(0.01,0.01) prior for r depending on MH or Gibbs
  s<-1               # Proposal variance  -- NOTE: may need to lower this as n_i increases
  
  #########   
  # Inits #
  #########
  beta<-alpha<-rep(0,p)
  #phi_init<-rmvnorm(1,sigma=diag(.1,4*n))	  # Random effects
  phi_init<-rmvnorm(1,sigma=diag(rep(c(5),times=n)))	  # Random effects
  phi_init<-matrix(phi_init,ncol=1,byrow=T)  # n x 3 matrix of spatial effects
  phi3<-phi_init[,1]
  phi3<-phi3-mean(phi3)
  Phi3 <- phi3
  # Phi1<-rep(phi1,nis)
  # Phi3<-rep(phi3,nis)
  
  phimat<-cbind(phi3)
  Sigmaphi<-cov(phimat)
  r<-1
  Acc<-0  
  y1<-rep(0,N)             # At risk indicator (this is W in paper)
  y1[y>0]<-1               # If y>0, then at risk w.p. 1
  N0<-length(y[y==0])      # Number of observed 0's
  q<-rep(.5,N)             # 1-p=1/(1+exp(X%*%alpha)), used for updating y1
  
  ############
  # Num Sims #
  ############
  nsim<- nsim			          # Number of MCMC Iterations (Used 50500 in paper)
  thin<-1			              # Thinning interval
  burn<-burn		              # Burnin
  lastit<-(nsim-burn)/thin	# Last stored value
  
  #########
  # Store #
  #########
  Beta<-Alpha<-matrix(0,lastit,p)
  R<-R2<-rep(0,lastit)
  Sigphi<-matrix(0,lastit,1)
  PHI3<-matrix(0,lastit,n)
  
  ########
  # MCMC #
  ########
  tmptime<-proc.time()
  
  for (i in 1:nsim){
    
    # # Update alpha  
    # mu<-X%*%alpha+Phi1
    # if (offset) {
    #   mu <- mu + pop
    # }
    # w<-rpg(N,1,mu)
    # z<-(y1-1/2)/w           
    # v<-solve(crossprod(sqrt(w)*X)+T0a)  
    # m<-v%*%(T0a%*%alpha0+t(sqrt(w)*X)%*%(sqrt(w)*(z-Phi1)))
    # alpha<-c(rmvnorm(1,m,v))
    # 
    # # Update phi1
    # priorprec<-1/(Sigmaphi[1,1]-Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1])%*%Sigmaphi[-1,1])*Q # Prior Prec of phi1|phi2,phi3,phi4
    # priormean<-diag(n)%x%(Sigmaphi[1,-1]%*%solve(Sigmaphi[-1,-1]))%*%c(t(phimat[,-1]))      # Prior mean of phi1|phi2,phi3,phi4
    # prec<-priorprec+as.spam(diag(tapply(w,id,sum),n,n))	
    # m<-c(priorprec%*%priormean)+tapply(w*(z-X%*%alpha),id,sum)
    # phi1<-rmvnorm.canonical(1, m, prec)[1,]
    
    # # Center
    # phi1<-phi1-mean(phi1)
    # Phi1<-rep(phi1,nis)
    
    # Update phi2
    # priorprec<-1/(Sigmaphi[2,2]-Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2])%*%Sigmaphi[-2,2])*Q # Prior Prec of phi2|phi1,phi3,phi4
    # priormean<-diag(n)%x%(Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2]))%*%c(t(phimat[,-2]))      # Prior mean of phi2|phi1,phi3,phi4
    # prec<-priorprec+as.spam(diag(tapply(w*tt^2,id,sum),n,n))	
    # m<-c(priorprec%*%priormean)+tapply(tt*w*(z-X%*%alpha-Phi1),id,sum)
    # phi2<-rmvnorm.canonical(1, m, prec)[1,]
    
    # Center
    # phi2<-phi2-mean(phi2)
    # Phi2<-rep(phi2,nis)
    
    # Update at-risk indicator y1 (W in paper)
    # eta1<-X%*%alpha+Phi1
    eta2<-X%*%beta+Phi3             # Use all n observations
    
    # pi<-pmax(0.001,pmin(0.999,exp(eta1)/(1+exp(eta1))))  # at-risk probability
    q<-pmax(0.001,pmin(0.999,1/(1+exp(eta2))))                      # Pr(y=0|y1=1)
    # q <- 1/(1+exp(eta2))
    # theta<-pi*(q^r)/(pi*(q^r)+1-pi)         # Conditional prob that y1=1 given y=0 -- i.e. Pr(chance zero|observed zero)
    # y1[y==0]<-rbinom(N0,1,theta[y==0])      # If y=0, then draw a "chance zero" w.p. theta, otherwise y1=1
    # N1<-sum(y1)
    # nis1<-tapply(y1,id,sum)
    
    # Update r
    rnew<-rtnorm(1,r,sqrt(s),lower=0)       # Treat r as continuous
    ratio<-sum(dnbinom(y,rnew,q,log=T), na.rm = TRUE)-sum(dnbinom(y,r,q,log=T), na.rm=TRUE)+    
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
    psi<-exp(eta2)/(1+exp(eta2))
    r2<-rgamma(1,0.01+sum(l),0.01-sum(log(1-psi)))
    
    # Update beta
    eta<-X%*%beta+Phi3
    w<-rpg(N,y+r,eta)                               # Polya weights
    z<-(y-r)/(2*w)                                      
    v<-solve(crossprod(X*sqrt(w))+T0b)               
    m<-v%*%(T0b%*%beta0+t(sqrt(w)*X)%*%(sqrt(w)*(z-Phi3)))  
    beta<-c(rmvnorm(1,m,v))
    if (offset) {
      # X <- cbind(1, x0, x1, x2, x32, x33, x34, x4, x5, x6, x7, x8, x9, x10, x12, x14); p<-ncol(X)
      beta[2] <- 1 #offset
    }
    
    # Update phi3
    # n1<-length(nis1[nis1>0])
    
    priorprec<-1/(Sigmaphi[1,1])*Q # Prior Prec of phi3|phi1,phi2,phi4
    # priormean<-diag(n)%x%(Sigmaphi[2,-2]%*%solve(Sigmaphi[-2,-2]))%*%c(t(phimat[,-2]))      # Prior mean of phi3|phi1,phi2,phi4
    priormean <- rep(0,n)
    prec<-priorprec+as.spam(diag(tapply(w,id,sum),n,n))
    tmp<-rep(0,n)                       # Account empty blocks
    tmp[nis>0]<-tapply(w*(z-array(X%*%beta)),id,sum)
    m<-c(priorprec%*%priormean)+tmp
    phi3<-rmvnorm.canonical(1, m, prec)[1,]
    
    # Center
    phi3<-phi3-mean(phi3)
    Phi3<-rep(phi3,nis)
    
    # Update phi4
    # priorprec<-1/(Sigmaphi[4,4]-Sigmaphi[4,-4]%*%solve(Sigmaphi[-4,-4])%*%Sigmaphi[-4,4])*Q # Prior Prec of phi4|phi1,phi2,phi3
    # priormean<-diag(n)%x%(Sigmaphi[4,-4]%*%solve(Sigmaphi[-4,-4]))%*%c(t(phimat[,-4]))      # Prior mean of phi4|phi1,phi2,phi3
    # 
    # prec<-priorprec+as.spam(diag(tapply(w*tt[y1==1]^2,id[y1==1],sum),n,n))
    # tmp<-rep(0,n)                       # Account for empty counties
    # tmp[nis1>0]<-tapply(tt[y1==1]*w*(z-array(X[y1==1,]%*%beta)-Phi3[y1==1]),id[y1==1],sum)
    # m<-c(priorprec%*%priormean)+tmp
    # phi4<-rmvnorm.canonical(1, m, prec)[1,]
    
    # Center
    # phi4<-phi4-mean(phi4)
    # Phi4<-rep(phi4,nis)
    
    # Update Sigma.phi
    phimat<-cbind(phi3)
    # Sigmaphi<-riwish(40000+n-1,diag(1,4)+t(phimat)%*%Q%*%phimat)
    Sigmaphi<-riwish(2+n-1,diag(1,1))
    
    # Store
    if (i> burn & i%%thin==0) {
      j<-(i-burn)/thin
      Alpha[j,]<-alpha
      Beta[j,]<-beta
      R[j]<-r		
      R2[j]<-r2	
      Sigphi[j,]<-c(Sigmaphi)
      PHI3[j,]<-phi3
    }
    
    if (i%%10==0) cat("MCMC param estimation (",i,"/",nsim,") completed",'\n')
    
  }
  
  tot.time<-proc.time()-tmptime  # Takes approx 6.6 hours to run without R2
  
  ###########
  # Results #
  ###########
  
  # params <- list(Alpha=Alpha, Beta=Beta, R=R, R2=R2, Sigphi = Sigphi, PHI1 = PHI1, PHI2=PHI2, PHI3=PHI3, PHI4=PHI4, W=W)
  params <- list(Alpha=Alpha, Beta=Beta, R=R, R2=R2, Sigphi = Sigphi, PHI3=PHI3)
  
  computeD = function(y,X,alpha,beta,r,PHI3){
    mylog = function(w){
      sapply(1:length(w), function(s){
        ifelse(w[s]!=0,log(w[s]),0)
      }
      )
    }
    # phi1 <- rep(PHI1,dim(X)[1]/length(PHI1))
    phi3 <- rep(PHI3,dim(X)[1]/length(PHI3))
    # eta1<-X%*%alpha+phi1
    eta2<-X%*%beta+phi3
    # pi = gtools::inv.logit(eta1)
    dNB = dnbinom(y,mu=eta2,size=r,log=TRUE)
    
    # outD = (-2)*(as.numeric(w==0)*mylog(1-pi)+as.numeric(w==1)*(mylog(pi)+dNB))
    outD = (-2)*dNB
    outD[is.na(outD)] <- min(outD,na.rm=T)
    outD
    
  }
  
  Davgparam = computeD(positives.county,X,
                       colMeans(params$Alpha),colMeans(params$Beta),mean(params$R),
                       colMeans(params$PHI3)) %>% sum(na.rm=T)
  
  Dmat = matrix(NA,lastit,1)
  for(iter in 1:lastit){
    Dmat[iter,] = computeD(positives.county,X,
                           params$Alpha[iter,],params$Beta[iter,],params$R[iter],
                           params$PHI3[iter,]) %>% as.numeric %>% var(na.rm=T)
    if(iter %% 100 == 0) cat("DIC calculation (",iter,"/",lastit,") completed",'\n')
  }
  pV = 0.5*mean(Dmat,na.rm=T)
  DIC = Davgparam + 2*pV
  
  sel.ind = c((chain.seed[1]+1):(chain.seed[1]+burn),
              (chain.seed[2]+1):(chain.seed[2]+burn),
              (chain.seed[3]+1):(chain.seed[3]+burn))
  
  res.Alpha = params$Alpha[sel.ind,]
  malpha <- colMeans(res.Alpha)
  alpha=round(malpha, 3)
  bounds.alpha <- round(hdi(res.Alpha), 2)
  bounds.alpha.txt <- unlist(lapply(1:dim(bounds.alpha)[2],
                                    function(x) paste("(",bounds.alpha[1,x], ", ", bounds.alpha[2,x], ")", sep="")))
  
  res.Beta = params$Beta[sel.ind,]
  mbeta <- colMeans(res.Beta)
  beta=round(mbeta, 3)
  bounds.beta <- round(hdi(res.Beta), 2)
  bounds.beta.txt <- unlist(lapply(1:dim(bounds.beta)[2],
                                   function(x) paste("(",bounds.beta[1,x], ", ", bounds.beta[2,x], ")", sep="")))
  
  res.R = params$R[sel.ind]
  mr<-mean(res.R)
  R = round(mr,3)
  bounds.R.txt = paste("(",round(hdi(res.R)[1], 2), ", ", round(hdi(res.R)[2], 2), ")", sep="")
  
  # res.Sigphi = params$Sigphi[sel.ind,]
  # msigphi<-colMeans(res.Sigphi)
  # msigphi.mat = matrix(msigphi,4,4)
  # msigphi.ar <- round(msigphi.mat[lower.tri(msigphi.mat, diag=TRUE)], 4)
  # msigphi.indx <- array(lower.tri(msigphi.mat, diag=TRUE))
  # bounds.sigphi <- round(hdi(res.Sigphi)[,msigphi.indx], 4)
  # bounds.sigphi.txt <- unlist(lapply(1:dim(bounds.sigphi)[2],
  #                                    function(x) paste("(",bounds.sigphi[1,x], ", ", bounds.sigphi[2,x], ")", sep="")))
  
  
  # write.csv(rbind(cbind(alpha, bounds.alpha.txt),
  #                 cbind(beta, bounds.beta.txt),
  #                 cbind(R, bounds.R.txt),
  #                 cbind(msigphi.ar, bounds.sigphi.txt)),
  #           file = results.file)
  
  write.csv(rbind(cbind(alpha, bounds.alpha.txt),
                  cbind(beta, bounds.beta.txt),
                  cbind(R, bounds.R.txt)),
            file = results.file)
  
  params <- list(Alpha=Alpha, Beta=Beta, R=R, R2=R2, Sigphi = Sigphi, PHI3=PHI3, DIC=DIC)
  
  return (params)
  
}
