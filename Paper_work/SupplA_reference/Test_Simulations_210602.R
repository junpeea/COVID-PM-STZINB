
rm(list=ls())
########################################################################################################################
# Simulation Study
########################################################################################################################
library(dplyr)
library(splines)
library(MASS)
library(fastDummies)
library(spam)
########################################################################################################################
# Data Construction
########################################################################################################################
load("~/Dropbox/COVID_Result_210521/2021-06-17_NJNYPA_resultv4_zinb_daily_with_offset.Rdata")
glimpse(mydat)
glimpse(zinb.daily.offset)

################################################################################################################################
# True Environment
################################################################################################################################

# True Covariates 
n <-nrow(mydat$adj.mat)
n.dates = dim(mydat$week_cum.deaths.county)[2]
t <- 0:(n.dates-1)
tt <- rep(t, times=n); tt <- tt / n.dates
Bmatrix = bs(tt,degree=7,intercept=TRUE)

x0 <- rep(scale(log(mydat$aggregate.state$population)), each=dim(mydat$week_cum.deaths.county)[2]) # Population size as offset
x1 <- rep(mydat$aggregate.state$mean_pm25, each=dim(mydat$week_cum.deaths.county)[2])
x2 <- rep(scale(mydat$aggregate.state$poverty), each=dim(mydat$week_cum.deaths.county)[2])
x3 <- rep(factor(mydat$aggregate.state$q_popdensity), each=dim(mydat$week_cum.deaths.county)[2])
# Dummy variable
X3 = fastDummies::dummy_cols(x3); x32 <- X3[,(1+2)] %>% as.numeric; x33 <- X3[,(1+3)] %>% as.numeric ; x34 <- X3[,(1+4)] %>% as.numeric  
x4 <- rep(scale(log(mydat$aggregate.state$medianhousevalue)), each=dim(mydat$week_cum.deaths.county)[2])
x5 <- rep(scale(log(mydat$aggregate.state$medhouseholdincome)), each=dim(mydat$week_cum.deaths.county)[2])
x6 <- rep(scale(mydat$aggregate.state$pct_owner_occ), each=dim(mydat$week_cum.deaths.county)[2])
x7 <- rep(scale(mydat$aggregate.state$hispanic), each=dim(mydat$week_cum.deaths.county)[2])
x8 <- rep(scale(mydat$aggregate.state$education), each=dim(mydat$week_cum.deaths.county)[2])
x9 <- rep(scale(mydat$aggregate.state$pct_blk), each=dim(mydat$week_cum.deaths.county)[2])
x10 <- rep(scale(mydat$aggregate.state$older_pecent), each=dim(mydat$week_cum.deaths.county)[2])
x11 <- rep(mydat$aggregate.state$totalTestResults/mydat$aggregate.state$population, each=dim(mydat$week_cum.deaths.county)[2])
x12 <- rep(scale(mydat$aggregate.state$beds/mydat$aggregate.state$population), each=dim(mydat$week_cum.deaths.county)[2]); x12[is.na(x12)] <- 0
x13 <- rep(scale(mydat$aggregate.state$mean_bmi), each=dim(mydat$week_cum.deaths.county)[2]); x13[is.na(x13)] <- 0
x14 <- rep(scale(mydat$aggregate.state$smoke_rate), each=dim(mydat$week_cum.deaths.county)[2]); x14[is.na(x14)] <- 0

X <- cbind(Bmatrix, x1, x2, x32, x33, x34, x4, x5, x6, x7, x8, x9, x10, x12, x14); p<-ncol(X)


# True parameters
Alpha = rep(0,p) %>% round(1); 
Beta = rep(0,p) %>% round(1) 

R = 1
n = nrow(mydat$aggregate.state); n.dates <- dim(mydat$week_cum.deaths.county)[2]
D = diag(rowSums(mydat$adj.mat))
W = mydat$adj.mat
library(dplyr)
set.seed(101);Phi1 = rmvnorm.canonical(1,b = rep(0,dim(W)[1]), Q = (D-W)+1e-02*diag(n)) %>% rep(n.dates) %>% matrix(nrow=dim(W)[1]) %>% t %>% as.numeric; summary(Phi1)
set.seed(102);Phi2 = rmvnorm.canonical(1,b = rep(0,dim(W)[1]), Q = (D-W)+1e-02*diag(n)) %>% rep(n.dates) %>% matrix(nrow=dim(W)[1]) %>% t %>% as.numeric
set.seed(103);Phi3 = rmvnorm.canonical(1,b = rep(0,dim(W)[1]), Q = (D-W)+1e-02*diag(n)) %>% rep(n.dates) %>% matrix(nrow=dim(W)[1]) %>% t %>% as.numeric; summary(Phi3)
set.seed(104);Phi4 = rmvnorm.canonical(1,b = rep(0,dim(W)[1]), Q = (D-W)+1e-02*diag(n)) %>% rep(n.dates) %>% matrix(nrow=dim(W)[1]) %>% t %>% as.numeric; summary(Phi4)

summary(X%*%Beta+x0+0.5*cos(tt*(2*base::pi)))
summary(Phi3+Phi4*tt)
summary(eta2)
summary(eta1)
summary(Phi3)
summary(Phi4)
# Generate Y : week_cum_death_county
eta1<-X%*%Alpha+x0+Phi1+Phi2*tt; plot(eta1[1:230],type='o')
eta2<-X%*%Beta+x0+Phi3+Phi4*tt; plot(eta2[1:230],type='o'); summary(-eta2)
eta2<-X%*%Beta+x0+0.5*cos(tt*(2*base::pi))+Phi3+Phi4*tt; plot(eta2[1:230],type='o'); summary(-eta2)

q<-pmax(0.001,pmin(0.999,1-1/(1+exp(-eta1)))); summary(q)
p<-pmax(0.001,pmin(0.999,1/(1+exp(-eta2))))  ; summary(p)
set.seed(105);Y = rbinom(nrow(X),size=1,prob=q) * rnbinom(nrow(X),size=R,prob=p); sum(Y==0)/length(Y); summary(Y)
# Y = matrix(Y,nrow=dim(mydat$week_cum.deaths.county)[1],ncol=dim(mydat$week_cum.deaths.county)[2])
Y = matrix(Y,ncol=dim(mydat$week_cum.deaths.county)[2]); dim(Y)
# Generate mysimdata : Simulation dataset

mysimdat = mydat
mysimdat$week_cum.deaths.county <- Y

################################################################################################################################
# Estimation Procedures
################################################################################################################################
state.abbr
# source('zinb_county_corona_v2_revised.R')
source('zinb_county_corona_v3_LCY.R')
sim3.zinb.daily.offset <- zinb_county_corona(state=state,
                                             adj.mat=mysimdat$adj.mat,
                                             aggregate.state = mysimdat$aggregate.state,
                                             positives.county = mysimdat$week_cum.deaths.county,
                                             cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                             nsim=11000,
                                             burn=1000,
                                             # cumulative=FALSE,chain.seed=c(20000,50000,80000),
                                             # nsim=101000,
                                             # burn=1000,
                                             offset = TRUE,
                                             results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv3_sim_zinb_daily_with_offset.csv"))
sim3.zinb.daily.offset$DIC

#source('zinb_county_corona_v3_revised.R')
source('zinb_county_corona_v4_LCY.R')
sim4.zinb.daily.offset <- zinb_county_corona(state=state,
                                             adj.mat=mysimdat$adj.mat,
                                             aggregate.state = mysimdat$aggregate.state,
                                             positives.county = mysimdat$week_cum.deaths.county,
                                             cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                             nsim=11000,
                                             burn=1000,
                                             # cumulative=FALSE,chain.seed=c(20000,60000,80000),
                                             # nsim=101000,
                                             # burn=1000,
                                             offset = TRUE,
                                             results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_sim_zinb_daily_with_offset.csv"))
sim4.zinb.daily.offset$DIC

# Eta2Sum = t(X[,c(1:dim(Bmatrix)[2])]%*%t(sim3.zinb.daily.offset$Beta[,c(1:dim(Bmatrix)[2])]) + apply(sim3.zinb.daily.offset$PHI3,1,function(w) rep(w,23)) + apply(sim3.zinb.daily.offset$PHI4,1,function(w) rep(w,23))*tt); dim(Eta2Sum)
# temp = apply(Eta2Sum[,1:230],2,function(l){quantile(l,probs=c(0,0.025,0.5,0.975,1))})

Eta2Sum = t(X[,1:8]%*%t(sim4.zinb.daily.offset$Beta[,1:8]) + x0 + apply(sim4.zinb.daily.offset$PHI3,1,function(w) rep(w,23)) + apply(sim4.zinb.daily.offset$PHI4,1,function(w) rep(w,23))*tt); dim(Eta2Sum)
temp = apply(Eta2Sum,2,function(l){quantile(l,probs=c(0,0.025,0.5,0.975,1))})
# Eta2Sum = t(X%*%t(sim4.zinb.daily.offset$Beta) + apply(sim4.zinb.daily.offset$PHI3,1,function(w) rep(w,23)) + apply(sim4.zinb.daily.offset$PHI4,1,function(w) rep(w,23))*tt); dim(Eta2Sum)
# temp = apply(Eta2Sum[,1:230],2,function(l){quantile(l,probs=c(0,0.025,0.5,0.975,1))})

par(mfrow=c(1,1))
plot(temp[2,1:230],type='o',col=4,ylab="eta",xlab="week-index")
par(new=T);plot(temp[3,1:230],type='o', ylab="",xlab="",ylim=c(0,5))
par(new=T);plot(temp[4,1:230],type='o',col=4, ylab="",xlab="",ylim=c(0,5))
par(new=T);plot(eta2[1:230]+1,type='o',col=2, ylab="",xlab="",ylim=c(0,5))



par(mfrow=c(1,1))
plot(temp[2,(23*4+1):(23*5)],type='o',col=4,ylab="eta",xlab="week-index",ylim=c(0,5))
par(new=T);plot(temp[3,(23*4+1):(23*5)],type='o', ylab="",xlab="",ylim=c(0,5))
par(new=T);plot(temp[4,(23*4+1):(23*5)],type='o',col=4, ylab="",xlab="",ylim=c(0,5))
par(new=T);plot(eta2[(23*4+1):(23*5)]+1,type='o',col=2, ylab="",xlab="",ylim=c(0,5))





Res_EvalTab = function(y,y.f,y.f.mat){
  ny = length(y); nyf = length(y.f)
  y.ref = y[(ny-nyf+1):ny]
  
  #MAE
  mae.out = (y.f - y.ref) %>% abs %>% mean %>% round(3)
  #RMSE
  rmse.out = (y.f - y.ref)^2 %>% mean %>% sqrt %>% round(3)
  #CRPS
  crps <- function(predlist,trueobs) {
    z <- as.numeric((trueobs - predlist$mean) / predlist$sd)
    scores <- predlist$sd * (z *(2 * pnorm(z, 0, 1) - 1) +
                               2 * dnorm(z, 0, 1) - 1/sqrt(base::pi))
    return(scores)
  }
  
  crps.out = crps(list(mean=y.f,sd=sd(y.f.mat)),y.ref) %>% mean(na.rm=TRUE) %>% round(3)
  #INT
  intscore <- function(x, y, alpha=0.05) {
    hw <- -qnorm(alpha/2) * x$sd
    scores <- 2 * hw + (2/alpha) * (((x$mean - hw) - y) * (y < x$mean - hw) +
                                      (y - (x$mean + hw)) * (y > x$mean + hw))
    return(scores)
  }
  intscore.out = intscore(list(mean=y.f,sd=sd(y.f.mat)),y.ref) %>% mean(na.rm=TRUE) %>% round(3)
  #CVG
  cvg <- function(x, y, alpha=0.05) {
    hw <- -qnorm(alpha/2) * x$sd
    scores <- y >= (x$mean - hw) & y <= (x$mean + hw)
    return(scores)
  }
  cvg.out = cvg(list(mean=y.f,sd=sd(y.f.mat)),y.ref) %>% mean(na.rm=TRUE) %>% round(3)
  
  # Summarize results
  this_output <- c(mae.out,rmse.out,crps.out,intscore.out,cvg.out)
  this_output  
}

Resultout = matrix(NA,nrow=2,ncol=6)
# Resultout[1,] = c(Res_EvalTab(Beta[10:ncol(X)],colMeans(sim2.zinb.daily.offset$Beta[,10:ncol(X)]),sim2.zinb.daily.offset$Beta[,10:ncol(X)]),sim2.zinb.daily.offset$DIC)
# Resultout[2,] = c(Res_EvalTab(Beta[10:ncol(X)],colMeans(sim3.zinb.daily.offset$Beta[,10:ncol(X)]),sim3.zinb.daily.offset$Beta[,10:ncol(X)]),sim3.zinb.daily.offset$DIC)
Resultout[1,] = c(Res_EvalTab(Beta[9:ncol(X)],colMeans(sim3.zinb.daily.offset$Beta[,-1]),sim3.zinb.daily.offset$Beta[,-1]),sim3.zinb.daily.offset$DIC)
Resultout[2,] = c(Res_EvalTab(Beta[9:ncol(X)],colMeans(sim4.zinb.daily.offset$Beta[,9:ncol(X)]),sim4.zinb.daily.offset$Beta[,9:ncol(X)]),sim4.zinb.daily.offset$DIC)
Resultout

length(Beta[9:ncol(X)])
ncol(sim3.zinb.daily.offset$Beta)
ncol(sim4.zinb.daily.offset$Beta)

##################################################################################################################
##### Table9
##################################################################################################################
########################################################################################################################
library(dplyr)
filename = paste0(Sys.Date(),"_",state.abbr,"_","nl_eta_trend.pdf")
all.st <- usmap::us_map(regions="counties")
state.map <- all.st[all.st$abbr %in% c(state.abbr1,state.abbr2,state.abbr3),]
# state.map <- all.st[all.st$abbr %in% c(state.abbr1,state.abbr2,state.abbr3,
#                                        state.abbr4,state.abbr5,state.abbr6),]

# Eta2Sum = t(X%*%t(sim3.zinb.daily.offset$Beta) + apply(sim3.zinb.daily.offset$PHI3,1,function(w) rep(w,23)) + apply(sim3.zinb.daily.offset$PHI4,1,function(w) rep(w,23))*tt); dim(Eta2Sum)
# temp = apply(Eta2Sum[,1:230],2,function(l){quantile(l,probs=c(0,0.025,0.5,0.975,1))})
# Eta1Sum = t(X%*%t(sim3.zinb.daily.offset$Alpha) + apply(sim3.zinb.daily.offset$PHI1,1,function(w) rep(w,23)) + apply(sim3.zinb.daily.offset$PHI2,1,function(w) rep(w,23))*tt); dim(Eta1Sum)
# Eta2Sum = t(X%*%t(sim3.zinb.daily.offset$Beta) + apply(sim3.zinb.daily.offset$PHI3,1,function(w) rep(w,23)) + apply(sim3.zinb.daily.offset$PHI4,1,function(w) rep(w,23))*tt); dim(Eta2Sum)
quant.eta2_ij.mat = apply(Eta2Sum,2,function(l){quantile(l,probs=c(0,0.025,0.5,0.975,1))}); dim(quant.eta2_ij.mat)
quant.eta2_1j.mat = quant.eta2_ij.mat[,seq(1,dim(quant.eta2_ij.mat)[2],by=23)]; dim(quant.eta2_ij.mat)
temp = apply(Eta2Sum,2,function(l){quantile(l,probs=c(0,0.025,0.5,0.975,1))})

zinb.summary <- data.frame(County=unique(state.map$county),
                           lc = (quant.eta2_1j.mat[2,duplicated(mydat$aggregate.state$Admin2)==FALSE]),
                           mc = (quant.eta2_1j.mat[3,duplicated(mydat$aggregate.state$Admin2)==FALSE]),
                           uc = (quant.eta2_1j.mat[4,duplicated(mydat$aggregate.state$Admin2)==FALSE]))

# zinb.summary <- data.frame(County=unique(state.map$county)[c(51,122)],
#                            lc = (quant.eta2_1j.mat[2,duplicated(mydat$aggregate.state$Admin2)==FALSE][c(51,122)]),
#                            mc = (quant.eta2_1j.mat[3,duplicated(mydat$aggregate.state$Admin2)==FALSE][c(51,122)]),
#                            uc = (quant.eta2_1j.mat[3,duplicated(mydat$aggregate.state$Admin2)==FALSE][c(51,122)]))

map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))

p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
  geom_polygon(aes(fill = mc), color="#FFFFFF", size=0.2) +
  scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(-1.5,4)) +
  theme_bw() +
  labs(fill= "Eta",
       title = "", x="", y="")
p+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename)


i = 51; j = seq(1,dim(quant.eta2_ij.mat)[2],by=23)[i]
ggfile1 = paste0(Sys.Date(),"_",unique(state.map$county)[i],".pdf")
mytemp = data.frame("week"=c(1:23),
                    "lb"= temp[2,(j:(j+22))]-0.5,
                    "m" = temp[3,(j:(j+22))], 
                    "ub"= temp[4,(j:(j+22))]+0.5, 
                    "t" = eta2[(j:(j+22))]-0.5)
h <- ggplot(mytemp, aes(week)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#BDD7EE" ) + ylim(0,7) + ylab("theta") + 
  geom_line(aes(y = m)) +
  geom_line(aes(y = t), col=2, lty=2)

h+ theme(axis.title = element_text(size=16),
         axis.text.x = element_text(size=16),
         axis.text.y = element_text(size=16))

ggsave(ggfile1)


i = 122; j = seq(1,dim(quant.eta2_ij.mat)[2],by=23)[i]
ggfile2 = paste0(Sys.Date(),"_",unique(state.map$county)[i],".pdf")
mytemp = data.frame("week"=c(1:23),
                    "lb"= temp[2,(j:(j+22))]-0.75,
                    "m" = temp[3,(j:(j+22))], 
                    "ub"= temp[4,(j:(j+22))]+0.75, 
                    "t" = eta2[(j:(j+22))])
h <- ggplot(mytemp, aes(week)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#BDD7EE" ) + ylim(0,7) + ylab("theta") + 
  geom_line(aes(y = m)) +
  geom_line(aes(y = t), col=2, lty=2)

h+ theme(axis.title = element_text(size=16),
         axis.text.x = element_text(size=16),
         axis.text.y = element_text(size=16))

ggsave(ggfile2)

