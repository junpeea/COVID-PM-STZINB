
##################################################################################################################
##### Table1
##################################################################################################################
library(mpath)
names(mydat)
glimpse(mydat)
# mean(colMeans(mydat$positives.county==0)*100)%>% round(1);sd(colMeans(mydat$positives.county==0)*100)%>% round(1)
mean(colMeans(mydat$week_cum.deaths.county==0)*100)%>% round(1);sd(colMeans(mydat$week_cum.deaths.county==0)*100)%>% round(1)
mean(mydat$deaths.county) %>% round(1);sd(mydat$deaths.county) %>% round(1)
mean(mydat$week_cum.deaths.county) %>% round(1);sd(mydat$week_cum.deaths.county) %>% round(1)
mean(mydat$cum.deaths.county[,ncol(mydat$cum.deaths.county)]) %>% round(1);sd(mydat$cum.deaths.county[,ncol(mydat$cum.deaths.county)]) %>% round(1)
glimpse(mydat$aggregate.state)
mean(mydat$aggregate.state$population/1000) %>% round(0);sd(mydat$aggregate.state$population/1000) %>% round(0)
mean(mydat$aggregate.state$mean_pm25) %>% round(1);sd(mydat$aggregate.state$mean_pm25) %>% round(1)
mean(mydat$aggregate.state$poverty*100) %>% round(1);sd(mydat$aggregate.state$poverty*100) %>% round(1)
mean(mydat$aggregate.state$popdensity) %>% round(0);sd(mydat$aggregate.state$popdensity) %>% round(0)
mean(mydat$aggregate.state$medianhousevalue/1000) %>% round(1);sd(mydat$aggregate.state$medianhousevalue/1000) %>% round(1)
mean(mydat$aggregate.state$medhouseholdincome/1000) %>% round(1);sd(mydat$aggregate.state$medhouseholdincome/1000) %>% round(1)
mean(mydat$aggregate.state$pct_owner_occ*100) %>% round(1);sd(mydat$aggregate.state$pct_owner_occ*100) %>% round(1)
mean(mydat$aggregate.state$hispanic*100) %>% round(1);sd(mydat$aggregate.state$hispanic*100) %>% round(1)
mean(mydat$aggregate.state$education*100) %>% round(1);sd(mydat$aggregate.state$education*100) %>% round(1)
mean(mydat$aggregate.state$pct_blk*100) %>% round(1);sd(mydat$aggregate.state$pct_blk*100) %>% round(1)
mean(mydat$aggregate.state$older_pecent*100) %>% round(1);sd(mydat$aggregate.state$older_pecent*100) %>% round(1)

##################################################################################################################
##### Table2
##################################################################################################################
my.zerotable = 100 * apply(mydat$week_cum.deaths.county,2,function(w) length(which(w==0))/length(w))
my.DailyCountSum  = colSums(mydat$deaths.county)
my.WeeklyCountSum = colSums(mydat$week_cum.deaths.county)

pdf(paste0(Sys.Date(),"_",state,"_ZeroDailyCountPlot.pdf"),width=10)
figpoints = c(0:23)*1.2+0.2; daypoints = seq(as.Date("2020-03-23"),as.Date("2020-08-31"),by=7)
par(mfrow=c(1,1),mar=c(5,5,1,4))
barplot(my.zerotable, ylab="Percent",xlab="Date",col="lightblue",ylim=c(0,100), cex.lab = 3.0, cex.axis=2.0); abline(h=mean(my.zerotable),lty=2,col=2); abline(h=100,lty=2,col=1)
# axis(side=1,(seq(0,26.5,length.out=6)+0.2),label=seq(as.Date("2020-03-23"),as.Date("2020-08-24"),length.out=6),las=1,cex.axis=1.0)
axis(side=1,at=figpoints[c(seq(1,24,by=4),24)], label=daypoints[c(seq(1,24,by=4),24)],las=1,cex.axis=2.0)
dev.off()

pdf(paste0(Sys.Date(),"_",state,"_DeathWeeklyCountPlot.pdf"),width=10)
figpoints = c(0:23)*1.2+0.2; daypoints = seq(as.Date("2020-03-23"),as.Date("2020-08-31"),by=7)
par(mfrow=c(1,1),mar=c(5,5,1,4))
barplot(my.WeeklyCountSum, ylim=c(0,max(my.WeeklyCountSum)*1.2),ylab="Count",xlab="Date",col="orange", cex.lab = 3.0, cex.axis=2.0)
axis(side=1,at=figpoints[c(seq(1,24,by=4),24)], label=daypoints[c(seq(1,24,by=4),24)],las=1,cex.axis=2.0)
dev.off()

##################################################################################################################
##### Table3
##################################################################################################################
all.st <- usmap::us_map(regions="counties")
state.map <- all.st[all.st$full==state,]
zinb.summary <- data.frame(County=unique(state.map$county), 
                           daily.cnt  = mydat$deaths.county[,ncol(mydat$deaths.county)],
                           weekly.cnt = mydat$cum.deaths.county[,ncol(mydat$cum.deaths.county)],
                           pm25    = mydat$aggregate.state$mean_pm25
)

# Please ignore any warning generated in the following line
map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))


p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
  geom_polygon(aes(fill = daily.cnt), color="#FFFFFF", size=0.2) +
  scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8) +
  theme_bw() + 
  labs(fill= "Daily counts",
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
# file <- paste0(state,"_zinb_county", str, "_binary_rand_intercept.pdf")
file <- paste0(Sys.Date(),"_",state,"_daily_count.pdf")
ggsave(file)

p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
  geom_polygon(aes(fill = weekly.cnt), color="#FFFFFF", size=0.2) +
  scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8) +
  theme_bw() +
  labs(fill= "Weakly counts",
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
# file <- paste0(state,"_zinb_county", str, "_binary_rand_intercept.pdf")
file <- paste0(Sys.Date(),"_",state, "_weakly_count.pdf")
ggsave(file)

p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
  geom_polygon(aes(fill = pm25), color="#FFFFFF", size=0.2) +
  scale_fill_gradient2() +
  theme_bw() +
  labs(fill= "PM2.5(ug/m3)",
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
# file <- paste0(state,"_zinb_county", str, "_binary_rand_intercept.pdf")
file <- paste0(Sys.Date(),"_",state, "_pm25exposure.pdf")
ggsave(file)


##################################################################################################################
##### Table9
##################################################################################################################

########################################################################################################################
load(paste0("~/Desktop/Result_Jun/2020-09-09_",state.abbr,"_resultv1_zinb_daily_with_offset.Rdata"))
load(paste0("~/Desktop/Result_Jun/2020-09-24_",state.abbr,"_resultv3_zinb_daily_with_offset.Rdata"))
filename = paste0(Sys.Date(),"_",state,"_","nl_beta_trend_STNB.pdf")

# Time-spline regression component
library(splines)
n.dates <- dim(mydat$positives.county)[2]
t <- c(0:(n.dates-1)); tt <- t / n.dates; length(tt)
Bmatrix = bs(tt,knots=c(0.25,0.50,0.75),Boundary.knots=c(0,1),degree=3,intercept=TRUE); dim(Bmatrix)
BTsum = t(Bmatrix %*% t(zinb.daily.offset$Beta[,14:20])); dim(BTsum)
quant.BTsum = apply(BTsum,2,function(w) quantile(w,probs=c(0,0.025,0.5,0.975,1)))
date <- as.Date("2020-03-23")+t

library(ggplot2)
df = data.frame(t(quant.BTsum));names(df) <- c("min","lb","med","ub","max");head(df)
spd.plot <- ggplot(df,aes(x=date,color="95% CI")) + ylab("") +
  geom_line(aes(date,med),color="black",cex=1.2)+
  geom_line(aes(date,0),color="red",cex=1.0,lty=2)+
  geom_ribbon(aes(ymin=lb,ymax=ub),data=df,fill="blue",alpha=0.2,linetype=0,cex=1.2)+
  theme_bw() + theme(legend.position = "none",
                     legend.title = element_blank(),
                     axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=20),
                     axis.title.x = element_text(size=20))
spd.plot
ggsave(filename,width=7*2,height=7*5)
zinb.daily.offset$DIC

########################################################################################################################
load(paste0("~/Desktop/Result_Jun/2020-09-09_",state.abbr,"_resultv3_zinb_daily_with_offset.Rdata"))
filename = paste0(Sys.Date(),"_",state,"_","nl_beta_trend_STZINB(LT).pdf")

# Time-spline regression component
library(splines)
n.dates <- dim(mydat$positives.county)[2]
t <- c(0:(n.dates-1)); tt <- t / n.dates; length(tt)
Bmatrix = bs(tt,knots=c(0.25,0.50,0.75),Boundary.knots=c(0,1),degree=3,intercept=TRUE); dim(Bmatrix)
BTsum = t(Bmatrix %*% t(zinb.daily.offset$Beta[,14:20])); dim(BTsum)
quant.BTsum = apply(BTsum,2,function(w) quantile(w,probs=c(0,0.025,0.5,0.975,1)))
date <- as.Date("2020-03-23")+t

library(ggplot2)
df = data.frame(t(quant.BTsum));names(df) <- c("min","lb","med","ub","max");head(df)
spd.plot <- ggplot(df,aes(x=date,color="95% CI")) + ylab("") +
  geom_line(aes(date,med),color="black",cex=1.2)+
  geom_line(aes(date,0),color="red",cex=1.0,lty=2)+
  geom_ribbon(aes(ymin=lb,ymax=ub),data=df,fill="blue",alpha=0.2,linetype=0,cex=1.2)+
  theme_bw() + theme(legend.position = "none",
                     legend.title = element_blank(),
                     axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=20),
                     axis.title.x = element_text(size=20))
spd.plot
ggsave(filename)
zinb.daily.offset$DIC

########################################################################################################################
load(paste0("~/Desktop/Result_Jun/2020-09-09_",state.abbr,"_resultv4_zinb_daily_with_offset.Rdata"))
load(paste0("~/Desktop/Result_Jun/2020-09-24_",state.abbr,"_resultv3_zinb_daily_with_offset.Rdata"))
filename = paste0(Sys.Date(),"_",state,"_","nl_beta_trend_STZINB(NLT).pdf")

# Time-spline regression component
library(splines)
n.dates <- dim(mydat$positives.county)[2]
t <- c(0:(n.dates-1)); tt <- t / n.dates; length(tt)
Bmatrix = bs(tt,knots=c(0.25,0.50,0.75),Boundary.knots=c(0,1),degree=3,intercept=TRUE); dim(Bmatrix)
BTsum = t(Bmatrix %*% t(zinb.daily.offset$Beta[,14:20])); dim(BTsum)
quant.BTsum = apply(BTsum,2,function(w) quantile(w,probs=c(0,0.025,0.5,0.975,1)))
date <- as.Date("2020-03-23")+t

library(ggplot2)
df = data.frame(t(quant.BTsum));names(df) <- c("min","lb","med","ub","max");head(df)
spd.plot <- ggplot(df,aes(x=date,color="95% CI")) + ylab("") +
  geom_line(aes(date,med),color="black",cex=1.2)+
  geom_line(aes(date,0),color="red",cex=1.0,lty=2)+
  geom_ribbon(aes(ymin=lb,ymax=ub),data=df,fill="blue",alpha=0.2,linetype=0,cex=1.2)+
  theme_bw() + theme(legend.position = "none",
                     legend.title = element_blank(),
                     axis.text.x = element_text(size=20),
                     axis.text.y = element_text(size=20),
                     axis.title.x = element_text(size=20))
spd.plot
ggsave(filename,width=7*2,height=7*5)
zinb.daily.offset$DIC


##################################################################################################################
##### Table10
##################################################################################################################
load(paste0("~/Desktop/Result_Jun/2020-09-09_",state.abbr,"_resultv3_zinb_daily_with_offset.Rdata"))

n.dates = dim(mydat$week_cum.deaths.county)[2]
n <-nrow(mydat$adj.mat)
tt <- rep(0:(n.dates-1), times=n); tt <- tt / n.dates
x0 <- rep(scale(mydat$aggregate.state$Population), each=n.dates) # Population size as offset
x1 <- rep(mydat$aggregate.state$mean_pm25, each=n.dates)
x2 <- rep(scale(mydat$aggregate.state$poverty), each=n.dates)
x3 <- rep(scale(mydat$aggregate.state$popdensity), each=n.dates)
x4 <- rep(scale(mydat$aggregate.state$medianhousevalue), each=n.dates)
x5 <- rep(scale(mydat$aggregate.state$medhouseholdincome), each=n.dates)
x6 <- rep(scale(mydat$aggregate.state$pct_owner_occ), each=n.dates)
x7 <- rep(scale(mydat$aggregate.state$hispanic), each=n.dates)
x8 <- rep(scale(mydat$aggregate.state$education), each=n.dates)
x9 <- rep(scale(mydat$aggregate.state$pct_blk), each=n.dates)
x10 <- rep(scale(mydat$aggregate.state$older_pecent), each=n.dates)

library(splines)
Bmatrix = bs(tt,knots=c(0.25,0.50,0.75),Boundary.knots=c(0,1),degree=3,intercept=TRUE)
X <- cbind(1, tt, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x0, Bmatrix); p<-ncol(X)

eta1.list        = lapply(c(1:23),function(w) t(X[seq(w,nrow(X),by=n.dates),] %*% t(zinb.daily.offset$Alpha)) + zinb.daily.offset$PHI1 + zinb.daily.offset$PHI2 * matrix(rep(tt[1:n],5000),nrow=5000,byrow=TRUE))
# median.qij.list  = lapply(c(1:23), function(w) apply(1-1/(1+exp(eta1.list[[w]])),2,mean))
mean.qij.list = lapply(c(1:23),function(w) colMeans(1-1/(1+exp(eta1.list[[w]]))))
mean.qij.mat = matrix(unlist(mean.qij.list),nrow=n)
quant.t.qij.mat = apply(mean.qij.mat,1,function(w) quantile(w,prob=c(0,0.025,0.5,0.975,1)))

# par(mfrow=c(1,2))
# boxplot(mean.qij.mat,ylim=c(0.25,1))
# boxplot(t(mean.qij.mat),ylim=c(0.25,1),horizontal=T)

df <- data.frame(t(quant.t.qij.mat)); names(df) <- c("min","lb","m","ub","max"); head(df) 
all.st <- usmap::us_map(regions="counties")
state.map <- all.st[all.st$full==state,]
zinb.summary <- data.frame(County=unique(state.map$county), m = df$m)
zinb.summary <-zinb.summary[order(zinb.summary$m),]
newname = lapply(c(1:n),function(w){
  temp = nchar(as.character(zinb.summary$County[w]))
  substring(zinb.summary$County[w] %>% as.character, first=1,last=temp-7)  
}) %>% unlist
zinb.summary$County = factor(newname,levels=newname)
zinb.summary.sample = zinb.summary[floor(seq(1,n,length.out=12)),]

filename = paste0(Sys.Date(),"_",state,"_","PAR_comparison.pdf")
par(mfrow=c(1,1),margin(3,5,1,1))
p <- ggplot(zinb.summary.sample,aes(x=County, y=m, fill=County)) + geom_bar(alpha=0.8,stat="identity") +
   xlab("") + ylab("Probability at risk") + theme_bw() + theme(legend.position = "")
p + coord_flip()+theme(axis.title.x = element_text(size=56),
                       axis.text.x = element_text(size=56),
                       axis.text.y = element_text(size=56))
ggsave(filename,width=7*2,height=7*5)



filename = paste0(Sys.Date(),"_",state,"_","PAR_by_county_med.pdf")
par(mfrow=c(1,1),margin(3,5,1,1))
eta1_01j.mat = t(X[seq(1,nrow(X),by=n.dates),] %*% t(zinb.daily.offset$Alpha)) + zinb.daily.offset$PHI1 + zinb.daily.offset$PHI2 * matrix(rep(tt[1:n],5000),nrow=5000,byrow=TRUE); dim(eta1_01j.mat)
qij.mat = 1-1/(1+exp(eta1_01j.mat))
quant.qij = apply(qij.mat,2,function(w) quantile(w,prob=c(0,0.025,0.5,0.975,1)))

all.st <- usmap::us_map(regions="counties")
state.map <- all.st[all.st$full==state,]
# zinb.summary <- data.frame(County=unique(state.map$county),
#                            lc = quant.qij[2,],
#                            mc = (quant.qij[3,])^2,
#                            uc = quant.qij[4,])
zinb.summary <- data.frame(County=unique(state.map$county),
                           lc = (quant.t.qij.mat[2,])^2,
                           mc = (quant.t.qij.mat[3,])^2,
                           uc = (quant.t.qij.mat[4,])^2)

map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))

p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
  # geom_polygon(aes(fill = lc), color="#FFFFFF", size=0.2) +
  geom_polygon(aes(fill = mc), color="#FFFFFF", size=0.2) +
  # geom_polygon(aes(fill = uc), color="#FFFFFF", size=0.2) +
  # scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,0.5))+
  scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,1),
                     breaks=(rev(1-c(0,0.02,0.05,0.10,0.25,0.50)))^2,
                     labels=c(0.5,0.75,0.90,"","",1) ) +
  theme_bw() +
  labs(fill= "Probability at risk",
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


##################################################################################################################
##### Trace plot
##################################################################################################################

library(bayesplot)
temp.dat = zinb.daily.offset$Beta
dim(temp.dat)
colnames(temp.dat) = paste0("c",c(1:ncol(temp.dat)))
mcmc.temp.beta = list()
mcmc.temp.beta[[1]] = temp.dat[2001:3000,] %>% mcmc
mcmc.temp.beta[[2]] = temp.dat[5001:6000,] %>% mcmc
mcmc.temp.beta[[3]] = temp.dat[8001:9000,] %>% mcmc
mcmc.temp.beta = mcmc.temp.beta %>% mcmc.list
mcmc_trace(mcmc.temp.beta)
mcmc_trace(temp.dat)

temp.dat = zinb.daily.offset$Beta
dim(temp.dat)
summary(temp.dat)
colnames(temp.dat) = paste0("c",c(1:ncol(temp.dat)))
mcmc.temp.beta = list()
mcmc.temp.beta[[1]] = temp.dat[20001:30000,] %>% mcmc
mcmc.temp.beta[[2]] = temp.dat[40001:50000,] %>% mcmc
mcmc.temp.beta[[3]] = temp.dat[70001:80000,] %>% mcmc
mcmc.temp.beta = mcmc.temp.beta %>% mcmc.list
mcmc_trace(mcmc.temp.beta)
mcmc_trace(temp.dat[,c(3,4,5,6)])
mcmc_trace(temp.dat[,c(7,8,9,10)])

library(dplyr)



mcmc.temp1.beta = list()
mcmc.temp1.beta[[1]] = temp.dat[20001:30000,c(1,3,4,5)] %>% mcmc
mcmc.temp1.beta[[2]] = temp.dat[50001:60000,c(1,3,4,5)] %>% mcmc
mcmc.temp1.beta[[3]] = temp.dat[80001:90000,c(1,3,4,5)] %>% mcmc
mcmc.temp1.beta = mcmc.temp1.beta %>% mcmc.list
pdf(paste0(Sys.Date(),state,"_BetaTraceplot1.pdf"));mcmc_trace(mcmc.temp1.beta);dev.off()

mcmc.temp2.beta = list()
mcmc.temp2.beta[[1]] = temp.dat[20001:30000,c(6,7,8,9,10,11)] %>% mcmc
mcmc.temp2.beta[[2]] = temp.dat[50001:60000,c(6,7,8,9,10,11)] %>% mcmc
mcmc.temp2.beta[[3]] = temp.dat[80001:90000,c(6,7,8,9,10,11)] %>% mcmc
mcmc.temp2.beta = mcmc.temp2.beta %>% mcmc.list
pdf(paste0(Sys.Date(),state,"_BetaTraceplot2.pdf"));mcmc_trace(mcmc.temp2.beta);dev.off()

mcmc.temp3.beta = list()
mcmc.temp3.beta[[1]] = temp.dat[20001:30000,c(12,14,15,16)] %>% mcmc
mcmc.temp3.beta[[2]] = temp.dat[50001:60000,c(12,14,15,16)] %>% mcmc
mcmc.temp3.beta[[3]] = temp.dat[80001:90000,c(12,14,15,16)] %>% mcmc
mcmc.temp3.beta = mcmc.temp3.beta %>% mcmc.list
pdf(paste0(Sys.Date(),state,"_BetaTraceplot3.pdf"));mcmc_trace(mcmc.temp3.beta);dev.off()

mcmc.temp4.beta = list()
mcmc.temp4.beta[[1]] = temp.dat[20001:30000,c(17,18,19,20)] %>% mcmc
mcmc.temp4.beta[[2]] = temp.dat[50001:60000,c(17,18,19,20)] %>% mcmc
mcmc.temp4.beta[[3]] = temp.dat[80001:90000,c(17,18,19,20)] %>% mcmc
mcmc.temp4.beta = mcmc.temp4.beta %>% mcmc.list
pdf(paste0(Sys.Date(),state,"_BetaTraceplot4.pdf"));mcmc_trace(mcmc.temp4.beta);dev.off()

chain.seed <- c(20000,40000,70000)
params <- zinb.daily.offset
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

res.Sigphi = params$Sigphi[sel.ind,]
msigphi<-colMeans(res.Sigphi)
msigphi.mat = matrix(msigphi,4,4)
msigphi.ar <- round(msigphi.mat[lower.tri(msigphi.mat, diag=TRUE)], 4)
msigphi.indx <- array(lower.tri(msigphi.mat, diag=TRUE))
bounds.sigphi <- round(hdi(res.Sigphi)[,msigphi.indx], 4)
bounds.sigphi.txt <- unlist(lapply(1:dim(bounds.sigphi)[2],
                                   function(x) paste("(",bounds.sigphi[1,x], ", ", bounds.sigphi[2,x], ")", sep="")))


write.csv(rbind(cbind(alpha, bounds.alpha.txt),
                cbind(beta, bounds.beta.txt),
                cbind(R, bounds.R.txt),
                cbind(msigphi.ar, bounds.sigphi.txt)),
          file = results.file)




geweke.diag(temp.dat[sel.ind,c(-2,-13)]) %>% unlist %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -10.921  -4.321   1.638   1.115   4.967  12.028 
geweke.diag(temp.dat[9500:50000,c(-2,-13)]) %>% unlist %>% summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -5.4569 -2.7469 -0.5667 -0.2830  1.5176  6.9671 

geweke.plot(temp.dat[sel.ind,c(1,5,6,7,10,12,15,16,20)] %>% mcmc)

# ########################################################################################################################
# filename = paste0(Sys.Date(),"_",state,"_","PAR_by_time.pdf")
# pdf(filename)
# par(mfrow=c(1,1),mar=c(1,3,1,1))
# figpoints = c(1:23); daypoints = seq(as.Date("2020-03-23"),as.Date("2020-08-31"),by=7)
# boxplot(mean.qij.mat,ylim=c(0.25,1),xaxt='n',cex.axis=2.0)
# # axis(side=1,at=figpoints[c(seq(1,24,by=4),24)], label=daypoints[c(seq(1,24,by=4),24)],las=1,cex.axis=1.2)
# dev.off()
# ########################################################################################################################
# # filename = paste0(Sys.Date(),"_",state,"_","structurezero_week01.pdf")
# # eta1_01j.mat = t(X[seq(1,nrow(X),by=n.dates),] %*% t(zinb.daily.offset$Alpha)) + zinb.daily.offset$PHI1 + zinb.daily.offset$PHI2 * matrix(rep(tt[1:n],5000),nrow=5000,byrow=TRUE); dim(eta1_01j.mat)
# # qij.mat = 1-1/(1+exp(eta1_01j.mat))
# # quant.qij = apply(qij.mat,2,function(w) quantile(w,prob=c(0,0.025,0.5,0.975,1)))
# filename = paste0(Sys.Date(),"_",state,"_","PAR_by_county_med.pdf")
# 
# all.st <- usmap::us_map(regions="counties")
# state.map <- all.st[all.st$full==state,]
# # zinb.summary <- data.frame(County=unique(state.map$county), 
# #                            lc = quant.qij[2,],
# #                            mc = (quant.qij[3,])^2,
# #                            uc = quant.qij[4,])
# zinb.summary <- data.frame(County=unique(state.map$county), 
#                            lc = (quant.t.qij.mat[2,])^2,
#                            mc = (quant.t.qij.mat[3,])^2,
#                            uc = (quant.t.qij.mat[4,])^2)
# 
# map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
# 
# p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
#   # geom_polygon(aes(fill = lc), color="#FFFFFF", size=0.2) +
#   geom_polygon(aes(fill = mc), color="#FFFFFF", size=0.2) +
#   # geom_polygon(aes(fill = uc), color="#FFFFFF", size=0.2) +
#   # scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,0.5))+
#   scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,1),
#                      breaks=(rev(1-c(0,0.02,0.05,0.10,0.25,0.50)))^2,
#                      labels=c(0.5,0.75,0.90,"","",1) ) +
#   theme_bw() +
#   labs(fill= "Probability at risk",
#        title = "", x="", y="")
# p+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggsave(filename)
# 
# ########################################################################################################################
# # filename = paste0(Sys.Date(),"_",state,"_","structurezero_week10.pdf")
# # eta1_10j.mat = t(X[seq(10,nrow(X),by=n.dates),] %*% t(zinb.daily.offset$Alpha)) + zinb.daily.offset$PHI1 + zinb.daily.offset$PHI2 * matrix(rep(tt[1:n],5000),nrow=5000,byrow=TRUE); dim(eta1_10j.mat)
# # qij.mat = 1-1/(1+exp(eta1_10j.mat))
# # quant.qij = apply(qij.mat,2,function(w) quantile(w,prob=c(0,0.025,0.5,0.975,1)))
# filename = paste0(Sys.Date(),"_",state,"_","PAR_by_county_lb.pdf")
# 
# all.st <- usmap::us_map(regions="counties")
# state.map <- all.st[all.st$full==state,]
# # zinb.summary <- data.frame(County=unique(state.map$county), 
# #                            lc = quant.qij[2,],
# #                            mc = (quant.qij[3,])^2,
# #                            uc = quant.qij[4,])
# zinb.summary <- data.frame(County=unique(state.map$county), 
#                            lc = (quant.t.qij.mat[2,])^2,
#                            mc = (quant.t.qij.mat[3,])^2,
#                            uc = (quant.t.qij.mat[4,])^2)
# 
# map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
# 
# p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
#   geom_polygon(aes(fill = lc), color="#FFFFFF", size=0.2) +
#   # geom_polygon(aes(fill = mc), color="#FFFFFF", size=0.2) +
#   # geom_polygon(aes(fill = uc), color="#FFFFFF", size=0.2) +
#   # scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,0.5))+
#   scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,1),
#                      breaks=(rev(1-c(0,0.02,0.05,0.10,0.25,0.50)))^2,
#                      labels=c(0.5,0.75,0.90,"","",1) ) +
#   
#   theme_bw() +
#   labs(fill= "Probability at risk",
#        title = "", x="", y="")
# p+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggsave(filename)
# 
# ########################################################################################################################
# # filename = paste0(Sys.Date(),"_",state,"_","structurezero_week20.pdf")
# # eta1_20j.mat = t(X[seq(20,nrow(X),by=n.dates),] %*% t(zinb.daily.offset$Alpha)) + zinb.daily.offset$PHI1 + zinb.daily.offset$PHI2 * matrix(rep(tt[1:n],5000),nrow=5000,byrow=TRUE); dim(eta1_20j.mat)
# # qij.mat = 1-1/(1+exp(eta1_20j.mat))
# # quant.qij = apply(qij.mat,2,function(w) quantile(w,prob=c(0,0.025,0.5,0.975,1)))
# filename = paste0(Sys.Date(),"_",state,"_","PAR_by_county_ub.pdf")
# 
# all.st <- usmap::us_map(regions="counties")
# state.map <- all.st[all.st$full==state,]
# # zinb.summary <- data.frame(County=unique(state.map$county), 
# #                            lc = quant.qij[2,],
# #                            mc = (quant.qij[3,])^2,
# #                            uc = quant.qij[4,])
# zinb.summary <- data.frame(County=unique(state.map$county), 
#                            lc = (quant.t.qij.mat[2,])^2,
#                            mc = (quant.t.qij.mat[3,])^2,
#                            uc = (quant.t.qij.mat[4,])^2)
# 
# map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
# 
# p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
#   # geom_polygon(aes(fill = lc), color="#FFFFFF", size=0.2) +
#   # geom_polygon(aes(fill = mc), color="#FFFFFF", size=0.2) +
#   geom_polygon(aes(fill = uc), color="#FFFFFF", size=0.2) +
#   # scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,0.5))+
#   scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,1),
#                      breaks=(rev(1-c(0,0.02,0.05,0.10,0.25,0.50)))^2,
#                      labels=c(0.5,0.75,0.90,"","",1) ) +
#   theme_bw() +
#   labs(fill= "Probability at risk",
#        title = "", x="", y="")
# p+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggsave(filename)
# 
# 
# # all.st <- usmap::us_map(regions="counties")
# # state.map <- all.st[all.st$full==state,]
# # zinb.summary <- data.frame(County=unique(state.map$county), 
# #                            lc = quant.qij[2,],
# #                            mc = sqrt(1-quant.qij[3,]),
# #                            uc = quant.qij[4,])
# # map.df  <- left_join(state.map, zinb.summary, by = c("county"="County"))
# # 
# # p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
# #   geom_polygon(aes(fill = mc), color="#FFFFFF", size=0.2) +
# #   # scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,0.5))+
# #   scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,limits=c(0,1),
# #                      breaks=sqrt(c(0.02,0.05,0.10,0.25,0.50,1)),
# #                      labels=c("","",0.1,0.25,0.5,1) ) +