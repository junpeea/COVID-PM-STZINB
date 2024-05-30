################################################################################
# Vignette code
# Produced by YB Jun (24.01.01)
source("Library_v6.R")
################################################################################

################################################################################
# DATA STEP
source("Test230817.R")
################################################################################
this_dat <- data.frame(sid,tid,y,X)
head(this_dat)
tbl_summary(this_dat) %>% modify_header(label = "**Coefficients**") 

################################################################################
# DATA SUMMARY
################################################################################
# US Disease Mapping Cumulative Death Counts
USDmapCount(state.sel = c("IA"),
            dat = this_dat,
            scol  = 1,
            tcol  = 2,
            cname = countyname,
            uplim=150)


# blue histogram
tmp<-table(y)/N*100  # convert to %s (divide by N multiply by 100)
par(mar=c(3,3,1,1))
barplot(tmp, ylab="Percent",xlab="Count",col="lightblue")

# sphagetti plot
library(CorrMixed)
# Plot individual profiles + mean
Spaghetti.Plot(Dataset = this_dat, Outcome = y, Id=sid, Time = tid)

# Moran's I
library(ape)
this_dat2 = cast(this_dat,sid~tid,sum,value="y")
Moran.I(rowMeans(this_dat2),A)
this_dat2 %>% apply(2,function(w) Moran.I(w,A)) %>% lapply(function(list) list$p.value) %>% unlist
Moran.I(rowMeans(this_dat2),A)

# US Disease Mapping Spatio-temporal trend
USDmapCount(state.sel = c("IA"),
            dat = this_dat,
            scol  = 1,
            tcol  = 2, tsel = 3,
            cname = countyname)

USDmapCount(state.sel = c("IA"),
            dat = this_dat,
            scol  = 1,
            tcol  = 2, tsel = 6,
            cname = countyname)

USDmapCount(state.sel = c("IA"),
            dat = this_dat,
            scol  = 1,
            tcol  = 2, tsel = 9,
            cname = countyname)

USDmapCount(state.sel = c("IA"),
            dat = this_dat,
            scol  = 1,
            tcol  = 2, tsel = 12,
            cname = countyname)

################################################################################
# MODEL PROCEDURE
################################################################################

# Bayesian Spatio-Temporal Poisson Model
res1 = BSTP(y, X, A, nt=24, oind = NULL, nchain=3, nsim=1000, nburn=500)
glimpse(res1)
# List of 6
# $ Beta  : num [1:500, 1:2, 1:3] 0.0112 -0.0197 -0.0604 -0.0249 -0.0387 ...
# $ Sigphi: num [1:500, 1:4, 1:3] 0.8 0.943 1.049 0.939 0.959 ...
# $ R     : num [1:500, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
# $ PHI3  : num [1:500, 1:99, 1:3] 0.3907 -0.4748 -0.4867 0.0103 -0.1689 ...
# $ PHI4  : num [1:500, 1:99, 1:3] 0.03067 -0.08835 -0.12439 -0.12946 0.00041 ...
# $ Eta   : num [1:500, 1:2376, 1:3] -0.215629 0.441038 -0.436264 -0.508471 -0.000396 ...

# Bayesian Spatia-Temporal Negative Binomial Model
res2 = BSTNB(y, X, A, nt=24, oind = NULL, nchain=3, nsim=1000, nburn=500)
glimpse(res2)
# List of 7
# $ Beta  : num [1:500, 1:2, 1:3] 0.269 0.249 0.254 0.361 0.316 ...
# $ R     : num [1:500, 1:3] 0.775 0.75 0.75 0.736 0.736 ...
# $ R2    : num [1:500, 1:3] 1.12 1.17 1.16 1.18 1.13 ...
# $ Sigphi: num [1:500, 1:4, 1:3] 0.746 1.166 1.069 0.91 0.722 ...
# $ PHI3  : num [1:500, 1:99, 1:3] -0.137 -0.574 -0.092 -0.461 -0.21 ...
# $ PHI4  : num [1:500, 1:99, 1:3] -0.0925 -0.0828 0.1404 -0.147 -0.1045 ...
# $ Eta   : num [1:500, 1:2376, 1:3] 0.5624 0.1513 -0.2609 0.2293 -0.0146 ...

# Bayesian Spatio-Temporal Zero Inflated Negative Binomial Model (Linear Time Trend)
# res3 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=TRUE, nchain=3, nsim=2000, nburn=1000, nthin=1)
res3 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=TRUE, nchain=3, nsim=200, nburn=100, nthin=1)
glimpse(res3)
# $ Alpha : num [1:1000, 1:3, 1:3] -0.192 -0.111 -0.101 0.131 0.129 ...
# $ Beta  : num [1:1000, 1:3, 1:3] 0.504 0.379 0.346 0.359 0.381 ...
# $ R     : num [1:1000, 1:3] 1.35 1.36 1.35 1.35 1.34 ...
# $ R2    : num [1:1000, 1:3] 1.39 1.32 1.31 1.32 1.32 ...
# $ Sigphi: num [1:1000, 1:16, 1:3] 0.378 0.353 0.482 0.481 0.68 ...
# $ PHI1  : num [1:1000, 1:99, 1:3] -0.149 -0.45 -0.367 -0.324 -0.166 ...
# $ PHI2  : num [1:1000, 1:99, 1:3] -1.216 -1.345 -1.222 -0.799 -1.458 ...
# $ PHI3  : num [1:1000, 1:99, 1:3] 0.2834 -0.0182 0.1306 0.1367 0.2953 ...
# $ PHI4  : num [1:1000, 1:99, 1:3] -0.0879 0.1143 -0.0104 -0.0636 0.2004 ...
# $ Eta1  : num [1:1000, 1:2376, 1:3] -0.616 -0.834 -0.761 -0.581 -0.433 ...
# $ Eta2  : num [1:1000, 1:2376, 1:3] 0.985 0.934 0.47 0.624 0.701 ...
# $ I     : num [1:1000, 1:2376, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
conv.test(res3$Alpha)
conv.test(res3$Beta)
conv.test(res3$R)
par(mfrow=c(1,3));plot(res3$R[,1]);plot(res3$R[,2]);plot(res3$R[,3])
par(mfrow=c(1,3));plot(res3$Alpha[,1,1],type='o');plot(res3$Alpha[,1,2],type='o');plot(res3$Alpha[,1,3],type='o')

apply(res3$Beta,2,mean)
# [1]  0.5013109 -0.2380035  0.2630158
true.beta
# [1]  0.50 -0.25  0.10
compute.ZINB.DIC(res3,dim(res3$Beta)[1],3)
# [1] 5723.374


# Bayesian Spatio-Temporal Zero Inflated Negative Binomial Model (Nonlinear Time Trend)
# res4 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=FALSE, nchain=3, nsim=2000, nburn=1000)
res4 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=FALSE, nchain=3, nsim=200, nburn=100)
glimpse(res4)
# $ Alpha : num [1:1000, 1:9, 1:3] 1.062 0.784 1.287 1.046 1.233 ...
# $ Beta  : num [1:1000, 1:9, 1:3] 0.398 0.3208 0.293 -0.0194 0.0964 ...
# $ R     : num [1:1000, 1:3] 0.895 0.91 0.91 0.916 0.914 ...
# $ R2    : num [1:1000, 1:3] 0.885 0.93 0.842 0.982 0.944 ...
# $ Sigphi: num [1:1000, 1:16, 1:3] 0.309 0.268 0.273 0.186 0.144 ...
# $ PHI1  : num [1:1000, 1:99, 1:3] -0.252 -0.256 -0.302 -0.189 -0.132 ...
# $ PHI2  : num [1:1000, 1:99, 1:3] -1.166 -0.779 0.235 0.558 -0.563 ...
# $ PHI3  : num [1:1000, 1:99, 1:3] -0.0489 -0.0787 0.0477 -0.1315 0.1258 ...
# $ PHI4  : num [1:1000, 1:99, 1:3] -0.1665 -0.3008 0.0048 -0.1617 -0.0953 ...
# $ Eta1  : num [1:1000, 1:2376, 1:3] 0.512 0.222 0.724 0.606 0.827 ...
# $ Eta2  : num [1:1000, 1:2376, 1:3] 0.59523 0.45777 0.34461 0.44517 -0.00195 ...
# $ I     : num [1:1000, 1:2376, 1:3] 1 1 1 1 1 1 1 1 1 1 ...

conv.test(res4$Alpha)
conv.test(res4$Beta)
conv.test(res4$R)
conv.test(res4$R2)
par(mfrow=c(1,3));plot(res4$R[,1]);plot(res4$R[,2]);plot(res4$R[,3])
par(mfrow=c(1,3));plot(res4$Alpha[,1,1],type='o');plot(res4$Alpha[,1,2],type='o');plot(res4$Alpha[,1,3],type='o')

apply(res4$Beta,2,mean)
# [1]  0.37675203 -0.25020238  0.55096343 -0.04687415  0.48697846  0.27652648  0.58710402  0.08147175  0.65011431
true.beta
# [1]  0.50 -0.25  0.10
mean(res4$R)
compute.ZINB.DIC(res4,dim(res4$Beta)[1],3)
# [1] 6401.966


################################################################################
# RESULT SUMMARY
################################################################################

# Result Summary Table (simple)
# glimpse(res3)
# colnames(res3$Alpha) <- c("","x","t")
# colnames(res3$Beta)  <- c("","x","t")
ResultTableSummary(res3)


# Result Summary Table  (Full)
# ResultTableSummary2(y, X, A, nt=24, nchain=3, nsim=2000, nburn=1000, nthin=1)
ResultTableSummary2(y, X, A, nt=24, nchain=3, nsim=200, nburn=100, nthin=1)

# Spatial Map Visualization

# Map Visualization : Time-averaged Eta
this_result_dat <- this_dat
this_result_dat$y <- apply(res4$Eta1,2,mean)
this_result_dat %>% head
USDmapCount(state.sel = c("IA"),
            dat = this_result_dat,
            scol  = 1,
            cname = countyname)

# Map Visualization : Spatio-temporal Random Effect of Eta 
this_result_dat <- this_dat
Phi1<-rep(apply(res4$PHI1,2,mean),nis)
Phi2<-rep(apply(res4$PHI2,2,mean),nis)
tt <- this_dat$tid / max(this_dat$tid)
this_result_dat$y <- Phi1+Phi2*tt
USDmapCount(state.sel = c("IA"),
            dat = this_result_dat,
            scol  = 1,
            tcol  = 2,
            cname = countyname)

# Map Visualization : Temporal Fixed Effect of Eta 
this_result_dat <- this_dat
Phi1<-rep(apply(res4$PHI1,2,mean),nis)
Phi2<-rep(apply(res4$PHI2,2,mean),nis)
tt <- this_dat$tid / max(this_dat$tid)
this_result_dat$y <- apply(res4$Eta1,2,mean) - Phi1+Phi2*tt
USDmapCount(state.sel = c("IA"),
            dat = this_result_dat,
            scol  = 1,
            tcol  = 2,
            cname = countyname)

# Map Visualization : Time-averaged probability at risk of disease 
this_result_dat <- this_dat
this_result_dat$y <- inv.logit(apply(res4$Eta1,2,mean))
this_result_dat %>% head
USDmapCount(state.sel = c("IA"),
            dat = this_result_dat,
            scol  = 1,
            tcol  = 2,
            cname = countyname)

# Map Visualization : Time-specified probability at risk of disease 
this_result_dat <- this_dat
this_result_dat$y <- inv.logit(apply(res4$Eta1,2,mean))
this_result_dat %>% head
USDmapCount(state.sel = c("IA"),
            dat = this_result_dat,
            scol  = 1,
            tcol  = 2,
            tsel  = 1,
            cname = countyname)


# Bar Plot of vn-quantile Probability at Risk of disease
qRankPar(state.set=c("IA"),ns=nrow(A),nt=24,cname=countyname,stfit=res3,vn=12)

# Bar Plot of Top vn number of Probability at Risk of disease
qRankParTop(state.set=c("IA"),ns=nrow(A),nt=24,cname=countyname,stfit=res3,vn=12)

# Line Plot of Nonlinear Time effect abundance
TimetrendCurve(res4,ncol(A),nt=24,countyname,vn=3,smooth.mode=FALSE)

# Line Plot of Nonlinear Time effect abundance (smoothed version)
TimetrendCurve(res3,ncol(A),nt=24,countyname,vn=5,smooth.mode=TRUE)



