################################################################################
# Vignette code
# Produced by YB Jun (24.01.01)
source("Library_v7.R")
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
# $ Beta  : num [1:500, 1:2, 1:3] -0.0539 0.0197 -0.0364 0.0148 -0.0196 ...
# ..- attr(*, "dimnames")=List of 3
# .. ..$ : NULL
# .. ..$ : chr [1:2] "" "x"
# .. ..$ : NULL
# $ Sigphi: num [1:500, 1:4, 1:3] 0.568 0.684 0.709 0.829 0.964 ...
# $ R     : num [1:500, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
# $ PHI3  : num [1:500, 1:99, 1:3] -0.108 -0.477 -0.625 0.176 -0.115 ...
# $ PHI4  : num [1:500, 1:99, 1:3] -0.369 0.162 -0.129 -0.211 -0.254 ...
# $ Eta   : num [1:500, 1:2376, 1:3] -0.2403 -0.0961 -0.3267 -0.5808 0.2698 ...

# Bayesian Spatia-Temporal Negative Binomial Model
res2 = BSTNB(y, X, A, nt=24, oind = NULL, nchain=3, nsim=1000, nburn=500)
glimpse(res2)
# List of 7
# $ Beta  : num [1:500, 1:2, 1:3] 0.277 0.247 0.178 0.191 0.249 ...
# ..- attr(*, "dimnames")=List of 3
# .. ..$ : NULL
# .. ..$ : chr [1:2] "" "x"
# .. ..$ : NULL
# $ R     : num [1:500, 1:3] 0.779 0.782 0.809 0.803 0.772 ...
# $ Sigphi: num [1:500, 1:4, 1:3] 0.506 0.493 0.428 0.482 0.425 ...
# $ PHI3  : num [1:500, 1:99, 1:3] -0.444 -0.2 -0.354 -0.302 -0.258 ...
# $ PHI4  : num [1:500, 1:99, 1:3] -0.0488 0.0102 -0.114 -0.2014 -0.637 ...
# $ Eta   : num [1:500, 1:2376, 1:3] 0.2259 -0.1561 0.0842 -0.0901 -0.0137 ...
# Bayesian Spatio-Temporal Zero Inflated Negative Binomial Model (Linear Time Trend)
# res3 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=TRUE, nchain=3, nsim=2000, nburn=1000, nthin=1)

res3 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=TRUE, nchain=3, nsim=1000, nburn=500, nthin=1)
glimpse(res3)
# List of 12
# $ Alpha : num [1:500, 1:3, 1:3] -0.0317 -0.1615 -0.1845 -0.0575 -0.1485 ...
# ..- attr(*, "dimnames")=List of 3
# .. ..$ : NULL
# .. ..$ : chr [1:3] "" "x" "t"
# .. ..$ : NULL
# $ Beta  : num [1:500, 1:3, 1:3] 0.54 0.466 0.518 0.392 0.419 ...
# ..- attr(*, "dimnames")=List of 3
# .. ..$ : NULL
# .. ..$ : chr [1:3] "" "x" "t"
# .. ..$ : NULL
# $ R     : num [1:500, 1:3] 1.3 1.3 1.33 1.33 1.33 ...
# $ R2    : num [1:500, 1:3] 1.33 1.3 1.39 1.34 1.36 ...
# $ Sigphi: num [1:500, 1:16, 1:3] 0.558 0.582 0.68 0.644 0.601 ...
# $ PHI1  : num [1:500, 1:99, 1:3] -0.0997 -0.1918 -0.5985 -0.9966 -0.7178 ...
# $ PHI2  : num [1:500, 1:99, 1:3] 0.0444 -0.4885 -1.0269 -0.6949 -1.0702 ...
# $ PHI3  : num [1:500, 1:99, 1:3] 0.022 0.2092 -0.0126 0.2908 0.0551 ...
# $ PHI4  : num [1:500, 1:99, 1:3] 0.1772 0.1204 -0.0371 0.0937 0.2543 ...
# $ Eta1  : num [1:500, 1:2376, 1:3] -0.626 -0.858 -1.237 -1.511 -1.417 ...
# $ Eta2  : num [1:500, 1:2376, 1:3] 0.849 0.785 0.972 0.746 0.957 ...
# $ I     : num [1:500, 1:2376, 1:3] 1 1 1 1 1 1 1 1 1 1 ...
conv.test(res3$Alpha)
conv.test(res3$Beta)
conv.test(res3$R)
par(mfrow=c(1,3));plot(res3$R[,1]);plot(res3$R[,2]);plot(res3$R[,3])
par(mfrow=c(1,3));plot(res3$Alpha[,1,1],type='o');plot(res3$Alpha[,1,2],type='o');plot(res3$Alpha[,1,3],type='o')

apply(res3$Beta,2,mean)
# [1] 0.5808066 -0.2359510  0.2817165 
true.beta
# [1]  0.50 -0.25  0.10
compute.ZINB.DIC(y,res3,dim(res3$Beta)[1],3)
# [1] 6173.019


# Bayesian Spatio-Temporal Zero Inflated Negative Binomial Model (Nonlinear Time Trend)
res4 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=FALSE, nchain=3, nsim=2000, nburn=1000)
# res4 = BSTZINB(y, X, A, nt=24, oind = NULL, LinearT=FALSE, nchain=3, nsim=200, nburn=100)
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
par(mfrow=c(1,3));plot(res4$R[,1]);plot(res4$R[,2]);plot(res4$R[,3])
par(mfrow=c(1,3));plot(res4$Alpha[,1,1],type='o');plot(res4$Alpha[,1,2],type='o');plot(res4$Alpha[,1,3],type='o')

apply(res4$Beta,2,mean)
#                       x          t1          t2          t3          t4 
# 0.37675203 -0.25020238  0.55096343 -0.04687415  0.48697846  0.27652648 
# t5          t6          t7 
# 0.58710402  0.08147175  0.65011431 
mean(res4$R)
# [1] 1.114724
compute.ZINB.DIC(y,res4,dim(res4$Beta)[1],3)
# [1] 6401.966


################################################################################
# RESULT SUMMARY
################################################################################
# Result Summary Table (simple)
ResultTableSummary(res3)

# Result Summary Table  (Full)
ResultTableSummary2(y, X, A, nt=24, nchain=3, nsim=2000, nburn=1000, nthin=1)
# ResultTableSummary2(y, X, A, nt=24, nchain=3, nsim=200, nburn=100, nthin=1)
# ResultTableSummary2(y, X, A, nt=24, nchain=3, nsim=2000, nburn=1000, nthin=10)

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



