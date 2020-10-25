

rm(list=ls())
########################################################################################################################
source('utilities.R')
start.date <- "2020-03-23"  # Don't go before this date
end.date <- "2020-08-31"
# end.date <- "2020-07-23"
cross.date <- "2020-08-23"
state <- "Florida"
state.abbr <- "FL"
mydat <- generate_data(state = state,
                       state.abbr = state.abbr,
                       start.date = start.date,
                       end.date = end.date)

mydat$aggregate.state$Deaths
# [1]   33    5   78    5  210 1184    8  110   57   76  176   33   22    4  254  186   15    3   31   10    3   11    6
# [24]    9   39   83   67  551    5   98   66    6    7  117  423   33    6    7   14  258  180  118 2437   16   19   77
# [47]   15  377  129 1119  180  658  443   37   57  235   55  202  172   50   60    5    5  189    7   20   16
rowSums(mydat$deaths.county)
# [1]   33    5   78    5  210 1183    8  112   57   78  176   33   23    4  254  187   16    3   31   10    3   11    6
# [24]    9   39   83   67  551    5   98   66    6    7  118  427   33    8    7   15  259  180  119 2437   17   19   77
# [47]   16  378  130 1120  182  658  443   39   57  236   54  204  174   51   62    5    5  189    8   20   17

mvd = 7
temp = cbind(matrix(0,nrow(mydat$cum.deaths.county),mvd),mydat$cum.deaths.county[,1:(ncol(mydat$cum.deaths.county)-mvd)])
week_cum.deaths.county = mydat$cum.deaths.county - temp
week_cum.deaths.county[week_cum.deaths.county < 0]  <- 0
mydat$week_cum.deaths.county <- week_cum.deaths.county[,seq(7,ncol(week_cum.deaths.county),by=7)]
mydat$week_cum.deaths.county1 <- week_cum.deaths.county[,seq((1+7),ncol(week_cum.deaths.county),by=7)];dim(mydat$week_cum.deaths.county1)
mydat$week_cum.deaths.county2 <- week_cum.deaths.county[,seq((2+7),ncol(week_cum.deaths.county),by=7)];dim(mydat$week_cum.deaths.county2)
mydat$week_cum.deaths.county3 <- week_cum.deaths.county[,seq((3+7),ncol(week_cum.deaths.county),by=7)];dim(mydat$week_cum.deaths.county3)
mydat$week_cum.deaths.county4 <- week_cum.deaths.county[,seq((4+7),ncol(week_cum.deaths.county),by=7)];dim(mydat$week_cum.deaths.county4)
mydat$week_cum.deaths.county5 <- week_cum.deaths.county[,seq((5+7),ncol(week_cum.deaths.county),by=7)];dim(mydat$week_cum.deaths.county5)
mydat$week_cum.deaths.county6 <- week_cum.deaths.county[,seq((6+7),ncol(week_cum.deaths.county),by=7)];dim(mydat$week_cum.deaths.county6)

head(mydat$week_cum.deaths.county)
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17]
# [1,]    0    6    6    8   11   13   15    9    8     0    36     6     2     1     0     0     0
# [2,]    0    1    0    0    1    0    0    0    0     0     0     0     0     0     1     0     0
# [3,]    0    0    0    0    0    0    0    0    0     0     0     0     0     0     0     0     0
# [4,]    3    1    2    3    8    5    1    5    4    12     8     0     0     6     0     2     0
# [5,]    0    0    0    0    0    0    0    0    0     0     5     0     0     1     0     0     0
# [6,]    0    0    1    0    0    0    0    0    0     0     1     0     0     0     0     0     0

################################################################################################################################
# Spatio-Temporal model
################################################################################################################################
source('zinb_county_corona_v1_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        # cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        # nsim=11000,
                                        # burn=1000,
                                        cumulative=FALSE,chain.seed=c(20000,50000,80000),
                                        nsim=101000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_zinb_daily_with_offset.Rdata"))
source('zinb_county_corona_v3_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        # cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        # nsim=11000,
                                        # burn=1000,
                                        cumulative=FALSE,chain.seed=c(20000,60000,80000),
                                        nsim=101000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv3_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv3_zinb_daily_with_offset.Rdata"))
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        # cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        # nsim=11000,
                                        # burn=1000,
                                        cumulative=FALSE,chain.seed=c(20000,40000,70000),
                                        nsim=101000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset.Rdata"))

################################################################################################################################
# Spatial model
################################################################################################################################
# source('nb_cross_county_corona.R')
source('nb_cross_county_corona_v2_new.R')
zinb.daily.offset <- nb_cross_county_corona(state=state,
                                            adj.mat=mydat$adj.mat,
                                            aggregate.state = mydat$aggregate.state,
                                            positives.county = mydat$cum.deaths.county,
                                            cumulative=TRUE,
                                            start.date = start.date,
                                            cross.date = cross.date,
                                            chain.seed=c(2000,5000,8000),
                                            nsim=11000,
                                            burn=1000,
                                            offset = TRUE,
                                            results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_nb_cum_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_nb_cum_with_offset.Rdata"))

mvd = 30
temp = cbind(matrix(0,nrow(mydat$cum.deaths.county),mvd),mydat$cum.deaths.county[,1:(ncol(mydat$cum.deaths.county)-mvd)])
month_cum.deaths.county = mydat$cum.deaths.county - temp
month_cum.deaths.county[month_cum.deaths.county < 0]  <- 0
zinb.daily.offset <- nb_cross_county_corona(state=state,
                                            adj.mat=mydat$adj.mat,
                                            aggregate.state = mydat$aggregate.state,
                                            positives.county = month_cum.deaths.county,
                                            cumulative=TRUE,
                                            start.date = start.date,
                                            cross.date = cross.date,
                                            chain.seed=c(2000,5000,8000),
                                            nsim=11000,
                                            burn=1000,
                                            offset = TRUE,
                                            results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_nb_1monthcum_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_nb_1monthcum_with_offset_.Rdata"))

mvd = 7
temp = cbind(matrix(0,nrow(mydat$cum.deaths.county),mvd),mydat$cum.deaths.county[,1:(ncol(mydat$cum.deaths.county)-mvd)])
month_cum.deaths.county = mydat$cum.deaths.county - temp
month_cum.deaths.county[month_cum.deaths.county < 0]  <- 0
zinb.daily.offset <- nb_cross_county_corona(state=state,
                                            adj.mat=mydat$adj.mat,
                                            aggregate.state = mydat$aggregate.state,
                                            positives.county = month_cum.deaths.county,
                                            cumulative=TRUE,
                                            start.date = start.date,
                                            cross.date = cross.date,
                                            chain.seed=c(2000,5000,8000),
                                            nsim=11000,
                                            burn=1000,
                                            offset = TRUE,
                                            results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_nb_1weekcum_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_nb_1weekcum_with_offset_.Rdata"))

################################################################################################################################################
# Sesnsitivity Analysis
################################################################################################################################################
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county1,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl1.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl1.Rdata"))
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county2,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl2.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl2.Rdata"))
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county3,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl3.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl3.Rdata"))
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county4,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl4.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl4.Rdata"))
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county5,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl5.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl5.Rdata"))
source('zinb_county_corona_v4_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county6,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl6.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset_suppl6.Rdata"))


# PLT #
source('zinb_county_corona_v4(2)_new.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4(2)_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4(2)_zinb_daily_with_offset.Rdata"))

