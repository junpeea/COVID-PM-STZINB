
rm(list=ls())
########################################################################################################################
source('utilities.R')
start.date <- "2020-03-23"  # Don't go before this date
end.date <- "2020-08-31"
cross.date <- "2020-08-23"

state <- "New Jersey"
state.abbr1 <- "NJ"
mydat.NJ <- generate_data(state = state,
                          state.abbr = state.abbr1,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.NJ$aggregate.state)# [1] 21 98

state <- "New York"
state.abbr2 <- "NY"
mydat.NY <- generate_data(state = state,
                          state.abbr = state.abbr2,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.NY$aggregate.state)# [1] 62 98

state <- "Pennsylvania"
state.abbr3 <- "PA"
mydat.PA <- generate_data(state = state,
                          state.abbr = state.abbr3,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.PA$aggregate.state) # [1] 67 98

# Combine datasets under Division 5
mydat.div2 = list()
mydat.div2$deaths.county = rbind(mydat.NJ$deaths.county,mydat.NY$deaths.county,mydat.PA$deaths.county)
mydat.div2$cum.deaths.county = rbind(mydat.NJ$cum.deaths.county,mydat.NY$cum.deaths.county,mydat.PA$cum.deaths.county)
mydat.div2$aggregate.state   = rbind(mydat.NJ$aggregate.state,mydat.NY$aggregate.state,mydat.PA$aggregate.state)

county.adjacency <- read.csv("county_adjacency2010.csv", header = TRUE)
county.names1 <- trimws(unlist(lapply(county.adjacency$Countyname, function(x) unlist(strsplit(as.character(x), ","))[1])))
county.names2 <- trimws(unlist(lapply(county.adjacency$neighborname, function(x) unlist(strsplit(as.character(x), ","))[1])))
state.abbs1 <- trimws(unlist(lapply(county.adjacency$Countyname, 
                                    function(x) tail(unlist(strsplit(as.character(x), ",")),n=1))))
state.abbs2 <- trimws(unlist(lapply(county.adjacency$neighborname, 
                                    function(x) tail(unlist(strsplit(as.character(x), ",")),n=1))))
state.abbs <- unique(union(state.abbs1, state.abbs2))
county.names1.full <- unlist(lapply(1:length(county.names1), function(x) paste(county.names1[x], state.abbs1[x], sep="-")))
county.names2.full <- unlist(lapply(1:length(county.names2), function(x) paste(county.names2[x], state.abbs2[x], sep="-")))

get.county <- function(state) {
  if (state %in% unique(state.abbs1)) {
    return (unique(county.names1.full[state.abbs1 %in% state]))
  } else {
    return (unique(county.names2.full[state.abbs2 %in% state]))
  }
}

county.list <- list()
for (state in state.abbs) {
  county.list <- c(county.list, get.county(state))
}
county.list <- unlist(county.list)
n.county <- length(county.list)

county.adj.mat <- matrix(0, nrow=n.county, ncol=n.county)
for (i in 1:dim(county.adjacency)[1]) {
  inx.county <- which(county.list == county.names1.full[i])
  inx.neighbor.county <- which(county.list == county.names2.full[i])
  county.adj.mat[inx.county, inx.neighbor.county] = 1
  county.adj.mat[inx.neighbor.county, inx.county] = 1
}

idx <- which(county.list %in% c(get.county("NJ"),get.county("NY"),get.county("PA")))
mydat.div2$adj.mat <-  county.adj.mat[idx,idx]

mydat <- mydat.div2

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

################################################################################################################################
# Spatio-Temporal model
state.abbr <- paste0(state.abbr1,state.abbr2,state.abbr3)
################################################################################################################################
# source('zinb_county_corona_v1_new.R')
source('zinb_county_corona_v1_revised.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        # cumulative=FALSE,chain.seed=c(20000,50000,80000),
                                        # nsim=101000,
                                        # burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC # [1] 10017.22
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv1_zinb_daily_with_offset.Rdata"))

# source('zinb_county_corona_v3_new.R')
source('zinb_county_corona_v3_revised.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        # cumulative=FALSE,chain.seed=c(20000,60000,80000),
                                        # nsim=101000,
                                        # burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv3_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv3_zinb_daily_with_offset.Rdata"))

# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
zinb.daily.offset <- zinb_county_corona(state=state,
                                        adj.mat=mydat$adj.mat,
                                        aggregate.state = mydat$aggregate.state,
                                        positives.county = mydat$week_cum.deaths.county,
                                        cumulative=FALSE,chain.seed=c(2000,5000,8000),
                                        nsim=11000,
                                        burn=1000,
                                        # cumulative=FALSE,chain.seed=c(20000,40000,70000),
                                        # nsim=101000,
                                        # burn=1000,
                                        offset = TRUE,
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_LCY.csv"))

# zinb.daily.offset$Beta[,(dim(Bmatrix)[2]+2)] %>% plot(type='l')
zinb.daily.offset$Beta %>% apply(2,function(w) quantile(w,probs=c(0,0.025,0.5,0.975,1)))
# save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset.Rdata"))
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_LCY.Rdata"))


################################################################################################################################
# Spatial model
################################################################################################################################
# source('nb_cross_county_corona_v2_new.R')
source('nb_cross_county_corona_v2_revised.R')
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
# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
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
# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
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
# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
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
# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
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
# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
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
# source('zinb_county_corona_v4_new.R')
source('zinb_county_corona_v4_revised.R')
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
# source('zinb_county_corona_v4(2)_new.R')
source('zinb_county_corona_v4(2)_revised.R')
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

