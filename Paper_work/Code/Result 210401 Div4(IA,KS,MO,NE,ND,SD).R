
rm(list=ls())
########################################################################################################################
source('utilities.R')
start.date <- "2020-03-23"  # Don't go before this date
end.date <- "2020-08-31"
cross.date <- "2020-08-23"

# West North Central (Iowa, Kansas, Missouri, Nebraska, North Dakota, and South Dakota)

state <- "Iowa"
state.abbr1 <- "IA"
mydat.IA <- generate_data(state = state,
                          state.abbr = state.abbr1,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.IA$aggregate.state) # [1] 99 98
length(get.county("IA")) # 99

state <- "Kansas"
state.abbr2 <- "KS"
mydat.KS <- generate_data(state = state,
                          state.abbr = state.abbr2,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.KS$aggregate.state) # [1] 105  98
length(get.county("KS")) # 105

state <- "Missouri"
state.abbr3 <- "MO"
mydat.MO <- generate_data(state = state,
                          state.abbr = state.abbr3,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.MO$aggregate.state) # [1] 115  98 
length(get.county("MO")) # 115

state <- "Nebraska"
state.abbr4 <- "NE"
mydat.NE <- generate_data(state = state,
                          state.abbr = state.abbr4,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.NE$aggregate.state) # [1] 93 98
length(get.county("NE")) # 93

state <- "North Dakota"
state.abbr5 <- "ND"
mydat.ND <- generate_data(state = state,
                          state.abbr = state.abbr5,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.ND$aggregate.state) # [1] 53 98
length(get.county("ND")) # 53

state <- "South Dakota"
state.abbr6 <- "SD"
mydat.SD <- generate_data(state = state,
                          state.abbr = state.abbr6,
                          start.date = start.date,
                          end.date = end.date)
dim(mydat.SD$aggregate.state) # [1] 65 98

# Combine datasets under Division 4
mydat.div4 = list()
mydat.div4$deaths.county = rbind(mydat.IA$deaths.county,mydat.KS$deaths.county,mydat.MO$deaths.county,
                                 mydat.NE$deaths.county,mydat.ND$deaths.county,mydat.SD$deaths.county)
mydat.div4$cum.deaths.county = rbind(mydat.IA$cum.deaths.county,mydat.KS$cum.deaths.county,mydat.MO$cum.deaths.county,
                                     mydat.NE$cum.deaths.county,mydat.ND$cum.deaths.county,mydat.SD$cum.deaths.county)
mydat.div4$aggregate.state   = rbind(mydat.IA$aggregate.state,mydat.KS$aggregate.state,mydat.MO$aggregate.state,
                                     mydat.NE$aggregate.state,mydat.ND$aggregate.state,mydat.SD$aggregate.state)

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

idx <- which(county.list %in% c(get.county("IA"),get.county("KS"),get.county("MO"),get.county("NE"),get.county("ND"),get.county("SD")))
idx2 = idx[idx != which(county.list %in% "Shannon-SD")]
mydat.div4$adj.mat <-  county.adj.mat[idx2,idx2]

mydat <- mydat.div4

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
state.abbr <- paste0(state.abbr1,state.abbr2,state.abbr3,state.abbr4,state.abbr5,state.abbr6)
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
                                        results.file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset.csv"))
zinb.daily.offset$DIC
save.image(file=paste0(Sys.Date(),"_",state.abbr,"_resultv4_zinb_daily_with_offset.Rdata"))


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

