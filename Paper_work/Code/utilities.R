library(RJSONIO)
library(RCurl)
library(dplyr)
library(stringr)

generate_data <- function(state, state.abbr, start.date, end.date) {
  
  ########################################################################################################
  ##################################### Generating Adjacency Matrix  #####################################
  ########################################################################################################
  
  org.state <- state
  
  county.adjacency <- read.csv("county_adjacency2010.csv", header = TRUE)
  
  county.names1 <- trimws(unlist(lapply(county.adjacency$Countyname, function(x) unlist(strsplit(as.character(x), ","))[1])))
  county.names2 <- trimws(unlist(lapply(county.adjacency$neighborname, function(x) unlist(strsplit(as.character(x), ","))[1])))
  
  state.abbs1 <- trimws(unlist(lapply(county.adjacency$Countyname, 
                                      function(x) tail(unlist(strsplit(as.character(x), ",")),n=1))))
  state.abbs2 <- trimws(unlist(lapply(county.adjacency$neighborname, 
                                      function(x) tail(unlist(strsplit(as.character(x), ",")),n=1))))
  state.abbs <- unique(union(state.abbs1, state.abbs2))
  
  state.nbr <- unique(unlist(lapply(1:length(state.abbs1), function(x) paste(state.abbs1[x],"-",state.abbs2[x], sep = ""))))
  state.adj.mat <- matrix(0, nrow=length(state.abbs), ncol=length(state.abbs))
  for (i in 1:length(state.nbr)) {
    nbr.states <- unlist(strsplit(state.nbr[i], split="-"))
    idx1 <- which(state.abbs==nbr.states[1])
    idx2 <- which(state.abbs==nbr.states[2])
    state.adj.mat[idx1,idx2] = 1
    state.adj.mat[idx2,idx1] = 1
  }
  state.adj.mat <- state.adj.mat[state.abbs %in% state.abb,state.abbs %in% state.abb]
  
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
  
  
  ####################################################################################################################
  ##############################################  Runing ZINB on Corona  #############################################
  ####################################################################################################################
  
  state <- org.state
  # n.dates <- as.Date("2020-05-27", format="%Y-%m-%d") - as.Date("2020-03-22", format="%Y-%m-%d")
  n.dates <- as.Date(end.date, format="%Y-%m-%d") - as.Date(start.date, format="%Y-%m-%d") + 1
  positives.mat <- matrix(0, nrow=length(get.county(state.abbr)), ncol=n.dates)
  # start.date <- as.Date(start.date, format="%Y-%m-%d")-1
  start.date <- as.Date(start.date, format="%Y-%m-%d")
  
  idx <- which(county.list %in% get.county(state.abbr))
  adj.mat.state <- county.adj.mat[idx,idx]
  
  # Import exposure PM2.5 data
  county_pm = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))
  
  county_temp = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv"))
  # Import census, brfss, testing, mortality, hosptial beds data as potential confounders
  county_census = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
  county_brfss = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/brfss_county_interpolated.csv"))
  
  statecode = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/statecode.csv"))
  
  hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
  hospitals$BEDS[hospitals$BEDS < 0] = 0
  
  county_base_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_base_mortality.txt"), sep = "",header = T)
  county_old_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_old_mortality.txt"), sep = "",header = T)
  colnames(county_old_mortality)[4] = c("older_Population")
  county_base_mortality = merge(county_base_mortality,county_old_mortality[,c(2,4)] ,by = "County.Code")
  county_base_mortality$older_pecent = county_base_mortality$older_Population/county_base_mortality$Population
  
  # pm average over 17 years
  county_pm_aggregated = county_pm %>% 
    group_by(fips) %>% 
    summarise(mean_pm25 = mean(pm25,na.rm=T))
  
  # pm average over 17 years
  county_temp_aggregated = county_temp %>% 
    group_by(fips) %>% 
    summarise(mean_winter_temp= mean(winter_tmmx,na.rm=T),
              mean_summer_temp= mean(summer_tmmx,na.rm=T),
              mean_winter_rm= mean(winter_rmax,na.rm=T),
              mean_summer_rm= mean(summer_rmax,na.rm=T))
  
  county_pm_aggregated = merge(county_pm_aggregated,county_temp_aggregated,by="fips",all.x = T)
  
  county_hospitals_aggregated = hospitals %>%
    group_by(COUNTYFIPS) %>%
    summarise(beds = sum(BEDS))
  
  county_census_aggregated2 = subset(county_census, year==2016)
  county_census_aggregated2$q_popdensity = 1
  quantile_popdensity = quantile(county_census_aggregated2$popdensity,c(0.25,0.5,0.75))
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity<=quantile_popdensity[1]] = 1
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[1] &
                                           county_census_aggregated2$popdensity<=quantile_popdensity[2]] = 2
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[2] &
                                           county_census_aggregated2$popdensity<=quantile_popdensity[3]] = 3
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[3]] = 4
  
  county_brfss_aggregated = subset(county_brfss, year==2012)
  
  county_census_aggregated2 = merge(county_census_aggregated2,county_brfss_aggregated,
                                    by="fips",all.x=T)
  
  positives.county <- matrix(0, nrow=length(get.county(state.abbr)), ncol=n.dates+1)
  deaths.county <- matrix(0, nrow=length(get.county(state.abbr)), ncol=n.dates+1)
  cum.positives.county <- matrix(0, nrow=length(get.county(state.abbr)), ncol=n.dates+1)
  positives.state <- matrix(0, nrow=49, ncol=n.dates+1)
  cum.deaths.county <- matrix(0, nrow=length(get.county(state.abbr)), ncol=n.dates+1)
  deaths.state <- matrix(0, nrow=49, ncol=n.dates+1)
  recovered.state <- matrix(0, nrow=49, ncol=n.dates+1)
  
  # Historical data
  covid_hist = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
  covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)
  state_test0= read.csv(text=getURL("https://api.covidtracking.com/v1/states/daily.csv"))
  
  for (day in 1:(n.dates+1)) {
    
    date_of_study <- as.character(as.Date(start.date) + day - 1, format="%m-%d-%Y")
    
    state_test = state_test0
    indicator = paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)) %>% as.integer
    state_test = subset(state_test, date == indicator)[,-20]
    # merging data
    state_test = merge(state_test,statecode,by.x = "state" ,by.y = "Code" )
    
    
    # Import outcome data from JHU CSSE
    covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_study,".csv")))
    covid_us = subset(covid[,1:12],Country_Region == "US" & is.na(FIPS)!=T)
    # covid_us = subset(covid,Country_Region == "US" & is.na(FIPS)!=T)
    covid_us = rbind(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))

    aggregate_pm = merge(county_pm_aggregated,covid_us,by.x="fips",by.y = "FIPS")
    
    aggregate_pm_census = merge(aggregate_pm,county_census_aggregated2,by.x="fips",by.y = "fips")
    
    aggregate_pm_census_cdc = merge(aggregate_pm_census,county_base_mortality[,c(1,4:5,8:9)],by.x = "fips",by.y = "County.Code",all.x = T)
    
    aggregate_pm_census_cdc = aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) ==F,]
    
    aggregate_pm_census_cdc_test = merge(aggregate_pm_census_cdc,state_test,by.x="Province_State",by.y = "State")
    aggregate_pm_census_cdc_test = aggregate_pm_census_cdc_test %>%
      group_by(Province_State) %>%
      mutate(population_frac_county = population/sum(population),
             totalTestResults_county = population_frac_county*total)
    
    aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,by.x = "fips.x",by.y = "COUNTYFIPS",all.x = T)
    
    aggregate.state <- subset(aggregate_pm_census_cdc, Province_State==state & Admin2 != "Unassigned" & Admin2 != "Walla Walla County" & Admin2 != "Garfield County")
  
    if(state.abbr %in% c("IL","FL","GA","NC","SC","CA","OR","PA","WA","NJ","NY","IA","KS","MO","NE","ND","SD")){
      aggregate.state <- aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Province_State == state,]
      levels(aggregate.state$Admin2)[levels(aggregate.state$Admin2)=="New York City"] <- "New York"
      levels(aggregate.state$Admin2)[levels(aggregate.state$Admin2)=="St. Louis City"] <- "St. Louis city"
      stateindic = match(paste0(aggregate.state$Admin2,"-",state.abbr),get.county(state.abbr))
      cum.positives.county[stateindic,day] <- aggregate.state$Confirmed
      cum.deaths.county[stateindic,day]    <- aggregate.state$Deaths
    }else{
      cum.positives.county[,day] <- aggregate.state$Confirmed
      cum.deaths.county[,day]    <- aggregate.state$Deaths
    }
    
    state.details <- 
      aggregate_pm_census_cdc_test_beds %>% group_by(Province_State) %>%
      summarize(Confirmed = sum(Confirmed),
                Deaths = sum(Deaths),
                Recovered = sum(Recovered),
      )
    positives.state[,day] <- state.details$Confirmed
    deaths.state[,day] <- state.details$Deaths
    recovered.state[,day] <- state.details$Recovered
  }
  
  us.covariates <- aggregate_pm_census_cdc_test_beds %>% group_by(Province_State) %>%
    summarize(mean_pm25 = mean(mean_pm25),
              poverty = mean(poverty),
              popdensity = mean(popdensity),
              pct_owner_occ = mean(pct_owner_occ),
              medianhousevalue = mean(medianhousevalue),
              medhouseholdincome = mean(medhouseholdincome),
              hispanic = mean(hispanic),
              education = mean(education),
              pct_blk = mean(pct_blk),
              older_pecent = mean(older_pecent)
    )
  
  for (i in (n.dates+1):2) {
    positives.county[,i] <- cum.positives.county[,i] - cum.positives.county[,(i-1)]
    positives.state[,i] <- positives.state[,i] - positives.state[,(i-1)]
    deaths.county[,i] <- cum.deaths.county[,i] - cum.deaths.county[,(i-1)]
    deaths.state[,i] <- deaths.state[,i] - deaths.state[,(i-1)]
    recovered.state[,i] <- recovered.state[,i] - recovered.state[,(i-1)]
  }
  positives.county <- positives.county[,-1]
  positives.county <- ifelse(positives.county <0, 0, positives.county)
  positives.state <- positives.state[,-1]
  positives.state <- ifelse(positives.state <0, 0, positives.state)
  deaths.county <- deaths.county[,-1]
  deaths.county <- ifelse(deaths.county <0, 0, deaths.county)
  deaths.state <- deaths.state[,-1]
  deaths.state <- ifelse(deaths.state <0, 0, deaths.state)
  recovered.state <- recovered.state[,-1]
  recovered.state <- ifelse(recovered.state <0, 0, recovered.state)
  cum.positives.county <- cum.positives.county[,-1]
  
  return (list(positives.county=positives.county,
               cum.positives.county = cum.positives.county,
               deaths.county = deaths.county,
               cum.deaths.county = cum.deaths.county,
               aggregate.state = aggregate.state,
               adj.mat = adj.mat.state
  )
  )
}


## Plots: Positive counts
count.plot <- function(state, current.date, cumulative, positives.county) {
  ## Select the column in positives.county
  day <- as.Date(current.date, format="%Y-%m-%d") - as.Date(start.date, format="%Y-%m-%d") + 1
  
  all.st <- usmap::us_map(regions="counties")
  state.map <- all.st[all.st$full==state,]
  
  count.summary <- data.frame(County=unique(state.map$county), pos.count=positives.county[,day])
  # Please ignore any warning generated in the following line
  map.df  <- left_join(state.map, count.summary, by = c("county"="County"))
  
  if (cumulative) {
    str <- "_cumulative"
  } else {
    str <- "_daily"
  }
  
  p <- ggplot(data = map.df, aes(x = x, y = y, group=group)) +
    geom_polygon(aes(fill = pos.count), color="#FFFFFF", size=0.2) +
    scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8) +
    theme_bw() +
    labs(fill= "Covid-19:\nPositives Count",
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
  file <- paste0(state,"_positives", str, current.date, ".pdf")
  ggsave(file)
}

