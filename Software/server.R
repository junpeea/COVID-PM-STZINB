
data_sets <- c("mtcars", "morley", "rock")

shinyServer(function(input, output, session) {
  
  # #input the dataframe
  df <- reactive({
    read.csv(file="./All1v6.csv", head=TRUE,sep=",")
    })
  df2 <- reactive({
    read.csv(file="./All2v5.csv", head=TRUE,sep=",")
    })
  
  # update.time <- with_tz(now(), tzone = "EST")
  update.time <- with_tz(today("EST")-3, tzone = "EST")
  
  all_st <- usmap::us_map(regions="states")
  pal <- wes_palette("Royal2", 100, type = "continuous")
  
  #input change probability
  change_prob <- reactive({
    input$chng
  })
  
  # Check boxes
  output$choose_state <- renderUI({
    
    colnames <- unique(df()$state)
    
    # Create the checkboxes and select them all by default
    selectInput("state", "Choose State", 
                choices  = colnames, selected = "NY")
  })
  
  state <- reactive({
    input$state
  })
  
  
  subdf <- reactive({
    df()[df()$state == state(), ]
  })
  
  
  dates <- reactive({
    subdf()$date
  })
  
  chng_dates <- reactive({
    dates()[plotdf()$chng_dates == "Significant Chance of Change"] %>% na.omit() %>% as.vector()
  })
  
  #Plotdataframe
  output$pltdf <- renderTable({
    plotdf()
  })
  
  
  ##################### Tab - 2 + #######################
  
  
  this_date <- reactive({
    input$choose_date
  })
  this_date1 <- reactive({
    input$choose_date1
  })
  this_date2 <- reactive({
    input$choose_date2
  })
  this_date3 <- reactive({
    input$choose_date3
  })
  this_date4 <- reactive({
    input$choose_date4
  })
  
  this_state <- reactive({
    input$choose_state
  })
  this_state1 <- reactive({
    input$choose_state1
  })
  this_state2 <- reactive({
    input$choose_state2
  })
  this_state3 <- reactive({
    input$choose_state3
  })
  this_state4 <- reactive({
    input$choose_state4
  })
  
  this_data1 <- reactive({
    input$choose_data1
  })
  this_data2 <- reactive({
    input$choose_data2
  })
  
  this_comparison_metric1 <- reactive({
    input$comparison_metric1
  })
  this_comparison_metric2 <- reactive({
    input$choose_comparison_metric2
  })
  this_comparison_metric3 <- reactive({
    input$comparison_metric3
  })
  this_comparison_metric4 <- reactive({
    input$comparison_metric4
  })
  
  this_option <- reactive({
    input$choose_option
  })
  this_option2 <- reactive({
    input$choose_option2
  })
  
  # Output the data
  output$date_table <- renderTable({
    if(this_data1()=="covidtrack"){
      if(this_option()=="daily"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "positiveIncrease", "deathIncrease", "hospitalizedIncrease","totalTestResultsIncrease")]
      }
      if(this_option()=="cum"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "positive", "death", "hospitalized","total")]
      }
      colnames(dat) <- c("State", "Confirmed", "Deaths", "Hospitalized", "Tested")
    }
    if(this_data1()=="jhu"){
      if(this_option()=="daily"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "daily_confirmed", "daily_Deaths")]
      }
      if(this_option()=="cum"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "cum_confirmed", "cum_Deaths")]
      }
      colnames(dat) <- c("State", "Confirmed", "Deaths")
    }
    
    dat2 = rbind(c(NA,as.integer(round(colSums(dat[,-1],na.rm=T),1))),dat)
    dat2$State <- c("US Total",as.character(dat$State)); dat2$State <- as.factor(dat2$State)
    dat2
  },striped = T, caption="NA: Non-Available")
  
  # comparison plot
  output$comparison_plot <- renderPlotly({
    
    if(this_data1()=="covidtrack"){
      if(this_option()=="daily"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "positiveIncrease", "deathIncrease", "hospitalizedIncrease","totalTestResultsIncrease")]
      }
      if(this_option()=="cum"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "positive", "death", "hospitalized","total")]
      }
      colnames(dat) <- c("state", "Confirmed", "Deaths", "Hospitalized", "Tested")
    }
    if(this_data1()=="jhu"){
      if(this_option()=="daily"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "daily_confirmed", "daily_Deaths")]
      }
      if(this_option()=="cum"){
        dat <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date1()), c("state", "cum_confirmed", "cum_Deaths")]
      }
      colnames(dat) <- c("state", "Confirmed", "Deaths")
    }
    
    comparison <- input$comparison_metric1
    
    dat2 = rbind(dat,c(NA,colSums(dat[,-1],na.rm=T)))
    dat2$state <- c(as.character(dat$state),"US Total"); dat2$state <- as.factor(dat2$state)
    dat2$outcome = dat2[,comparison]
    dat2 = dat2[order(dat2$outcome),]
    dat2$outbreak = factor(dat2$state, levels=dat2$state)
    
    p1 <- ggplot(dat2, aes(x = outbreak, y = log(outcome)+1, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + 
      geom_bar(alpha = 0.8, stat="identity") + ylab("") + xlab("") + theme_bw() +
      theme(legend.position = "")
    
    ggplotly(p1+scale_y_continuous(labels=NULL) + coord_flip() +
               theme(axis.title.x=element_text(colour="white", size = 14),
                     axis.text.x= element_text(colour="white", size = 14),
                     axis.text.y =element_text(colour="white", size = 14),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.background=element_rect(fill = rgb(54,69,79,max=255)),
                     panel.background = element_rect(fill =  rgb(54,69,79,max=255)),
                     legend.background = element_rect(fill =  rgb(54,69,79,max=255), color = NA)
               ),tooltip = c("text")) %>% layout(showlegend = FALSE)
    
  })
  
  output$graph_state <- renderPlotly({

    if(this_data2()=="covidtrack"){
      if(this_option2()=="daily"){
        this_data <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date2()), c("state", "positiveIncrease", "deathIncrease", "hospitalizedIncrease","totalTestResultsIncrease")]
      }
      if(this_option2()=="cum"){
        this_data <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date2()), c("state", "positive", "death", "hospitalized","total")]
      }
      colnames(this_data) <- c("state", "Confirmed", "Deaths", "Hospitalized", "Tested")
      this_data$p  <- log(this_data$Confirmed + exp(1e-10))
      this_data$d  <- log(sapply(1:nrow(this_data),function(w) max(this_data$Deaths[w],0)) + exp(1e-10))
      this_data$h  <- log(sapply(1:nrow(this_data),function(w) max(this_data$Hospitalized[w],0)) + exp(1e-10))
      this_data$t  <- log(this_data$Tested + exp(1e-10))
    }
    if(this_data2()=="jhu"){
      if(this_option2()=="daily"){
        this_data <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date2()), c("state", "daily_confirmed", "daily_Deaths")]
      }
      if(this_option2()=="cum"){
        this_data <- df()[as.Date(df()$date, format='%Y-%m-%d') == (this_date2()), c("state", "cum_confirmed", "cum_Deaths")]
      }
      colnames(this_data) <- c("state", "Confirmed", "Deaths")
      this_data$p  <- log(this_data$Confirmed + exp(1e-10))
      this_data$d  <- log(sapply(1:nrow(this_data),function(w) max(this_data$Deaths[w],0)) + exp(1e-10))
    }

    comparison <- input$comparison_metric2
    height  <- session$clientData$output_graph_state_height
    
    if(comparison == "Confirmed"){
      this_data$hover <- with(this_data, paste0("Positive: ", Confirmed, "\n", "State: ", height))
      mapdf <- merge(all_st, this_data, by.x = "full", by.y = "state")
      mapdf <- arrange(mapdf, group, order)
      break.guide <- quantile(this_data$p,probs=seq(0,1,0.10),na.rm=T)
      label.guide <- as.integer(round(exp(break.guide) - 1, digits=0))
      p <- ggplot(data = mapdf, aes(x = x, y = y, group=group, text = hover)) +
        geom_polygon(aes(fill = p), size=0.2) +
        geom_polygon(fill = NA, color="#FFFFFF", size=0.2) +
        scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,
                           breaks=break.guide[!duplicated(label.guide)],
                           labels=label.guide[!duplicated(label.guide)]) +
        labs(fill= "",
             title= paste(as.character(this_date2()),
                          ": Statewise Positive COVID-19 Cases in US"), x="", y="")
    }else if(comparison == "Deaths"){
      this_data$hover <- with(this_data, paste0("Deaths: ", Deaths, "\n", "State: ", state))
      mapdf <- merge(all_st, this_data, by.x = "full", by.y = "state")
      mapdf <- arrange(mapdf, group, order)
      break.guide <- quantile(this_data$d,probs=seq(0,1,0.10),na.rm=T)
      label.guide <- as.integer(round(exp(break.guide) - 1, digits=0))
      p <- ggplot(data = mapdf, aes(x = x, y = y, group=group, text = hover)) +
        geom_polygon(aes(fill = d), size=0.2) +
        geom_polygon(fill = NA, color="#FFFFFF", size=0.2) +
        scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,
                           breaks=break.guide[!duplicated(label.guide)],
                           labels=label.guide[!duplicated(label.guide)]) +
        labs(fill= "",
             title= paste(as.character(this_date2()),
                          ": Statewise Death COVID-19 Cases in US"), x="", y="")
    }else if(comparison=="Tested"){
      this_data$hover <- with(this_data, paste0("Tested: ", Tested, "\n", "State: ", state))
      mapdf <- merge(all_st, this_data, by.x = "full", by.y = "state")
      mapdf <- arrange(mapdf, group, order)
      break.guide <- quantile(this_data$t,probs=seq(0,1,0.10),na.rm=T)
      label.guide <- as.integer(round(exp(break.guide) - 1, digits=0))
      p <- ggplot(data = mapdf, aes(x = x, y = y, group=group, text = hover)) +
        geom_polygon(aes(fill = t), size=0.2) +
        geom_polygon(fill = NA, color="#FFFFFF", size=0.2) +
        scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,
                           breaks=break.guide[!duplicated(label.guide)],
                           labels=label.guide[!duplicated(label.guide)]) +
        labs(fill= "",
             title= paste(as.character(this_date2()),
                          ": Statewise Tested COVID-19 Cases in US"), x="", y="")
    }else{
      this_data$hover <- with(this_data, paste0("Hospitalized: ", Hospitalized, "\n", "State: ", state))
      mapdf <- merge(all_st, this_data, by.x = "full", by.y = "state")
      mapdf <- arrange(mapdf, group, order)
      break.guide <- quantile(this_data$h,probs=seq(0,1,0.10),na.rm=T)
      label.guide <- as.integer(round(exp(break.guide) - 1, digits=0))
      p <- ggplot(data = mapdf, aes(x = x, y = y, group=group, text = hover)) +
        geom_polygon(aes(fill = h), size=0.2) +
        geom_polygon(fill = NA, color="#FFFFFF", size=0.2) +
        scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,
                           breaks=break.guide[!duplicated(label.guide)],
                           labels=label.guide[!duplicated(label.guide)]) +
        labs(fill= "",
             title= paste(as.character(this_date2()),
                          ": Statewise Hospitalized Cases due to COVID-19 in US"), x="", y="")

    }
    
    ggplotly(p +
               theme(plot.title = element_text(colour="white", size = 18),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     legend.title = element_text(colour="white", size = 15),
                     legend.text = element_text(colour="white", size = 12),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     plot.background=element_rect(fill = "black"),
                     panel.background = element_rect(fill = "black"),
                     legend.background = element_rect(fill = "black", color = NA)
               )
    )

  })
  
  output$date_table2 <- renderTable({
    dat <- df2()[as.Date(df2()$date, format='%Y-%m-%d') == (this_date3()) & df2()$state == this_state3(),
                 c("state","county", "Confirmed", "Deaths")]
    colnames(dat) <- c("State","County","Confirmed", "Deaths")
    # dat
    dat2 = rbind(c(NA,NA,as.integer(round(colSums(dat[,-c(1,2)]),1))),dat)
    dat2$State  <- c(as.character(dat[2,"State"]),as.character(dat$State))
    dat2$County <- c(as.character("Total"),as.character(dat$County))
    dat2$State <- as.factor(dat2$State); dat2$County <- as.factor(dat2$County)
    dat2
  },striped = T)
  
  
  # # comparison plot
  output$comparison_plot2 <- renderPlotly({
    
    key <- table(df2()$state)
    
    dat <- df2()[as.Date(df2()$date, format='%Y-%m-%d') == (this_date3()) & df2()$state == names(key[this_state3()]),
                 c("state","county", "Confirmed", "Deaths")]
    colnames(dat) <- c("State","County","Confirmed", "Deaths")
    
    comparison <- input$comparison_metric3
    
    dat2 = rbind(dat,c(NA,NA,colSums(dat[,-c(1,2)])))
    dat2$State <- c(as.character(dat$State),"State")
    dat2$County <- c(as.character(dat$County),paste(dat$State[1],"Total"))
    dat2$State <- as.factor(dat2$State); dat2$County <- as.factor(dat2$County)
    
    dat2$outcome = dat2[,comparison]
    dat2 = dat2[order(dat2$outcome),]
    dat2$outbreak = factor(dat2$County, levels=dat2$County)
    
    p1 <- ggplot(dat2, aes(x = outbreak, y = log(outcome), fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
      ylab("") + xlab("") + theme_bw() +
      theme(legend.position = "")
    
    ggplotly(p1+scale_y_continuous(labels=NULL) + coord_flip() +
               theme(axis.title.x=element_text(colour="white", size = 14),
                     axis.text.x= element_text(colour="white", size = 14),
                     axis.text.y =element_text(colour="white", size = 14),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.background=element_rect(fill = rgb(54,69,79,max=255)),
                     panel.background = element_rect(fill =  rgb(54,69,79,max=255)),
                     legend.background = element_rect(fill =  rgb(54,69,79,max=255), color = NA)
               ),tooltip = c("text")) %>% layout(showlegend = FALSE)
    
  })
  
  
  output$graph_county <- renderPlotly({
    this_data <- df2()[as.Date(df2()$date, format='%Y-%m-%d') == (this_date4()) & df2()$state == this_state4(),
                       c("state","county", "Confirmed", "Deaths")]
    this_data$district <- with(this_data, paste(state, county, "County"))
    if(this_state4() == "Alaska"){
      keytable = read.csv("Alaska_county_key.csv")
      temp <- merge(this_data, keytable, by.x = "district", by.y = 3, all.x = TRUE)
      this_data$district <- temp$County
    }
    
    this_data$p  <- log(this_data$Confirmed + exp(1e-10))
    this_data$d  <- log(sapply(1:nrow(this_data),function(w) max(this_data$Deaths[w],0)) + exp(1e-10))
    
    this_map = usmap::us_map(regions="counties")
    this_map$address <- with(this_map, paste(full, county))
    
    comparison <- input$comparison_metric4
    
    if(comparison == "Confirmed"){
      this_data$hover <- with(this_data, paste0("Positive: ", Confirmed, "\n", "County: ", county))
      mapdf <- merge(this_map, this_data, by.x = "address", by.y = "district")
      mapdf <- arrange(mapdf, group, order)
      if(max(this_data$p) < log(11)){
        break.guide = seq(0,max(this_data$p),length.out=8)
      }else{
        break.guide = quantile(this_data$p,probs=seq(0,1,0.10),na.rm=T)
      }
      label.guide = as.integer(round(exp(break.guide) - 1, digits=0))
      p <- ggplot(data = mapdf, aes(x = x, y = y, group=group, text = hover)) +
        geom_polygon(aes(fill = p), size=0.2) +
        geom_polygon(fill = NA, color="#FFFFFF", size=0.2) +
        scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,
                           breaks=break.guide[!duplicated(label.guide)],
                           labels=label.guide[!duplicated(label.guide)]) +
        labs(fill= "",
             title= paste(as.character(this_date4()),
                          paste0(": Countywise Positive COVID-19 Cases in ", this_state4(),", US")), x="", y="")
    }else if(comparison=="Deaths"){
      this_data$hover <- with(this_data, paste0("Deaths: ", Deaths, "\n", "County: ", county))
      mapdf <- merge(this_map, this_data, by.x = "address", by.y = "district")
      mapdf <- arrange(mapdf, group, order)
      if(max(this_data$d) < log(11)){
        break.guide = seq(0,max(this_data$d),length.out=8)
      }else{
        break.guide = quantile(this_data$d,probs=seq(0,1,0.10),na.rm=T)
      }
      label.guide = as.integer(round(exp(break.guide) - 1, digits=0))
      p <- ggplot(data = mapdf, aes(x = x, y = y, group=group, text = hover)) +
        geom_polygon(aes(fill = d),size=0.2) +
        geom_polygon(fill = NA, color="#FFFFFF", size=0.2) +
        scale_fill_viridis(option="plasma",begin=0, end=0.8, direction=-1,alpha=.8,
                           breaks=break.guide[!duplicated(label.guide)],
                           labels=label.guide[!duplicated(label.guide)]) +
        labs(fill= "",
             title= paste(as.character(this_date4()),
                          paste0(": Countywise Deaths COVID-19 Cases in ", this_state4(),", US")), x="", y="")
    }
    
    ggplotly(p +
               theme(plot.title = element_text(colour="white", size = 18),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     legend.title = element_text(colour="white", size = 15), 
                     legend.text = element_text(colour="white", size = 12), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     plot.background=element_rect(fill = "black"),
                     panel.background = element_rect(fill = "black" ),
                     legend.background = element_rect(fill =  "black", color = NA)
               )
    )
  })
  
  output$InputSlider <- renderUI({
    
    this.date <- this_date1()
    if(is.null(this_date1()) == TRUE){ this.date <- today("EST") - 2 }
    if(this_data1()=="covidtrack"){
      min.date = as.Date('2020-01-23', timeFormat="%Y-%m-%d")
    }else{
      min.date = as.Date('2020-03-22', timeFormat="%Y-%m-%d")
    }
    sliderInput("choose_date1", "Select the date",
                min = min.date,
                max = today("EST") - 2,
                value = this.date
    )
  })
  output$InputSlider2 <- renderUI({
    
    this.date <- this_date2()
    if(is.null(this_date2()) == TRUE){ this.date <- today("EST") - 2 }
    if(this_data2()=="covidtrack"){
      min.date = as.Date('2020-01-23', timeFormat="%Y-%m-%d")
    }else{
      min.date = as.Date('2020-03-22', timeFormat="%Y-%m-%d")
    }
    sliderInput("choose_date2", "Select the date",
                min = min.date,
                max = today("EST") - 2,
                value = this.date
    )
  })
  
  output$InputComparisonSelect <- renderUI({
    
    if(this_data1()=="covidtrack"){
      selectInput("comparison_metric1", "Select comparison:",
                  c("Confirmed","Deaths","Hospitalized","Tested")
      )
    }else{
      selectInput("comparison_metric1", "Select comparison:",
                  c("Confirmed","Deaths")
      )
    }
    
  })
  output$InputComparisonSelect2 <- renderUI({
    
    if(this_data2()=="covidtrack"){
      selectInput("comparison_metric2", "Select comparison:",
                  c("Confirmed","Deaths","Hospitalized","Tested")
      )
    }else{
      selectInput("comparison_metric2", "Select comparison:",
                  c("Confirmed","Deaths")
      )
    }
    
  })
  
  output$foot_notes_1 <- renderText({paste("Last updated:",update.time, tz(update.time))})
  output$foot_notes_2 <- renderText({paste("Last updated:",update.time, tz(update.time))})
  output$foot_notes_3 <- renderText({paste("Last updated:",update.time, tz(update.time))})
  output$foot_notes_4 <- renderText({paste("Last updated:",update.time, tz(update.time))})
  

})  




