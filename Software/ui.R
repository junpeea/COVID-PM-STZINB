# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library('bcp')
library('ggplot2')
library('maps')
library('RColorBrewer')
library(wesanderson)
library("viridis")
library(usmap)
library("dplyr")
library(stringr)
library(RCurl)
library(lubridate)
library(dashboardthemes)
# devtools::install_github("nik01010/dashboardthemes")
statecode = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/statecode.csv"))

dashboardPage(
  dashboardHeader(title = tags$h3("COVID-19 TITLE")),
  dashboardSidebar(
    # sliderInput("rateThreshold", "Warn when rate exceeds",
    #             min = 0, max = 50, value = 3, step = 0.1
    # ),
    sidebarMenu(
      menuItem(tags$h4("Introduction"), tabName = "intro"),
      menuItem(tags$h4("Statewise Table & Chart"), tabName = "statewise_descriptions"),
      menuItem(tags$h4("Statewise Map"), tabName = "statewise_map"),
      menuItem(tags$h4("Countywise Table & Chart"), tabName = "countywise_descriptions"),
      menuItem(tags$h4("Countywise Map"), tabName = "countywise_map")
    )
  ),
  dashboardBody(
    
    tags$style(HTML(' .skin-blue .main-sidebar {background-color:black}')),
    tags$style(HTML('.content-wrapper {background-color:black}')),
    tags$style(HTML('.sidebar-menu {background-color:black}')),
    tags$style(type = "text/css","label { font-size: 20px; }"),
    tags$style(type = "text/css",".selectize-input { font-size: 20px; line-height: 18px}"),
    tags$style(type = "text/css",".selectize-dropdown { font-size: 18px; line-height: 20px}"),
    tags$style("#foot_notes_1{font-size: 20px}"),tags$style("#foot_notes_2{font-size: 20px}"),tags$style("#foot_notes_3{font-size: 20px}"),tags$style("#foot_notes_4{font-size: 20px}"),
    
    ### changing theme
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    tabItems(
      tabItem("intro",
              tags$div(
                tags$h3(tags$span(style="color:orange","Mission")),
                tags$h4("According to the World Health Organization (WHO), this is an emergency and the whole world is going through the Coronavirus disease (COVID-19) Pandemic. Countably infinite number of people are working on this all over the world and we, as a team, would like to contribute our viewpoint as related to this Pandemic, besides our primary commitment to work."),
                tags$h4("Conditioned on the strength of our team and the dynamics of the disease, we realize that it is impossible (more specifically, the size of our team and the dimension of the problem will never ever meet the convergence of reality) to analyze the underline distribution of this Pandemic. "),
                tags$h4("Therefore, we have decided to restrict ourselves to information available only in the USA. Yes, it is still an insurmountable task for us to focus on every aspect of the Pandemic, conditioned on the diverse nature of this country. We acknowledge each and every one who is on this problem and value YOUR contribution."),
                tags$h4("Our work is solely based on publicly available data in the USA. We, as a team revere every single person who are involved in this humongous project: every minuscule COUNTS. Our primary focus is only on the US data as related to this Pandemic. "),tags$br(),
                
                tags$h3(tags$span(style="color:orange","Type of work")),
                tags$h4("What we have been seeing for the last few months is that, there have been tremendous amount of graphical interpretation of the available data along with meaningful analysis."),tags$br(),
                
                tags$h3(tags$span(style="color:orange","Visualization")),
                tags$h4("With limited resources, we are interested to visualize few important numbers as associated to this Pandemic: (1) as a function of time for the entire country (2) as a function of time for counties given a geographic region."),tags$br(),
                
                tags$h3(tags$span(style="color:orange","Modeling of the available data")),
                tags$h4("\"Location, location, and location\": Here; we want to clarify that we are MAINLY focused on the \"spatial structure of the data\". Almost all of our models are intended to incorporate geographical information available from the publicly available data sets. "),tags$br(),
                
                tags$h3(tags$span(style="color:orange","Reference pages")),
                tags$b("Reference Dashboard: ")   , tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                tags$b("COVID-19 data source 1: "), tags$a(href="https://covidtracking.com/", "The COVID Tracking Project"),tags$br(),
                tags$b("COVID-19 data source 2: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19", "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE"),tags$br(),
                tags$b("PM2.5 data source: ")     , tags$a(href="https://github.com/wxwx1993/PM_COVID", "Public available code and data to Reproduce Analyses in <Exposure to air pollution and COVID-19 mortality in the United States>."),tags$br(),
                tags$b("US Hospitalization: ")    , tags$a(href="https://hub.arcgis.com/search", "ArcGIS Hub"),tags$br(),
                tags$br(),
                
                tags$h3(tags$span(style="color:orange","Team")),
                tags$h4("First and foremost, there is no LEADER in our group. Everyone is a leader, whatever they are contributing is \"gold\" to the rest of the team. The team is listed alphabetically based on their last name. We, as a team, are pretty diverse and at the same time unique; because each of us are passionate of our contributions.
                If YOU are interested to participate in this initiative, please let us know as listed below."),
                tags$h4("1.	Sounak Chakraborty (Associate Professor of Statistics at the University of Missouri)"),
                tags$h4("2.	Tanujit Dey (Faculty at the Center for Surgery and Public Health, Brigham and Women's Hospital, Harvard Medical School)"),
                tags$h4("3.	Francesca Dominici (Professor of Biostatistics at the Harvard T.H. Chan School of Public Health and Co-Director of the Data Science Initiative at Harvard University)"),
                tags$h4("4.	Yoon-Bae Jun (Graduate student of Statistics at the Seoul National University)"),
                tags$h4("5.	Chae Young Lim (Associate Professor of Statistics at the Seoul National University)"),
                tags$h4("6.	Anish Mukherjee (Research Associate at the Case Western Reserve University School of Medicine)"),
                tags$br(),
                tags$img(src = "sounakchakrabortyweb.jpg", width = "150px", height = "180px"),
                tags$img(src = "HeadShot-TDey-300x274-e1575488516751.jpg", width = "150px", height = "180px"),
                tags$img(src = "Francesca-Dominici.jpg", width = "150px", height = "180px"),
                tags$img(src = "profile_YoonbaeJun.png", width = "150px", height = "180px"),
                tags$img(src = "profile_ChaeYoungLim.png", width = "150px", height = "180px"),
                tags$img(src = "profile_AnishMukherjee.jpg", width = "150px", height = "180px"),tags$br(),tags$br(),
                
                tags$h3(tags$span(style="color:orange","Contact")),
                tags$h4("1.	Sounak Chakraborty <chakrabortys@missouri.edu>"),
                tags$h4("2.	Tanujit Dey <tanujit.dey@gmail.com>"),
                tags$h4("3.	Francesca Dominici <fdominic@hsph.harvard.edu> "),
                tags$h4("4.	Yoon-Bae Jun <junpeea@gmail.com>"),
                tags$h4("5.	Chae Young Lim <limc.stat@gmail.com>"),
                tags$h4("6.	Anish Mukherjee <anishmk9@gmail.com>")
              )
      ),
      tabItem("statewise_descriptions",
              column(width = 2, align="left",
                     radioButtons("choose_data1", "Select the dataset:",
                                  c("COVID Tracking" = "covidtrack",
                                    "Johns Hopkins (CSSE)" = "jhu"
                                  ), inline = FALSE
                     )
              ),
              column(width = 3, align="left",
                    uiOutput("InputSlider")
              ),
              column(width = 2, align="left",
                     radioButtons("choose_option", "Select an option:",
                                  c("Daily" = "daily","Cumulative" = "cum"
                                  ), inline = FALSE
                     )
              ),
              column(width = 4, align="center",
                     uiOutput("InputComparisonSelect"),
                     textOutput("foot_notes_1")
              ),
              tabItem(tabName = "description1",
                      fluidRow(
                        column(width = 6, align="center",
                               div(tableOutput("date_table"), style = "font-size:140%")
                        ),
                        column(width = 6, align="center",
                               plotlyOutput("comparison_plot",height="2000px")
                        )
                      )
                      
              ) # close tabItem
      ),
      tabItem("statewise_map",
              column(width = 2, align="left",
                     radioButtons("choose_data2", "Select the dataset:",
                                  c("COVID Tracking" = "covidtrack",
                                    "Johns Hopkins (CSSE)" = "jhu"
                                  ), inline = FALSE
                     )
              ),
              column(width = 3, align="left",
                     uiOutput("InputSlider2")
              ),
              column(width = 2, align="left",
                     radioButtons("choose_option2", "Select an option:",
                                  c("Daily" = "daily","Cumulative" = "cum"
                                  ), inline = FALSE
                     )
              ),
              column(width = 4, align="center",
                     uiOutput("InputComparisonSelect2"),
                     textOutput("foot_notes_2")
              ),
              column(width = 12, offset = -1, align="center",
                     plotlyOutput("graph_state",width="75%",height="1350px")
              )
      ),
      tabItem("countywise_descriptions",
              column(width = 4,align="center",
                     sliderInput("choose_date3", "Select the date",
                                 min = as.Date('2020-03-22', timeFormat="%Y-%m-%d"),
                                 max = today("EST") - 3,
                                 value = today("EST") - 3
                     )
              ),
              column(width = 4,align="center",
                     selectInput("choose_state3", "Choose a state:",
                                 statecode[,1]
                     )
              ),
              column(width = 4, align="center",
                     selectInput("comparison_metric3", "Select comparison:",
                                  c("Confirmed" = "Confirmed","Deaths" = "Deaths")
                     ),
                     textOutput("foot_notes_3")
              ),
              tabItem(tabName = "description2",
                      fluidRow(
                        column(width = 6, align="center",
                               div(tableOutput("date_table2"), style = "font-size:140%")
                        ),
                        column(width = 6, align="center",
                               plotlyOutput("comparison_plot2",height="2000px")
                        )
                      )
                      
              ) # close tabItem
      ),
      tabItem("countywise_map",
               column(width = 4,align="center",
                      sliderInput("choose_date4", "Select the date",
                                  min = as.Date('2020-03-22', timeFormat="%Y-%m-%d"),
                                  max = today("EST") - 3,
                                  value = today("EST") - 3
                      )
               ),
               column(width = 3,align="center",
                      selectInput("choose_state4", "Choose a state:",
                                  statecode[,1]
                      )
               ),
               column(width = 5,align="center",
                      selectInput("comparison_metric4", "Select comparison:",
                                  c("Confirmed","Deaths")
                      ),
                      textOutput("foot_notes_4")
               ),
               column(width = 12, offset = -1, align="center",
                     plotlyOutput("graph_county",width="60%",height="1200px")
               )

      )
    )
  )
)






