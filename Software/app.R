#rm(list = ls())
'pkgs <- c("dplyr", "shiny", "bcp", "plotly")
sapply(pkgs, function(x){
  if (!require(x, character.only = T))
    install.packages(x)
  require(x, character.only = T)
})'
#path = 'C:\\Users\\user\\Downloads\\Shiny\\Shiny'
#path <- 'C:\\Users\\rd2nr\\Documents\\Gun\\shiny_new'
# path <- '~/Dropbox/Gupt_Rog/Software/5fe5935aa8664496a3848f9715c96397/'
path <- '~/Dropbox/Gupt_Rog/Software/Final_Interface_software/'
# path <- "C:/Users/user/Desktop/WORK2020/200413_COVIDproj/5fe5935aa8664496a3848f9715c96397"
# setwd(path)
#runApp(appDir = paste0(path))
library('shiny')
library('devtools')
library('bcp')
library('plotly')
library('dplyr')
library(wesanderson)
library("viridis")
library(usmap)
#library('ggplot2')
#library('maps')
runApp()

library(rsconnect)
rsconnect::setAccountInfo(name='sounakchakraborty',
                          token='E81AEBBE62DF028F414A22610D0C571E',
                          secret='Pcc9KGWrK0Y8oKlQMs7XzHXfys82EDyDr2EB1gXI')
rsconnect::deployApp(here::here(),
                     account = 'sounakchakraborty', launch.browser=TRUE)
# rsconnect::terminateApp("final_interface_software")
# rsconnect::terminateApp("final_interface_software2")