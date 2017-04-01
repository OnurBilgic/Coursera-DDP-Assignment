#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(caret)
library(dplyr)
library(stringr)
## Only run examples in interactive R sessions
  setwd("C:/Onur_Data/Folder/DATA SCIENCE/Developing_Data_Products-master/Shiny Application and Reproducible Pitch/models")    
  alldata<-read.csv("allbet.csv",header = TRUE,stringsAsFactors=FALSE)
  league<-read.csv("league.csv",header = TRUE,stringsAsFactors=FALSE)
  HomeTeam<-read.csv("HomeTeam.csv",header = TRUE,stringsAsFactors=FALSE)
  AwayTeam<-read.csv("AwayTeam.csv",header = TRUE,stringsAsFactors=FALSE)
  matches<-read.csv("matches.csv",header = TRUE,stringsAsFactors=FALSE)
  load("modelfits.rda")
  load("modelfitz.rda") 
  
  Home<-as.list(alldata[which(alldata$Div=="B1"),"HomeTeam"])
  Away<-as.list(alldata[which(alldata$Div=="B1"),"AwayTeam"])


shinyUI(fluidPage(
  titlePanel("Major European Soccer Leagues 20 Years Results"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("range", "1993-2017",min= 1993,max =  2017,value = c(1994,2005)),    
      selectInput("League", "League:",
                  list(
                    "Belgium 1" = "B1","England 1" = "E0","England 2" = "E1", 
                    "Scotland 1" = "SC0","Scotland 2" = "SC1", "Bundesliga 1"= "D1",  
                    "Bundesliga 2"= "D2","Italy 1"= "I1","Italy 2"= "I2","Spain 1"="SP1",
                    "Spain 2"="SP2","France 1"="F1","France 2"="F2","Netherlands"="N1",
                    "Portugal"="P1","Greece"="G1")),
      selectInput("HomeTeam", "HomeTeam:",Home),
      selectInput("AwayTeam", "AwayTeam:",Away),
      numericInput("point", "Input Year",2017,step = 1),
      numericInput("homeodds","Input Home Odds",1.5,step = 0.1), 
      numericInput("awayodds","Input Away Odds",2.5,step = 0.1), 
      numericInput("drawodds","Input Draw Odds",2,step = 0.1), 
      numericInput("2.5over","Input 2.5 Over Odds",2,step = 0.1), 
      numericInput("2.5under","Input 2.5 Under Odds",1.5,step = 0.1), 
      numericInput("halfhome","Input Half Goals Home Team",1,step = 1), 
      numericInput("awayhome","Input Half Goals Away Team",0,step = 1) 
    ),
    mainPanel(
      h4(textOutput("text2")),
      h3(textOutput("text1")),
      tabsetPanel(
        tabPanel('Matches',
                 dataTableOutput("mytable1")),
        tabPanel('Additional Info',
                 dataTableOutput("mytable2"))
      ),
      
      plotOutput("plot1"),
      plotOutput("plot2")
      
    )))
  )
