#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observe({
    x <- input$League
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    Home<-as.list(alldata[which(alldata$Div==x),"HomeTeam"])
    # Can also set the label and select items
    updateSelectInput(session, "HomeTeam",
                      choices = Home
    )
    Away<-as.list(alldata[which(alldata$Div==x),"HomeTeam"])
    # Can also set the label and select items
    updateSelectInput(session, "AwayTeam",
                      choices = Away
    )
  })
  output$mytable1 = renderDataTable({
    library(ggplot2)
    outputdata<-read.csv("outputdata.csv",header = TRUE)
    outputdata<-outputdata[which(outputdata$Div==input$League),]
    C<-c(input$range[1]:input$range[2])
    outputdata<-outputdata[which(outputdata$Date %in% C),]
    outputdata<-outputdata[which(outputdata$HomeTeam==input$`HomeTeam`),]
    outputdata<-outputdata[which(outputdata$AwayTeam==input$`AwayTeam`),]
    t1<-c("Div","Date","HomeTeam","AwayTeam","Full.Time.All.Goals", "Full.Time.Home.Goals","Full.Time.Away.Goals")
    outputdata<-outputdata[,t1]
    mytable1<-outputdata[order(outputdata$Date,decreasing = TRUE),]
  })
  output$mytable2 = renderDataTable({
    library(ggplot2)
    outputdata<-read.csv("outputdata.csv",header = TRUE)
    outputdata<-outputdata[which(outputdata$Div==input$League),]
    C<-c(input$range[1]:input$range[2])
    outputdata<-outputdata[which(outputdata$Date %in% C),]
    outputdata<-outputdata[which(outputdata$HomeTeam==input$`HomeTeam`),]
    outputdata<-outputdata[which(outputdata$AwayTeam==input$`AwayTeam`),]
    t2<-c("Full.Time.Results","Half.Time.Home.Goals","Half.Time.Away.Goals","Half.Time.Result","Half.Time.All.Goals","Home.Odds","Away.Odds","Draw.Odds","X2.5.over","X2.5.under")
    mytable2<-outputdata[,t2]
  })
  
  
  
  
  
  output$plot1 <- renderPlot({
    dataplot<-alldata[which(alldata$matches==paste(input$`HomeTeam`,"vs",input$`AwayTeam`)),]
    dataplot<-dataplot[which(dataplot$Div==input$League),]
    C<-c(input$range[1]:input$range[2])
    dataplot<-dataplot[which(dataplot$Date %in% C),]
    dataplot[which(dataplot$FTR=="D"),"FTR"]<-"Draw"
    dataplot[which(dataplot$FTR=="H"),"FTR"]<-input$`HomeTeam`
    dataplot[which(dataplot$FTR=="A"),"FTR"]<-input$`AwayTeam`
    barplot(height=table(dataplot$FTR),col = "grey",xlab = "Results",main = "Results Bar")
  })
  output$plot2 <- renderPlot({
    dataplot<-alldata[which(alldata$matches==paste(input$`HomeTeam`,"vs",input$`AwayTeam`)),]
    dataplot<-dataplot[which(dataplot$Div==input$League),]
    C<-c(input$range[1]:input$range[2])
    dataplot<-dataplot[which(dataplot$Date %in% C),]
    barplot(height=table(dataplot$AG),col = "red",xlab = "Goals",main = "Goals Bar")
    
  })
  output$text1 <- renderText({ 
    testdata<-alldata[0,]
    testdata[1,"Div"]<-input$League
    testdata[1,"Date"]<-input$point
    testdata[1,"HomeTeam"]<-input$HomeTeam
    testdata[1,"AwayTeam"]<-input$AwayTeam
    testdata[1,"meanh"]<-input$homeodds
    testdata[1,"meana"]<-input$awayodds
    testdata[1,"meand"]<-input$drawodds
    testdata[1,"meanover"]<-input$`2.5over`
    testdata[1,"meanunder"]<-input$`2.5under`
    testdata[1,"HTHG"]<-input$`halfhome`
    testdata[1,"HTAG"]<-input$`awayhome`
    testdata[1,"Divscore"]<-league[match(testdata$Div,league$league),2]
    testdata[1,"HomeTeamscore"]<-HomeTeam[match(testdata$HomeTeam,HomeTeam$HomeTeam),2]
    testdata[1,"AwayTeamscore"]<-AwayTeam[match(testdata$AwayTeam,AwayTeam$AwayTeam),2]
    testdata[1, "matches"]<-paste(input$HomeTeam,"vs",input$AwayTeam)
    testdata[1,"matchesscore"]<-matches[match(testdata$matches,matches$match),2]
    preds<-predict(modelfits,testdata)
    preds<-round(preds)
    predz<-predict(modelfitz,testdata)
    if(predz=="H"){A=input$HomeTeam}
    if(predz=="A"){A=input$AwayTeam}
    if(predz=="D"){A="Draw"}
    if(A=="Draw"){paste("Predicted Results is draw with %56 accuracy and predicted full time all goal is",preds,"with a 33% accuracy")}
    else
      paste("Predicted Results is",A,"winner with %56 accuracy and predicted full time all goal is",preds,"with a 33% accuracy")
  })
  output$text2 <- renderText({ 
    paste(" This application is made for only Coursera Devoloping Data Products Mooc. Please consider it a only exprimental project.Nothing is serious.
            The aim of this application is help to bet lovers after half time of the match to predict who is the winner and all goals.One of my 
            collegue plays this game like an addiction and I coded this app also for him.Please use side bar and select league and teams to bring 
            table and graphics. After that to refill related information (Date/Odds/Halftime Goals) to help the app for estimating. Decision tree is 
            used for results modelling. Moreoever goals prediction is made by lineer regression.")
    
  })
})
  
