library(caret)
library(rpart)
library(randomforest)
setwd("C:/Onur_Data/Folder/DATA SCIENCE/Developing_Data_Products-master/Shiny Application and Reproducible Pitch/bet")
list<-list.files()
library(dplyr)
library(stringr)
library(xlsx)
if("files.csv" %in% list ){
file<-read.csv("files.csv",header = TRUE,stringsAsFactors=FALSE)
} else{
file<-data.frame(Div=character())
file2<-data.frame(Div=character()) 
for(i in 1:length(list)){
  file2<-read.csv(list[i],header = TRUE)
  file<-bind_rows(file,file2)
  }
setwd("C:/Onur_Data/Folder/DATA SCIENCE/Developing_Data_Products-master/Shiny Application and Reproducible Pitch/models")
write.csv(file,file="files.csv")
}
setwd("C:/Onur_Data/Folder/DATA SCIENCE/Developing_Data_Products-master/Shiny Application and Reproducible Pitch/models")

#'Div = League Division	Date = Match Date (dd/mm/yy)	HomeTeam = Home Team	AwayTeam = Away Team	FTHG = Full Time Home Team Goals	FTAG = Full Time Away Team Goals	FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)	HTHG = Half Time Home Team Goals	HTAG = Half Time Away Team Goals	HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)
file<-file[,-1]
file<-file[!is.na(file$FTHG),]
file$Date<-as.character(file$Date)
file$Date<-as.Date(file$Date, "%d/%m/%y")
file$Date<-as.numeric(format(file$Date,'%Y'))
file[is.na(file$HomeTeam),"HomeTeam"]<-as.factor("Unknown")
file[is.na(file$AwayTeam),"AwayTeam"]<-as.factor("Unknown")
file[is.na(file$HTHG),"HTHG"]<-mean(file$HTHG,na.rm = TRUE)
file[which(file$HTHG>file$FTHG),"HTHG"]<-file$FTHG
file[is.na(file$HTAG),"HTAG"]<-mean(file$HTAG,na.rm = TRUE)
file[which(file$HTAG>file$FTAG),"HTHG"]<-file$FTAG
file$HAG<-file$HTHG+file$HTAG
file$AG<-file$FTHG+file$FTAG
file[is.na(file$HTR),"HTR"]<-as.factor("Unknown")
data1<-c(1:10,133,134)
file1<-file[,data1]

#'homeodds
h<-c(11,14,17,20,23,26,29,53,56,59,89,92,112)
fileh<-file[,h]
meanh<-round(rowMeans(fileh,na.rm = TRUE),2)
meanh[is.na(meanh)]<-median(meanh,na.rm = TRUE)
#'drawodds
d<-c(12,15,18,21,24,27,30,54,57,60,90,93,113)
filed<-file[,d]
meand<-rowMeans(filed,na.rm = TRUE)
meand[is.na(meand)]<-median(meand,na.rm = TRUE)
#'awayodds
a<-c(13,16,19,22,25,28,31,55,58,61,91,94,114)
filea<-file[,a]
meana<-rowMeans(filea,na.rm = TRUE)
meana[is.na(meana)]<-median(meana,na.rm = TRUE)
#' 2.5 overodds 
over<-c(40,41,62,107)
file2.5over<-file[,over]
meanover<-rowMeans(file2.5over,na.rm = TRUE)
meanover[is.na(meanover)]<-median(meanover,na.rm = TRUE)


#' 2.5 underodds
under<-c(42,43,63,108)
file2.5under<-file[,under]
meanunder<-rowMeans(file2.5under,na.rm = TRUE)
meanunder[is.na(meanunder)]<-median(meanunder,na.rm = TRUE)

#' understand the NAs

c<-data.frame(V1=character(), 
              V2=character(),
              stringsAsFactors=FALSE)
for(i in 1:length(names(file))){
  c[i,2]<-sum(is.na(file[,i]))}

c$V1<-names(file)
names(c)<-c("feature","NA-Counts")
#'data partition training and testing
alldata<-cbind(file1,meanh,meana,meand,meanover,meanunder)

# write.csv(alldata,file="allbet.csv",row.names = FALSE)
alldata<-read.csv("allbet.csv",header = TRUE,stringsAsFactors=FALSE)

alldata<-alldata[,-1]
#' creating new variables
#' league
league<-aggregate(alldata$AG~alldata$Div, FUN=mean)
names(league)<-c("league","score")
write.csv(league,file="league.csv",row.names = FALSE)


alldata$Divscore<-league[match(alldata$Div, league$league),2]

# HomeTeam
HomeTeam<-aggregate(alldata[,"Full.Time.Home.Goals"]~alldata$HomeTeam, FUN=mean)
names(HomeTeam)<-c("HomeTeam","score")
write.csv(HomeTeam,file="HomeTeam.csv",row.names = FALSE)

alldata$HomeTeamscore<-HomeTeam[match(alldata$HomeTeam, HomeTeam$HomeTeam),2]


# HomeTeam Score Matches
HomeTeam2<-aggregate(alldata$FTRScore~alldata$HomeTeam, FUN=mean)
names(HomeTeam2)<-c("HomeTeam","score")

alldata$HomeTeamscore2<-HomeTeam2[match(alldata$HomeTeam, HomeTeam2$HomeTeam),2]



# AwayTeam
AwayTeam<-aggregate(alldata[,"Full.Time.Away.Goals"]~alldata$AwayTeam, FUN=mean)
names(AwayTeam)<-c("AwayTeam","score")

write.csv(AwayTeam,file="AwayTeam.csv",row.names = FALSE)

alldata$AwayTeamscore<-AwayTeam[match(alldata$AwayTeam, AwayTeam$AwayTeam),2]

# AwayTeam Score Matches
AwayTeam2<-aggregate(alldata$FTRScore~alldata$AwayTeam, FUN=mean)
names(AwayTeam2)<-c("AwayTeam2","score")

alldata$AwayTeamscore2<-AwayTeam2[match(alldata$AwayTeam, AwayTeam2$AwayTeam),2]


# results
alldata[which(alldata$FTR=="H"),"FTRScore"]<-1
alldata[which(alldata$FTR=="A"),"FTRScore"]<--1
alldata[which(alldata$FTR=="D"),"FTRScore"]<-0



alldata$AwayTeamscore<-AwayTeam[match(alldata$AwayTeam, AwayTeam$AwayTeam),2]


# halftime results
alldata[which(alldata$HTR=="H"),"HTRScore"]<-1
alldata[which(alldata$HTR=="A"),"HTRScore"]<--1
alldata[which(alldata$HTR=="D"),"HTRScore"]<-0
# matches 

alldata$matches<-paste(alldata$HomeTeam,alldata$AwayTeam,sep = " vs ")
matches<-aggregate(alldata$AG~alldata$matches, FUN=mean)
write.csv(matches,file="matches.csv",row.names = FALSE)


names(matches)<-c("matches","score")
alldata$matchesscore<-matches[match(alldata$matches, matches$matches),2]

# matches score2 

matches2<-aggregate(alldata$FTRScore~alldata$matches, FUN=mean)
names(matches2)<-c("matches","score")

alldata$matchesscore2<-matches2[match(alldata$matches, matches2$matches),2]
# write.csv(alldata,file="allbet.csv",row.names = FALSE)

colnames(alldata)[colnames(alldata)=="FTHG"] <- "Full Time Home Goals"
colnames(alldata)[colnames(alldata)=="FTAG"] <- "Full Time Away Goals"
colnames(alldata)[colnames(alldata)=="FTR"] <- "Full Time Results"
colnames(alldata)[colnames(alldata)=="HTHG"] <- "Half Time Home Goals"
colnames(alldata)[colnames(alldata)=="HTAG"] <- "Half Time Away Goals"
colnames(alldata)[colnames(alldata)=="HTR"] <- "Half Time Result"
colnames(alldata)[colnames(alldata)=="HAG"] <- "Half Time All Goals"
colnames(alldata)[colnames(alldata)=="AG"] <- "Full Time All Goals"
colnames(alldata)[colnames(alldata)=="meanh"] <- "Home Odds"
colnames(alldata)[colnames(alldata)=="meana"] <- "Away Odds"
colnames(alldata)[colnames(alldata)=="meand"] <- "Draw Odds"
colnames(alldata)[colnames(alldata)=="meanover"] <- "2.5 over"
colnames(alldata)[colnames(alldata)=="meanunder"] <- "2.5 under"

c<-c("Div","Date","HomeTeam","AwayTeam","Full Time Home Goals","Full Time Away Goals","Full Time Results","Half Time Home Goals","Full Time Away Goals","Full Time Away Goals","Full Time Away Goals","Full Time Away Goals","Home Odds","Away Odds","Draw Odds","2.5 over","2.5 under")
outputdata<-alldata[,names(alldata)[1:17]]
write.csv(outputdata,file="outputdata.csv",row.names = FALSE)
outputdata<-read.csv("outputdata.csv",header = TRUE)




inTrain<-createDataPartition(y=alldata$FTR,p=.70,list = FALSE)
training<- alldata[inTrain,]
testing<- alldata[-inTrain,]
dim(training)  
dim(testing)
#' decision tree

set.seed(1001)
modelfit0<-train(AG~.,method="rpart",data=training)
pred0<-predict(modelfit0,testing)
confusionMatrix(pred0,testing$AG)

#' random forest
set.seed(1002)
modelfit3<-train(AG~Div+Date+FTHG+FTAG+FTR+HTHG+HTAG+HAG+meanh+meana+meand+meanover+meanunder,data = training,method="rf",ntree=20)
save(modelfit3, file = "modelfit3.rda")
#' load("modelfit1.rda")
pred3<-predict(modelfit3,testing)
pred3<-round(pred3)
confusionMatrix(pred3,testing$AG)
#' save(modelfit1, file = "modelfit1.rda")
#' load("modelfit1.rda")

#' lineer regression
set.seed(1003)
modelfit2<-train(AG~Date+FTHG+FTAG+HTHG+HTAG+HAG+meanh+meana+meand+meanover+meanunder,data = training,method="lm")
pred2<-predict(modelfit2,testing)
pred2<-round(pred2)
confusionMatrix(pred2,testing$AG)
#' save(modelfit2, file = "modelfit2.rda")
#' load("modelfit2.rda")
 
pca<-princomp(training[,c(2,5,6,8,9,11,12,13,14,15,16,17)],cor = TRUE,scores = TRUE)
plot(pca)
biplot(pca)

?write.xlsx()
 write.xlsx(testing,"testing.xlsx",sheetName = 1)
 write.xlsx2(testing[1:10,], "testing.xlsx", sheetName="Sheet1",
             col.names=TRUE)
 
 #' lineer regression
 set.seed(1003)
 modelfit4<-train(AG~Date+HTHG+HTAG+HAG+meanh+meana+meand+meanover+meanunder,data = training,method="lm")
 pred4<-predict(modelfit4,testing)
 pred4<-round(pred4)
 confusionMatrix(pred4,testing$AG)
 #' save(modelfit2, file = "modelfit2.rda")
 #' load("modelfit2.rda")
 
 
 
 
 
 #' random forest
 set.seed(1002)
 modelfit5<-train(AG~Div+Date+HTHG+HTAG+HAG+meanh+meana+meand+meanover+meanunder,data = training,method="rf",ntree=20)
 save(modelfit5, file = "modelfit5.rda")
 #' load("modelfit5.rda")
 pred5<-predict(modelfit5,testing)
 pred5<-round(pred5)
 confusionMatrix(pred5,testing$AG)
 
 
 #' lineer regression
 set.seed(1006)
 D<-"Date+meanh+meana+meand+meanover+meanunder+Divscore+HomeTeamscor+AwayTeamscore+matchesscore"
 modelfit6<-train(AG~matchesscore+matchesscore2,data = training,method="lm")
 pred6<-predict(modelfit6,testing)
 pred6<-round(pred6)
 confusionMatrix(pred6,testing$AG)
 #' save(modelfit2, file = "modelfit2.rda")
 #' load("modelfit2.rda")
 #' 
 
 
 dataplot<-alldata[which(alldata$matches==paste("St Truiden","vs","Club Brugge")),]
 dataplot[which(dataplot$FTR=="D"),"FTRN"]<-0
 dataplot[which(dataplot$FTR=="H"),"FTRN"]<-1
 dataplot[which(dataplot$FTR=="A"),"FTRN"]<--1
 hist(dataplot$FTRN)
 hist(dataplot$FTRN,col = "grey",xlab = "Results",breaks = "Sturges",main = "Results Histogram")
 #' Away<-as.list(alldata[which(alldata$Div=="B1"),"AwayTeam"])
 #' Home<-as.list(alldata[which(alldata$Div=="B1"),"HomeTeam"])

###################################################################################
 
 ###### model 
 
 # League,HomeTeam,AwayTeam,Year,Home Odds,Away Odds,Draw Odds,2.5 Over Odds,2.5 Under Odds,Input Half Goals Home Team,Input Half Goals Away Team

 set.seed(1006)

 modelfits<-train(AG~Date+meanh+meana+meand+meanover+meanunder+HTHG+HTAG+Divscore+HomeTeamscore+AwayTeamscore+matchesscore,data = training,method="lm")
 preds<-predict(modelfits,testing)
 preds<-round(preds)
 confusionMatrix(preds,testing$AG)
 #' save(modelfits, file = "modelfits.rda")
 #' load("modelfits.rda")

 modelfitz<-train(FTR~Date+meanh+meana+meand+meanover+meanunder+HTHG+HTAG+Divscore+HomeTeamscore+AwayTeamscore+matchesscore,data = training,method="rpart")
 preds<-predict(modelfitz,testing)
 preds<-round(preds)
 confusionMatrix(preds,testing$FTR)
 #' save(modelfits, file = "modelfits.rda")
 #' load("modelfits.rda") 
 #' save(modelfitz, file = "modelfitz.rda")
 #' load("modelfitz.rda") 
 
alldata[which(alldata$FTR=="D"),"FTRN"]<-0
alldata[which(alldata$FTR=="H"),"FTRN"]<-1
alldata[which(alldata$FTR=="A"),"FTRN"]<--1
 
 
 testdata<-alldata[0,]
 testdata[1,"Div"]<-"B1"
 testdata[1,"Date"]<-alldata[1,"Date"]
 testdata[1,"HomeTeam"]<-"St Truiden"
 testdata[1,"AwayTeam"]<-"Club Brugge"
 testdata[1,"meanh"]<-2.5
 testdata[1,"meana"]<-1.2
 testdata[1,"meand"]<-2.3
 testdata[1,"meanover"]<-1.2
 testdata[1,"meanunder"]<-1.4
 testdata[1,"meanunder"]<-1.3
 testdata[1,"HTHG"]<-2
 testdata[1,"HTAG"]<-3
 testdata[1,"Divscore"]<-2.3
 testdata[1,"HomeTeamscore"]<-2.4
 testdata[1,"AwayTeamscore"]<-1.2
 testdata[1, "matches"]<-"St Truiden vs Club Brugge"
 testdata[1,"matchesscore"]<-1.2
 testdata[1,"Divscore"]<-league[match(testdata$Div,league$league),2]
 testdata[1,"HomeTeamscore"]<-HomeTeam[match(testdata$HomeTeam,HomeTeam$HomeTeam),2]
 testdata[1,"AwayTeamscore"]<-AwayTeam[match(testdata$AwayTeam,AwayTeam$AwayTeam),2]
 testdata[1, "matches"]<-paste(testdata[1,"HomeTeam"],"vs",testdata[1,"AwayTeam"])
 testdata[1,"matchesscore"]<-matches[match(testdata$matches,matches$match),2]
 
 preds<-predict(modelfits,testdata)
 preds<-round(preds)
 