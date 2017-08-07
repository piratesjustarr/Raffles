#!/usr/bin/Rscript
# Daily Pick
##############

#
# Backtest of standalone script intended to be run by Cron job to report daily picks.
#

delta=30
theDate=as.Date(Sys.time())-10


#Share Select
setwd("/home/raffles/Raffles/")
source("Quantlib.R")
setwd("./Data/")
loadLocalData()

#Loads basic libraries and sets up required environments for Quantstrat
loadLibraries<-function()
{
  require(slackr)
  require(quantmod)
  require(quantstrat)
  require(readr)
  require(chron)
}



loadLibraries()
library(chron)

#Make a list
shift<-list()

#Loop through known symbols
for (symbol in ls(LoadedSymbols))
{
  data<-Cl(LoadedSymbols[[symbol]])
  #Only use data we have access to.
  data<-data[paste("::",theDate,sep="")]
  
  #which is recent
  recentdata=index(last(data))>=as.Date(theDate)-2
  
  #If we have enough recent data
  if(nrow(data)>delta && recentdata)
  {
    #Get ROC vs delta periods ago
    #val=median(ROC(data,n = delta),na.rm = TRUE)
    val=median(ROC(data,n = delta),na.rm = TRUE)
  }
  else
  {
    #Else shove it to bottom of the pile
    val=0
  }
  shift[symbol]<-val
}


#Transpose and sort
res<-(t(as.data.frame(shift)))
res<-res[order(res,decreasing = TRUE),]
picks<-gsub(names(res),pattern = "\\.",replacement = ":")
picks<-head(picks,50)

#Sum of return over last 10 days
dayreturns<-list()
for(pick in picks)
{
  data<-LoadedSymbols[[pick]]
  #Only use data we have access to.
  data<-data[paste("::",theDate,sep="")]
  
  recent<<-last(data,"10 days")
  recent_cl<-(Cl(recent))
  barChart(recent_cl,name = pick)
  #return<-as.numeric(recent_cl[10])/as.numeric(recent_cl[1])*100
  return<-sum(ROC(recent_cl),na.rm = TRUE)
  print(pick)
  print(return)
  dayreturns[[pick]]<-return
}
picks<-t(as.data.frame(dayreturns[order(t(as.data.frame(dayreturns)),decreasing = TRUE)]))
write.csv(picks,"picks.csv")
picks<-head(picks,5)

slackr_setup()
slackrBot("BACKTEST from 10 days ago: Making daily picks from highest median gain in previous 30 days, top 50 sorted by sum of ROC in last 10 days:")
slackrBot(print(picks))

#Get names not returns
picks<-rownames(picks)
picks<-gsub(picks,pattern = "\\.",replacement = ":")

message=""
for(pick in picks)
{
  data<-LoadedSymbols[[pick]]
  print(last(data,"14 days"))
  data<-Cl(data)
  current<-last(data)
  #Only use data we have access to.
  data<-data[paste("::",theDate,sep="")]
  btprice<-last(data)
  returnprc<-(as.numeric(current)/as.numeric(btprice))*100
  message<-paste(message,pick,"price at backtest was",btprice,"now currently",current,"which is ",returnprc," percent \n")
}
slackrMsg(txt=message)

messageLinks=""
for(pick in picks)
{
  messageLinks<-paste(messageLinks,"\nhttp://www.iii.co.uk/research/",pick,sep="")
}

messageLinks<-gsub(messageLinks,pattern = "LON",replacement = "LSE")
slackrMsg(txt=messageLinks)

#Blart Everything to Slack
for(i in 1:5)
{
  jpeg("Plot.jpeg")
  barChart(LoadedSymbols[[picks[i]]],name=picks[i],TA='addRSI();addVo()')
  dev.off()
  slackrUpload(filename = "Plot.jpeg", title = picks[i], channels = "raffles")
}
dev.new()
slackrUpload(filename = "picks.csv", title = "all 
picks", channels = "raffles")

