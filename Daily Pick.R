#!/usr/bin/Rscript
# Daily Pick
##############

#
# Standalone script intended to be run by Cron job to report daily picks.
#

delta=30
theDate=as.Date(Sys.time())


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
  
  #If we have enough data
  if(nrow(data)>delta)
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
picks<-head(picks,5)


slackr_setup()
slackrBot("Making daily picks from highest median gain in previous 30 days:")
slackrBot(print(head(res)))

messageLinks=""
for(pick in picks)
{
  messageLinks<-paste(messageLinks,"\nhttp://www.iii.co.uk/research/",pick,sep="")
}

messageLinks<-gsub(messageLinks,pattern = "LON",replacement = "LSE")
slackrMsg(txt=messageLinks)

#Blart Everything to Slack
barChart(LoadedSymbols[[picks[1]]],name=picks[1],TA='addRSI();addVo()')
dev_slackr()
zoomChart(paste(as.Date(Sys.time())-30,"::",sep=""))
dev_slackr()
barChart(LoadedSymbols[[picks[2]]],name=picks[2],TA='addRSI();addVo()')
dev_slackr()
zoomChart(paste(as.Date(Sys.time())-30,"::",sep=""))
dev_slackr()
barChart(LoadedSymbols[[picks[3]]],name=picks[3],TA='addRSI();addVo()')
dev_slackr()
zoomChart(paste(as.Date(Sys.time())-30,"::",sep=""))
dev_slackr()
barChart(LoadedSymbols[[picks[4]]],name=picks[4],TA='addRSI();addVo()')
dev_slackr()
zoomChart(paste(as.Date(Sys.time())-30,"::",sep=""))
dev_slackr()
barChart(LoadedSymbols[[picks[5]]],name=picks[5],TA='addRSI();addVo()')
dev_slackr()
zoomChart(paste(as.Date(Sys.time())-30,"::",sep=""))
dev_slackr()