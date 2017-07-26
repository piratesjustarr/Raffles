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
  
  #which is recent
  recentdata=index(last(data))>=as.Date(Sys.time())-2
  
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
write.csv(picks,"picks.csv")
picks<-head(picks,50)

dayreturns<-list()
for(pick in picks)
{
  recent<<-last(LoadedSymbols[[pick]],"15 days")
  recent_cl<-(Cl(recent))
  return<-as.numeric(recent_cl[10])/as.numeric(recent_cl[1])*100
  print(pick)
  print(return)
  dayreturns[[pick]]<-return
}
picks<-t(as.data.frame(dayreturns[order(t(as.data.frame(dayreturns)),decreasing = TRUE)]))
picks<-head(picks,5)

slackr_setup()
slackrBot("Making daily picks from highest median gain in previous 30 days, top 50 sorted by return in last 15 days:")
slackrBot(print(picks))

#Get names not returns
picks<-rownames(picks)

messageLinks=""
for(pick in picks)
{
  messageLinks<-paste(messageLinks,"\nhttp://www.iii.co.uk/research/",pick,sep="")
}

messageLinks<-gsub(messageLinks,pattern = "LON",replacement = "LSE")
slackrMsg(txt=messageLinks)

#Blart Everything to Slack
picks<-gsub(picks,pattern = "\\.",replacement = ":")
for(i in 1:5)
{
jpeg("Plot.jpeg")
barChart(LoadedSymbols[[picks[i]]],name=picks[i],TA='addRSI();addVo()')
dev.off()
slackrUpload(filename = "Plot.jpeg", title = picks[i], channels = "raffles")
}


slackrUpload(filename = "picks.csv", title = "all 
picks", channels = "raffles")

