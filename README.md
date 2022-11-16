# Group-project
project code and data sets
library(dplyr)
library(quantmod)
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(data.table)
library(tidyquant)
library(gridExtra)
library(rugarch)
library(kableExtra)
library(tidyr)
library(goftest)
library(vars)
library(knitr)
library(FinTS)
library(aTSA)
library(timeSeries)
library(fGarch)
library(fBasics)
library(caTools)
library(PerformanceAnalytics)
library(zoo)
library(timetk)
library(sjPlot)

#Setting the working directory

setwd("C:/Users/user/Pictures/BlueStacks/New folder")

#Data import

AEX<- read_csv("AEX.csv")
AEX['Date'] <- as.Date(AEX$Date)

CWEB <- read_csv("CWEB.csv")
CWEB['Date'] <- as.Date(CWEB$Date)

EWD <- read_csv("EWD.csv")
EWD['Date'] <- as.Date(EWD$Date)

FSZ <- read_csv("FSZ.csv")
FSZ['Date'] <- as.Date(FSZ$Date)

IXIC <- read_csv("IXIC.csv")
IXIC['Date'] <- as.Date(IXIC$Date)

VGK <- read_csv("VGK.csv")
VGK['Date'] <- as.Date(VGK$Date)

#selecting the 'close' and date columns of each data set

AEXprice<-AEX$Close
AEXdate<-AEX$Date
AEX.TS<- xts(AEXprice,order.by=AEXdate)

CWEBprice<-CWEB$Close
CWEBdate<-CWEB$Date
CWEB.TS<- xts(CWEBprice,order.by=CWEBdate)

EWDprice<-EWD$Close
EWDdate<-EWD$Date
EWD.TS<- xts(EWDprice,order.by=EWDdate)

VGKprice<-VGK$Close
VGKdate<-VGK$Date
VGK.TS<- xts(VGKprice,order.by=VGKdate)

IXICprice<-IXIC$Close
IXICdate<-IXIC$Date
IXIC.TS<- xts(IXICprice,order.by=IXICdate)

FSZprice<-FSZ$Close
FSZdate<-FSZ$Date
FSZ.TS<- xts(FSZprice,order.by=FSZdate)

#Plotting the average daily returns of each dataset

p1<-ggplot(AEX.TS, aes(x=AEXdate,y=AEXprice))+geom_line()+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Date",y="Price",title="Average Daily Price for AEX Stock Index")

p2<-ggplot(CWEB.TS, aes(x=CWEBdate,y=CWEBprice))+geom_line()+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Date",y="Price",title="Average Daily Price for CWEB Stock Index")

p3<-ggplot(CWEB.TS, aes(x=EWDdate,y=CWEBprice))+geom_line()+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Date",y="Price",title="Average Daily Price for EWD Stock Index")

p4<-ggplot(CWEB.TS, aes(x=VGKdate,y=VGKprice))+geom_line()+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Date",y="Price",title="Average Daily Price for VGK Stock Index")

p5<-ggplot(IXIC.TS, aes(x=VGKdate,y=IXICprice))+geom_line()+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Date",y="Price",title="Average Daily Price for IXIC Stock Index")

p6<-ggplot(FSZ.TS, aes(x=FSZdate,y=FSZprice))+geom_line()+
  theme(plot.title=element_text(hjust=0.5))+
  labs(x="Date",y="Price",title="Average Daily Price for FSZ Stock Index")

grid.arrange(p1 , p2 ,p3, p4, p5, p6)

#Calculate daily returns

CWEB.rtn <- returns(CWEB.TS)
EWD.rtn <- returns(EWD.TS)
VGK.rtn <- returns(VGK.TS)
IXIC.rtn <- returns(IXIC.TS)
AEX.rtn <- returns(AEX.TS)
FSZ.rtn <- returns(FSZ.TS)

# calculating the log returns 

logCWEB.rtn<-diff(log(CWEBprice), lag=1)
(head(logCWEB.rtn))
logEWD.rtn<-diff(log(EWDprice), lag=1)
(head(logEWD.rtn))
logVGK.rtn<-diff(log(VGKprice), lag=1)
(head(logVGK.rtn))
logIXIC.rtn<-diff(log(IXICprice), lag=1)
(head(logIXIC.rtn))
logAEX.rtn<-diff(log(AEXprice), lag=1)
(head(logAEX.rtn))
logFSZ.rtn<-diff(log(FSZprice), lag=1)
(head(logFSZ.rtn))

logs_dataframe <- data.frame(logCWEB.rtn, logEWD.rtn, logVGK.rtn, logIXIC.rtn, logAEX.rtn, logFSZ.rtn)
basicStats(logs_dataframe)
table<- data.table(basicStats)
table
#Tests
#Is the mean log return significantly different from zero=T test
t.test(CWEB.rtn)
#Normality Tests
#JB Test
jb1 <- jarqueberaTest(logCWEB.rtn);jb1
jb2 <- jarqueberaTest(logEWD.rtn);jb2
jb3 <- jarqueberaTest(logVGK.rtn);jb3
jb4 <- jarqueberaTest(logIXIC.rtn);jb4
jb5 <- jarqueberaTest(logAEX.rtn);jb5
jb6 <- jarqueberaTest(logFSZ.rtn);jb6

#ADF test
adf.test(logCWEB.rtn)
adf.test(logEWD.rtn)
adf.test(logVGK.rtn)
adf.test(logIXIC.rtn)
adf.test(logAEX.rtn)
adf.test(logFSZ.rtn)


#Is the mean log return significantly different from zero=T test
t.test(EWD.rtn)
#Normality Tests
#JB Test
#kable.(jb1,jb2)
#histogram
h1 <- hist(logEWD.rtn, prob=TRUE, col = "grey")
lines(density(logEWD.rtn,adjust=8), col="blue", lwd=2)

#Is the mean log return significantly different from zero=T test
t.test(VGK.rtn)
#Normality Tests
#histogram
h2 <- hist(logVGK.rtn, prob=TRUE, col = "grey")
lines(density(logCWEB.rtn,adjust=8), col="blue", lwd=2)



par(mfrow= c(1,2))
h1 <-hist(logCWEB.rtn, prob=TRUE, col = "grey")
lines(density(logCWEB.rtn,adjust=8), col="blue", lwd=2)
h2 <- hist(logEWD.rtn, prob=TRUE, col = "grey")
lines(density(logEWD.rtn,adjust=8), col="blue", lwd=2)
h3 <- hist(logVGK.rtn, prob=TRUE, col = "grey")
lines(density(logCWEB.rtn,adjust=8), col="blue", lwd=2)
h4 <- hist(logIXIC.rtn, prob=TRUE, col = "grey")
lines(density(logIXIC.rtn,adjust=8), col="blue", lwd=2)
h5 <- hist(logAEX.rtn, prob=TRUE, col = "grey")
lines(density(logAEX.rtn,adjust=8), col="blue", lwd=2)
h6 <- hist(logFSZ.rtn, prob=TRUE, col = "grey")
lines(density(logFSZ.rtn,adjust=8), col="blue", lwd=2)

#Is the mean log return significantly different from zero=T test
t.test(IXIC.rtn)
#Normality Test
#Is the mean log return significantly different from zero=T test
t.test(AEX.rtn)
#Normality Test
#histogram
#Is the mean log return significantly different from zero=T test
t.test(FSZ.rtn)
#Normality Tests


#stationarity Test
p1<-ggplot(CWEB.TS[-1], aes(CWEBdate[-1],logCWEB.rtn))+geom_line()+
  labs(x="Date",y="Log Return",title = "CWEB Log Returns plot")

p2<-ggplot(EWD.TS[-1], aes(EWDdate[-1],logEWD.rtn))+geom_line()+
  labs(x="Date",y="Log Return",title = "EWD Log Returns plot")


p3<-ggplot(VGK.TS[-1], aes(VGKdate[-1],logVGK.rtn))+geom_line()+
  labs(x="Date",y="Log Return",title = "VGK Log Returns plot")


p4<-ggplot(IXIC.TS[-1], aes(IXICdate[-1],logIXIC.rtn))+geom_line()+
  labs(x="Date",y="Log Return",title = "IXIC Log Returns plot")

p5<-ggplot(AEX.TS[-1], aes(AEXdate[-1],logAEX.rtn))+geom_line()+
  labs(x="Date",y="Log Return",title = "AEX Log Returns plot")

p6<-ggplot(FSZ.TS[-1], aes(FSZdate[-1],logFSZ.rtn))+geom_line()+
  labs(x="Date",y="Log Return",title = "FSZ Log Returns plot")

grid.arrange(p1,p2,p3,p4, p5,p6)


#serial correlation in the log return series
Box.test(logCWEB.rtn,lag=20,type= "Ljung")
mod<-arima(logCWEB.rtn, order=c(0,0,0))
ArchTest(logCWEB.rtn, lag=20, demean = FALSE)

Box.test(logEWD.rtn,lag=20,type= "Ljung")
mod<-arima(logEWD.rtn, order=c(0,0,2))
ArchTest(logEWD.rtn, lag=20, demean = FALSE)

Box.test(logVGK.rtn,lag=20,type= "Ljung")
mod<-arima(logVGK.rtn, order=c(0,0,2))
ArchTest(logVGK.rtn, lag=20, demean = FALSE)

Box.test(logIXIC.rtn,lag=20,type= "Ljung")
mod<-arima(logIXIC.rtn, order=c(0,0,2))
ArchTest(logIXIC.rtn, lag=20, demean = FALSE)

Box.test(logAEX.rtn,lag=20,type= "Ljung")
mod<-arima(logAEX.rtn, order=c(0,0,2))
ArchTest(logAEX.rtn, lag=20, demean = FALSE)

Box.test(logFSZ.rtn,lag=20,type= "Ljung")
mod<-arima(logFSZ.rtn, order=c(0,0,1))
ArchTest(logFSZ.rtn, lag=20, demean = FALSE)


#Estimated Mean Equation
#AEX
AR1 <- Arima(logAEX.rtn,order=c(0,0,1))
AR1
AR2 <- Arima(logAEX.rtn,order=c(0,0,2))
AR2
AR3 <- Arima(logAEX.rtn,order=c(1,0,0))
AR3
AR4 <- Arima(logAEX.rtn,order=c(1,1,0))
AR4
AR5 <- Arima(logAEX.rtn,order=c(1,2,0))
AR5
AR6 <- Arima(logAEX.rtn,order=c(2,0,0))
AR6
AR7 <- Arima(logAEX.rtn,order=c(2,1,0))
AR7
AR8 <- Arima(logAEX.rtn,order=c(2,2,0))
AR8

#CWEB data


AR1 <- Arima(logCWEB.rtn,order=c(0,0,1))
AR1
AR2 <- Arima(logCWEB.rtn,order=c(0,0,2))
AR2
AR3 <- Arima(logCWEB.rtn,order=c(1,0,0))
AR3
AR4 <- Arima(logCWEB.rtn,order=c(1,1,0))
AR4
AR5 <- Arima(logCWEB.rtn,order=c(1,2,0))
AR5
AR6 <- Arima(logCWEB.rtn,order=c(2,0,0))
AR6
AR7 <- Arima(logCWEB.rtn,order=c(2,1,0))
AR7
AR8 <- Arima(logCWEB.rtn,order=c(2,2,0))
AR8

#EWD data
AR1 <- Arima(logEWD.rtn,order=c(0,0,1))
AR1
AR2 <- Arima(logEWD.rtn,order=c(0,0,2))
AR2
AR3 <- Arima(logEWD.rtn,order=c(1,0,0))
AR3
AR4 <- Arima(logEWD.rtn,order=c(1,1,0))
AR4
AR5 <- Arima(logEWD.rtn,order=c(1,2,0))
AR5
AR6 <- Arima(logEWD.rtn,order=c(2,0,0))
AR6
AR7 <- Arima(logEWD.rtn,order=c(2,1,0))
AR7
AR8 <- Arima(logEWD.rtn,order=c(2,2,0))
AR8

#FSZ
AR1 <- Arima(logFSZ.rtn,order=c(0,0,1))
AR1
AR2 <- Arima(logFSZ.rtn,order=c(0,0,2))
AR2
AR3 <- Arima(logFSZ.rtn,order=c(1,0,0))
AR3
AR4 <- Arima(logFSZ.rtn,order=c(1,1,0))
AR4
AR5 <- Arima(logFSZ.rtn,order=c(1,2,0))
AR5
AR6 <- Arima(logFSZ.rtn,order=c(2,0,0))
AR6
AR7 <- Arima(logFSZ.rtn,order=c(2,1,0))
AR7
AR8 <- Arima(logFSZ.rtn,order=c(2,2,0))
AR8

#IXIC
AR1 <- Arima(logIXIC.rtn,order=c(0,0,1))
AR1
AR2 <- Arima(logIXIC.rtn,order=c(0,0,2))
AR2
AR3 <- Arima(logIXIC.rtn,order=c(1,0,0))
AR3
AR4 <- Arima(logIXIC.rtn,order=c(1,1,0))
AR4
AR5 <- Arima(logIXIC.rtn,order=c(1,2,0))
AR5
AR6 <- Arima(logIXIC.rtn,order=c(2,0,0))
AR6
AR7 <- Arima(logIXIC.rtn,order=c(2,1,0))
AR7
AR8 <- Arima(logIXIC.rtn,order=c(2,2,0))
AR8


#VGK data
AR1 <- Arima(logVGK.rtn,order=c(0,0,1))
AR1
AR2 <- Arima(logVGK.rtn,order=c(0,0,2))
AR2
AR3 <- Arima(logVGK.rtn,order=c(1,0,0))
AR3
AR4 <- Arima(logVGK.rtn,order=c(1,1,0))
AR4
AR5 <- Arima(logVGK.rtn,order=c(1,2,0))
AR5
AR6 <- Arima(logVGK.rtn,order=c(2,0,0))
AR6
AR7 <- Arima(logVGK.rtn,order=c(2,1,0))
AR7
AR8 <- Arima(logVGK.rtn,order=c(2,2,0))
AR8


auto.arima(logCWEB.rtn)
auto.arima(logEWD.rtn)
auto.arima(logVGK.rtn)
auto.arima(logIXIC.rtn)
auto.arima(logAEX.rtn)
auto.arima(logFSZ.rtn)
#ARCH effect
#Box.test($residuals^2,lag=20, type = "Ljung-Box")
#aic/bic <- data.frame(c(at1,at2,at3,at4))


#Estimated the volatility Equation/ Model GARCH(1,1)

#Error  distribution AEX

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "std")
ugarchfit(data=logAEX.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logAEX.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "ged")
ugarchfit(data=logAEX.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sged")
ugarchfit(data=logAEX.rtn,spec=uc)


#error distribution CWEB 

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "std")
ugarchfit(data=logCWEB.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logCWEB.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "ged")
ugarchfit(data=logCWEB.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sged")
ugarchfit(data=logCWEB.rtn,spec=uc)


#error distribution EWD

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "std")
ugarchfit(data=logEWD.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logEWD.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "ged")
ugarchfit(data=logEWD.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sged")
ugarchfit(data=logEWD.rtn,spec=uc)

#Error distribution FSZ

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "std")
ugarchfit(data=logFSZ.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logFSZ.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "ged")
ugarchfit(data=logFSZ.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sged")
ugarchfit(data=logFSZ.rtn,spec=uc)


#error distribution IXIC 

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "std")
ugarchfit(data=logIXIC.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logIXIC.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "ged")
ugarchfit(data=logIXIC.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sged")
ugarchfit(data=logIXIC.rtn,spec=uc)


#error distribution VGK

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "std")
ugarchfit(data=logVGK.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logVGK.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "ged")
ugarchfit(data=logVGK.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sged")
ugarchfit(data=logVGK.rtn,spec=uc)


#Exponential GARCH AEX

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(model="eGARCH",garchOrder=c(1,1)),
              distribution.model = "sstd")

#CWEB

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(model="eGARCH",garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logCWEB.rtn,spec=uc)

#EWD

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(model="eGARCH",garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logEWD.rtn,spec=uc)

#FSZ
uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(model="eGARCH",garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logFSZ.rtn,spec=uc)

#IXIC

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(model="eGARCH",garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logIXIC.rtn,spec=uc)

#VGK

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(model="eGARCH",garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logVGK.rtn,spec=uc)


#parameter estimates for GARCH (1,1)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logAEX.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logCWEB.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logEWD.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logFSZ.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logIXIC.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logVGK.rtn,spec=uc)


#parameter estimates TGARCH (1,1)
uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(model="fGARCH",submodel= "eGARCH", garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logAEX.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(model="fGARCH",submodel= "eGARCH", garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logCWEB.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(2,0,0)),variance.model = list(model="fGARCH",submodel= "eGARCH", garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logEWD.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(1,0,0)),variance.model = list(model="fGARCH",submodel= "eGARCH", garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logFSZ.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(model="fGARCH",submodel= "eGARCH", garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logIXIC.rtn,spec=uc)

uc=ugarchspec(mean.model=list(armaOrder=c(0,0,2)),variance.model = list(model="fGARCH",submodel= "TGARCH", garchOrder=c(1,1)),
              distribution.model = "sstd")
ugarchfit(data=logVGK.rtn,spec=uc)
