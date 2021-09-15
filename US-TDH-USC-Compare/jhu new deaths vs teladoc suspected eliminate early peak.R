library(astsa)
library(readxl)
library(tidyverse)
library(dplyr)
#con<-file("e:/meeker teladoc covid/output/suspected_on_new_cases.log")
#sink(con,append=TRUE)
sdat<-data.frame(read_excel("E:/meeker teladoc covid/data/covid19_publication_rawdata_deaths_import3.xlsx"))
#
#   JHU new deaths as dep var
#
#   eliminate early peak in suspected cases first
subdat<-data.frame(subset(sdat,sdat$date>"2020-05-31"))
#
date<-as.Date(subdat$date)
datets<-as.ts(date)
cases<-as.ts(subdat$Cases_New_7dra)
deaths<-as.ts(subdat$deaths)
#   teladoc suspected cases as x var
susp<-as.ts(subdat$dx_suspected_7dra)
#
# plot cross-correlation function
ccf(susp,deaths,lag.max=35,na.action=na.omit,ylab="lagged cross-correlation")
acf2(susp)
#  to ID possible lagged terms
# PACF shows possible AR(2)
#   AR(1), MA(6) model fits well
susp_ar1<-sarima(susp,2,0,2)
susp_ar1
suspres<-resid(sarima(susp,2,0,2)$fit)
acf2(suspres)   
# ARMA(1,0,6) residuals ACF, PACF look OK
#   Now adjust/filter cases series
newdeaths=stats::filter(deaths,filter=c(1,-1.1499,0.1499),sides=1)
#  now check new ccf 
ccf(suspres,newdeaths,lag.max=35,na.action=na.omit,ylab="Cross-correlation")
#  list ccf
ccfvalues <- ccf(suspres,newdeaths,na.action=na.omit)
ccfvalues  # lags

#   scatterplots of deaths(t) versus suspres(t+lag)
#   need to get rid of missings
suspresc<- na.omit(suspres)
newdeathsc<- na.omit(newdeaths)
lag2.plot(suspresc,newdeathsc,15)  
#  align x and y and create x lag variables and y lag variables
alldata<-data.frame(ts.intersect(datets,deaths,susp,ylag1=stats::lag(deaths,-1), 
  ylag2=stats::lag(deaths,-2), ylag3=stats::lag(deaths,-3),
  ylag4=stats::lag(deaths,-4),ylag5=stats::lag(deaths,-5),
  ylag6=stats::lag(deaths,-6),
  xlag30=stats::lag(susp,-30)))
#  regression model 1: x lag variables only - xlag30 strongest
regx<-lm(deaths~xlag30,data=alldata)
summary(regx)
AIC(regx)
BIC(regx)
acf2(residuals(regx))
predx<-predict(regx,alldata)
allpredx<-cbind(alldata,predx)
allpredx$date<-as.Date(allpredx$datets,origin="1970-01-01")
ggplot(allpredx)+
  geom_path(aes(x=date,y=deaths,color='Observed deaths')) +
  geom_path(aes(x=date,y=predx,color='Predicted deaths')) +
  ggtitle("Observed and Predicted Deaths: Suspect case lag30 only\nFrom 06/06/20") +
  xlab("Date")
#  lag y variables - first 3 lags 
regy<-lm(deaths~ylag1+ylag2+ylag3,data=alldata)
summary(regy)
AIC(regy)
BIC(regy)
acf2(residuals(regy))
predy<-predict(regy,alldata)
allpredy<-cbind(alldata,predy)
allpredy$date<-as.Date(allpredy$datets,origin="1970-01-01")
ggplot(allpredy)+
  geom_path(aes(x=date,y=deaths,color='Observed deaths')) +
  geom_path(aes(x=date,y=predy,color='Predicted deaths')) +
  ggtitle("Observed and Predicted Deaths: Deaths lag 1-3\nFrom 06-01-20") +
  xlab("Date")
#  suspected lag 30 plus y lags
regxy<-lm(deaths~ylag1+ylag2+ylag3+xlag30,data=alldata)
summary(regxy)
AIC(regxy)
BIC(regxy)
acf2(residuals(regxy))
predxy<-predict(regxy,alldata)
allpredxy<-cbind(alldata,predxy)
allpredxy$date<-as.Date(allpredxy$datets,origin="1970-01-01")
ggplot(allpredxy)+
  geom_path(aes(x=date,y=deaths,color='Observed deaths')) +
  geom_path(aes(x=date,y=predxy,color='Predicted deaths')) +
  ggtitle("Observed and Predicted Deaths: Suspect case lag30,\ndeaths lag 1-3\nFrom 06-01-20") +
  xlab("Date")

#sink()
reg<-lm(deaths~ylag1+ylag2,data=alldata)
summary(reg)
AIC(reg)
BIC(reg)


