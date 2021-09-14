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
date<-as.Date(sdat$date)
datets<-as.ts(date)
cases<-as.ts(sdat$Cases_New_7dra)
deaths<-as.ts(sdat$deaths)
#   teladoc suspected cases as x var
susp<-as.ts(sdat$dx_suspected_7dra)
#
# plot cross-correlation function
ccf(susp,deaths,na.action=na.omit,ylab="lagged cross-correlation")
#  x time series structure
#  to ID possible lagged terms
# PACF shows possible AR(2)
#   AR(1), MA(6) model fits well
susp_ar1<-sarima(susp,1,0,6)
susp_ar1
suspres<-resid(sarima(susp,1,0,6)$fit)
acf2(suspres)   
# ARMA(1,0,6) residuals ACF, PACF look OK
#   Now adjust/filter cases series
newdeaths=stats::filter(deaths,filter=c(1,-1.9707,0.9707),sides=1)
#  now check new ccf 
ccf(suspres,newdeaths,na.action=na.omit,ylab="Cross-correlation")
#  list ccf
ccfvalues <- ccf(suspres,newdeaths,na.action=na.omit)
ccfvalues  # lags
#   Try AR(2) MA(6)
susp_ar1<-sarima(susp,2,0,6)
susp_ar1
suspres<-resid(sarima(susp,2,0,6)$fit)
acf2(suspres)   
# ARMA(2,0,6) residuals ACF, PACF look OK
#   Now adjust/filter cases series
newdeaths=stats::filter(deaths,filter=c(1,-2.0375,1.0375),sides=1)
#  now check new ccf 
ccf(suspres,newdeaths,na.action=na.omit,ylab="Cross-correlation")
#  list ccf
ccfvalues <- ccf(suspres,newdeaths,na.action=na.omit)
ccfvalues  # lags
#   scatterplots of deaths(t) versus suspres(t+lag)
#   need to get rid of missings
suspresc<- na.omit(suspres)
newdeathsc<- na.omit(newdeaths)
lag2.plot(suspresc,newdeathsc,15)  # try lags 10
#  align x and y and create x lag variables and y lag variables
alldata<-data.frame(ts.intersect(datets,deaths,susp,ylag1=stats::lag(deaths,-1), 
  ylag2=stats::lag(deaths,-2), ylag3=stats::lag(deaths,-3),
  ylag4=stats::lag(deaths,-4),ylag5=stats::lag(deaths,-5),
  ylag6=stats::lag(deaths,-6),
  xlag1=stats::lag(susp,-1),
  xlag2=stats::lag(susp,-2),xlag3=stats::lag(susp,-3),
  xlag4=stats::lag(susp,-4),xlag5=stats::lag(susp,-5),
  xlag6=stats::lag(susp,-6),xlag7=stats::lag(susp,-7),
  xlag8=stats::lag(susp,-8),xlag9=stats::lag(susp,-9),
  xlag10=stats::lag(susp,-10),xlag11=stats::lag(susp,-11), 
  xlag12=stats::lag(susp,-12),xlag13=stats::lag(susp,-13), 
  xlag14=stats::lag(susp,-14),xlag15=stats::lag(susp,-15),
  xlag16=stats::lag(susp,-16),xlag17=stats::lag(susp,-17),
  xlag18=stats::lag(susp,-18),xlag19=stats::lag(susp,-19),
  xlag20=stats::lag(susp,-20)))
#  regression model 1: x lag variables only - xlag19 strongest
regx<-lm(deaths~xlag19,data=alldata)
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
  ggtitle("Observed and Predicted Deaths: Suspect case lag19 only") +
  xlab("Date")
#  lag y variables - first 2 lags 
regy<-lm(deaths~ylag1+ylag2,data=alldata)
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
  ggtitle("Observed and Predicted Deaths: Deaths lag 1 and 2") +
  xlab("Date")
#  suspected lag 19 plus y lags
regxy<-lm(deaths~ylag1+ylag2+xlag19,data=alldata)
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
  ggtitle("Observed and Predicted Deaths: Suspect case lag19,\ndeaths lag 1 and 2") +
  xlab("Date")

#sink()
reg<-lm(deaths~ylag1+ylag2,data=alldata)
summary(reg)
AIC(reg)
BIC(reg)


