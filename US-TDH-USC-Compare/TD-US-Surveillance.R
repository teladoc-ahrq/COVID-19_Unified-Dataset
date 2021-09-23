# RUN ../AHRQ-C19-DataPrep.r tot creaet TDHU data set

dfile<-"~/teladoc-ahrq/teladoc-prv/TDJHU"

library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)
library(broom)
library(ggplot2)
theme_set(theme_bw())
library(dplyr)
library(mgcv)
library(tidymv)
sdat<-read_csv(dfile)
# something odd with the first few test observations need to verify data
sdat<-sdat[5:nrow(sdat),]

# Primary Predictor - dx_suspected_7dra (TD moving average)
# CHECK - is this window centered on date, or ending on date
# Outcomes - 
## Death - weighted moving average of last week of JHU Deaths)
## Cases (weighted moving average of last week of JHU Cases)
# Adjust For  - MA_ weighted moving average of last week of Tests#sink(con,append=TRUE)

date<-as.Date(sdat$Date)
#  cut date into quarters for ggplot
datecat<-cut(date,breaks="quarter")


#TD suspected cases leads by t=-14
ccf(suspts,deathsts)
casests<-as.ts(sdat$MA_New_wt_Confirmed)
deathsts<-as.ts(sdat$MA_New_wt_Deaths) 
teststs<-as.ts(sdat$MA_New_wt_Tests)
suspts<-as.ts(sdat$dx_suspected_7dra)
tposts<-as.ts(sdat$MA_New_wt_Confirmed/sdat$MA_New_wt_Tests)
cases<-sdat$MA_New_wt_Confirmed
deaths<-sdat$MA_New_wt_Deaths
tests<-sdat$MA_New_wt_Tests
susp<-sdat$dx_suspected_7dra
tpos<-sdat$MA_New_wt_Confirmed/sdat$MA_New_wt_Tests

#casests<-as.ts(sdat$Cases_New_7dra)
ucasests<-as.ts(sdat$MA_New_uw_Confirmed)
udeathsts<-as.ts(sdat$MA_New_uw_Deaths)
# unweighted for sanity check
ccf(ucasests,udeathsts)
# weighted is not too different; highest at t=0, slow decline
ccf(casests,deathsts)

ctdifts<-as.ts(casests-teststs)
ccf(ctdifts[1:100],suspts[1:100], lag.max=60)
ccf(ctdifts[101:200], suspts[101:200], lag.max=60)
ccf(ctdifts[201:300], suspts[201:300], lag.max=60)
ccf(ctdifts[300:length(ctdifts)], suspts[300:length(suspts)], lag.max=60)
ccf(ctdifts[1:90], suspts[1:90], lag.max=60)
ccf(ctdifts[200:length(ctdifts)],suspts[200:length(suspts)],lag.max=60)
ccf(tposts[100:length(tposts)],suspts[100:length(tposts)], lag.max=60)
plot.ts(ctdifts)
plot.ts(suspts)
plot.ts(casests)

summary(reg)
plot.ts(teststs)
plot.ts(tposts)
# for binding the time series together
ts.intersect(date,casests,deathsts,suspts,teststs,tposts)

# check that AIC is positive and LL is negative for outcome series


plot.ts(deathsts,main="New Deaths time series") #time series plot
plot.ts(teststs, main="Tests timte series")
#   teladoc suspected cases as x var

plot.ts(casests,tests)
plot.ts(suspts,cases,main="Teladoc Suspected COVID",xy.labels=F)
plot.ts(suspts,tests)

ccf(suspts,deathsts,lag.max=35,na.action=na.omit,ylab="lagged cross-correlation")
# CCF - FOR TEST POSITIVITY, PEAK @ -20
ccf(teststs,casests,lag.max=35,na.action=na.omit,ylab="lagged cross-correlation")
lag2.plot(casests,suspts,40)
lag2.plot(casests,deathsts,40)
lag2.plot(ctdifts,suspts,40)
ccf(ctdifts[100:340,], suspts[100:340,],lag.max=60)
lag2.plot(casests,teststs,40)
acf2(ctdifts)
auto.arima(ctdifts)
auto.arima(ctdifts, xreg=suspts)
ccf(teststs,casests)



auto.arima(casests)
#ARIMA(0,2,3)
auto.arima(suspts)
auto.arima(casests,xreg=suspts+teststs)
#ARIMA(4,0,2)
# accounting for trend and autocorrelation in suspected cases
# data is a moving aveage - expect high correlation with lag up to 7 days
acf2(suspts)
# this plot is generated in acf2 
# pacf(susp)

#  to ID possible lagged terms
#   PACF shows possible AR(2)
#   AR(1), MA(6) model fits well
susp_ar1<-sarima(suspts,2,0,2)
susp_ar1
suspres<-resid(sarima(suspts,2,0,2)$fit)



# Per Wendy "2 looks good" 
# to ID possible lagged terms
# PACF shows possible AR(2)
# AR(1), MA(6) model fits well (expect MA 7)
susp_ar1<-sarima(susp, c(2,0,2))
sa<-arima(casests,xreg=c(suspts,testts))
susp_ar1
# Q for wendy -- is this addressing autocorrelation?
suspres<-resid(sarima(susp,2,0,2)$fit)
acf2(suspres)

datets<-as.ts(date)
dcatts<-as.ts(cut(date,breaks="quarter"))
plot<-as.data.frame(
  ts.intersect(
    datets,
    dcatts,
    deathsts,
    casests,
    teststs,
    suspts,
    susp_14=stats::lag(suspts,-14),
    
    test_14=stats::lag(teststs,-14),
    deaths_21=stats::lag(deathsts,-21)
  ),
  date
)

LagReg(plot$deathsts,plot$suspts, L=3)


# pick the bet lags for tests
z=c(1:335)
lag2.plot(casests[z],teststs[z],30)
lag2.plot(casests, suspts)


regct<-lm(data=plot, casests~susp_14+teststs)
regdt<-lm(data=plot, deathsts~susp_14+teststs)
regd<-lm(data=plot,deathsts~teststs)
regc<-lm(data=plot, casests~teststs)
reg<-lm(data=plot, casests~teststs)
#reg<-lm(deathsts~susp_14*date)
#reg<-lm(deathsts~date*datecat)
predc<-predict(regc)
predd<-predict(regd)
predct<-predict(regct)
models<-list(regc,regct, regd, regdt)
lapply(models, summary)

anova(regc, regct) 
anova(regd,regdt)
sapply(models,AIC)

summary(regct)
AIC(reg)
BIC(reg)

plot$predc<-predc
ggplot(data = plot, aes(x=date))+
  geom_path(aes(y=deathsts,color='Observed deaths')) +
  geom_path(aes(y=predd,color='Predicted deaths'))

    ggplot(data=plot, aes(x=as.Date(datets),origin=date[1])+
      geom_path(aes(y=casests, color='Observed Cases'))+
      #geom_path(aes(y=predc, color='Predicted Cases  (L14, unadjusted)')) +
      geom_path(aes(y=predct,color='Predicted Cases (L14, test adjusted'))+
      geom_path(aes(y=suspts*100,color='TD Clinical Dx( x 100'))
    



# https://stackoverflow.com/questions/46186677/how-to-loop-lapply-to-create-lag-terms-over-multiple-variables-in-r
# n<- names(TDJHU)
# lagvars<-n[!grepl("Date",n)]
# #vars <- c("b","c")
# rpv  <- rep(1:2, each=length(lagvars))
# #df[, paste(vars, "lag", rpv, sep="_") := Map(shift, .SD, rpv), by=a, .SDcols=vars]
# TDJHU[, paste(lagvars, "lag", rpv, sep="_") := Map(shift, .SD, rpv), .SDcols=lagvars]

# Try  this
# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
# https://github.com/SubhasreeUC/Master/blob/master/TimeSeriesExample.R

#install.packages("funitroots", "forecast","lmtest","FitAR")
library("fUnitRoots")
library(lmtest)
library("forecast")
library(FitAR)
startyear<-format(RawData$Date[1],'%Y')
endyear<-format(RawData$Date[length(RawData$Date)],'%Y')
RawData<-TDJHU[,c("Date","MA_New_wt_Confirmed")]
startday<-as.numeric(RawData$Date[1])-(as.numeric(as.Date(paste0(startyear,"-01-01"))))
endday<-as.numeric(RawData$Date[length(RawData$Date)])-(as.numeric(as.Date(paste0(endyear,'-01-01'))))
tsData = ts(RawData, start = c(as.numeric(startyear),as.numeric(startday)), end=c(as.numeric(endyear),as.numeric(endday)), frequency = 365)
components.ts = decompose(tsData)
plot(components.ts)
acf(tsData,lag.max=365)
pacf(tsData, lag.max=30)
fitARIMA<-arima(RawData$MA_New_wt_Confirmed)
fitARIMA<-arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
fitARIMA
