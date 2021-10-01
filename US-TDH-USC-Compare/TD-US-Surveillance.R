# RUN ../AHRQ-C19-DataPrep.r tot creaet TDHU data set

dfile<-"~/teladoc-ahrq/teladoc-prv/TDJHU"

library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)
library(ggplot2)
dev.off()
sdat<-read_csv(dfile)

# Primary Predictor - dx_suspected_7dra (TD moving average)
# CHECK - is this window centered on date, or ending on date
# Outcomes - 
## Death - weighted moving average of last week of JHU Deaths)
## Cases (weighted moving average of last week of JHU Cases)
# Adjust For  - MA_ weighted moving average of last week of Tests#sink(con,append=TRUE)

date<-as.Date(sdat$Date)
#  cut date into quarters for ggplot
datecat<-cut(date,breaks="quarter")
platform=date>as.Date("2020-03-13")
stdt<-as.numeric(format(date[1],"%j"))
 
#casests<-as.ts(sdat$Cases_New_7dra)

tsdf<- 
  #as.data.frame(

    ts.intersect(
    platform=ts(platform,c(2020,stdt),frequency=365),
    date=ts(date,start=c(2020,stdt), frequency=365),
    dcat=ts(datecat,start=c(2020,stdt),frequency=365),
    casest=ts(sdat$MA_New_wt_Confirmed,start=c(2020, stdt),frequency=365),
    testst=ts(sdat$MA_New_wt_Tests,start=c(2020,stdt), frequency=365),
    deathst=ts(sdat$MA_New_wt_Deaths,start=c(2020,stdt), frequency=365),
    suspt=ts(sdat$dx_suspected_7dra,start=c(2020,stdt), frequency=365),
    susp_14t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14),
    pt_susp_14t=platform* stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14)
    #)
)


train<-window(tsdf,start=c(2020,stdt),end=c(2020,stdt+200))
test<-window(tsdf,start=c(2020,stdt+201))


lmfit<-tslm(data=tsdf,casest~susp_14t*platform+testst)
fcastall<-forecast(lmfit,newdata=as.data.frame(tsdf))
ltrain<-tslm(data=train,casest~susp_14t*platform+testst)
fcast<-forecast(ltrain,newdata=as.data.frame(test))
autoplot(fcast,series="forecast") +
  autolayer(tsdf[,c("casest")],series="cases")+
  ggtitle("Forecasts") +
  xlab("Date") + ylab("Cases")


fcast<-forecast(lmfit,newdata=as.data.frame(test))

ntsdf=tsdf[,c("testst","suspt_14t")]
or<-c(2,1,0)

# LIKELIHOOD RATIO TEST COMPARING ADDITION OF LAG OF SUSPECTED CASES
arautofit<-auto.arima(tsdf[,"casest"], xreg=tsdf[,c("testst","susp_14t","pt_susp_14t","platform")])
arfitnull<-arima(tsdf[,"casest"], xreg=tsdf[,"testst"], order=or)
arfitlag<-arima(tsdf[,"casest"],xreg=tsdf[, c("testst","susp_14t")],order=or)
#resid<-checkresiduals(arfit)
teststat<- 2*(as.numeric(logLik(arfitlag))-as.numeric(logLik(arfitnull)))
teststat
pchisq(teststat, df=1, lower.tail = FALSE )
# p-value = 0.044
adf.test(tsdf[,"casest"])
adf.test(diff(tsdf[,"casest"]))
adf.test(diff(diff(casest)))
checkresiduals(arfit)
arcast<-forecast(arfit,newxreg=tsdf[,c("testst","susp_14t")],newdata=as.data.frame(tsdf),h=10)

arf<-forecast(arfit,h=10)
deathsts<-as.ts(sdat$MA_New_wt_Deaths) 
teststs<-as.ts(sdat$MA_New_wt_Tests)
suspts<-as.ts(sdat$dx_suspected_7dra)
tposts<-as.ts(sdat$MA_New_wt_Confirmed/sdat$MA_New_wt_Tests)
cases<-sdat$MA_New_wt_Confirmed
deaths<-sdat$MA_New_wt_Deaths
tests<-sdat$MA_New_wt_Tests
susp<-sdat$dx_suspected_7dra
tpos<-sdat$MA_New_wt_Confirmed/sdat$MA_New_wt_Tests
datecatts<-as.ts(datecat)
plot<-ts.intersect(casests,deathsts,teststs,suspts)
susp_14<-as.ts(stats::lag(suspts,-14))
#set up the autoregression matrix
xmat<-cbind(
  casests,
  susp=suspts,
  datecatts,
  teststs,
  susp_8=stats::lag(suspts,-8),
  susp_14=stats::lag(suspts,-14),
  dtcatsusp_14=datecatts*(stats::lag(suspts,-14))
)

xts<-as.data.frame(ts.intersect(date,ts(casests),ts(suspts),ts(datecatts),ts(teststs),ts(stats::lag(suspts,-14))))
rows<-c(15:nrow(xmat))
ar<-c(0,1,1)
clms<-c("teststs","susp_14")
arbase<-arima(casests[rows],xreg=xmat[rows,clms],order=ar)
arsusp<-arima(casests[rows],xreg=xmat[rows,2:4], order=ar)
arlag<-arima(casests[rows],xreg=xmat[rows,2:6],order=ar)
arlagi<-arma(casests[rows],xreg=xmat[rows,2:7], order=ar)
lmlag<-tslm(xts[,"casests"]~xts[,"teststs"]+as.factor(xts[,"datecatts"])*xts[,"susp_14"])
lmlag<-tslm(tsdf$casests~tsdf$susp_14+tsdf$teststs,data=tsdf)


flmlag<-forecast(lmlag)
autoplot(flmlag)


accuracy(arlag)

an<-auto.arima(casests[rows], trace=TRUE)
plot<-forecast(aa)
# for binding the time series together
ts.intersect(date,casests,deathsts,suspts,teststs,tposts,susp_14)
e <- tsCV(xmat[rows,1], forecastfunction=rwf, h=14, xreg=xmat[rows,clms])
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:14, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()



# check that AIC is positive and LL is negative for outcome series


plot.ts(deathsts,main="New Deaths time series") #time series plot
plot.ts(teststs, main="Tests timte series")
#   teladoc suspected cases as x var

plot.ts(casests,tests)
plot.ts(suspts,cases,main="Teladoc Suspected COVID",xy.labels=F)
plot.ts(suspts,tests)

ccf(suspts,deathsts,lag.max=35,na.action=na.omit,ylab="lagged cross-correlation")
# CCF - FOR TEST POSITIVITY, PEAK @ -20
ccf(tposts,suspts,lag.max=35,na.action=na.omit,ylab="lagged cross-correlation")
z<-c(100:nrow(sdat))
# looking very different 1:100 vs 100:end, but t-8 and t-14 are both very good.
z<-c(1:100)
lag2.plot(suspts[z],casests[z],30)
lag2.plot(suspts,deathst,30)
lag2.plot(tposts,suspts,30)
ccf(teststs,casests)
ccf(casests[z],suspts[z])
auto.arima(casests)
#ARIMA(0,2,3)
auto.arima(suspts)
#ARIMA(4,0,2)
# accounting for trend and autocorrelation in suspected cases
# data is a moving aveage - expect high correlation with lag up to 7 days
acf2(suspts)
# this plot is generated in acf2 
# pacf(susp)

#  to ID possible lagged terms
#   PACF shows possible AR(2)
#   "AR(1), MA(6) model fits well"
susp_ar1<-sarima(suspts,2,0,2)
susp_ar1
suspres<-resid(sarima(suspts,2,0,2)$fit)

xreg<-ts.intersect(casests,susp_14,teststs,suspts)

auto.arima(casests, xreg=suspts)
# need to set up the matrix to control for tests

xreg<-cbind(casests,susp_14,teststs)
acf2(casests)
auto.arima(casests,xreg=xreg[,2:3])

#get the right order for differencing (stationary at diff=1)
adf.test(suspts)
adf.test(casests)
adf.test(diff(suspts))
adf.test(diff(casests))
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


#test for stationarity (1st order difference)
adf.test(casests) # p=0.085
adf.test(diff(casests)) # p=0.01
adf.test(suspts) # p=0.482
adf.test(diff(suspts)) # p=0.01


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
