 options(repos = c(CRAN = "http://cran.rstudio.com"))
# RUN ../AHRQ-C19-DataPrep.r tot creaet TDHU data set

dfile<-"~/teladoc-ahrq/teladoc-prv/TDJHU"

library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)
library(ggplot2)
library(ggfortify)
library("segmented")
library(lmtest)
#install.packages("MLmetrics","ggpubr")
library("ggpubr")
library("MLmetrics")
#dev.off()

sdat<-read_csv(dfile)
sdat$rw_case=sum(sdat$MA_New_uw_Confirmed)/sum(sdat$MA_New_wt_Confirmed)
sdat$rw_tests=sum(sdat$MA_New_uw_Tests)/sum(sdat$MA_New_wt_Tests)
sdat$rw_deaths=sum(sdat$MA_New_uw_Deaths)/sum(sdat$MA_New_wt_Deaths)


date<-as.Date(sdat$Date)
#  cut date into quarters for ggplot
datecat<-cut(date,breaks="quarter")
platform<-date==as.Date("2020-03-13")
stdt<-as.numeric(format(date[1],"%j"))



tsdf<- 
  #as.data.frame(
    ts.intersect(
    platform=ts(platform,c(2020,stdt),frequency=365),
    date=ts(date,start=c(2020,stdt), frequency=365),
    dcat=ts(datecat,start=c(2020,stdt),frequency=365),
    casest=ts(sdat$MA_New_wt_Confirmed,start=c(2020, stdt),frequency=365),
    f14_casest=stats::lag(ts(sdat$MA_New_wt_Confirmed, start=c(2020,stdt),frequency = 365),14),
    f14_deathst=stats::lag(ts(sdat$MA_New_wt_Deaths,start=c(2020,stdt),frequency=365),14),
    testst=ts(sdat$MA_New_wt_Tests,start=c(2020,stdt), frequency=365),
    deathst=ts(sdat$MA_New_wt_Deaths,start=c(2020,stdt), frequency=365),
    suspt=ts(sdat$dx_suspected_7dra,start=c(2020,stdt), frequency=365),
    cncrn_14=stats::lag(ts(sdat$COVID_CONCERNS_7dra,start=c(2020,stdt),frequency = 365),-14),
    susp_14t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14),
    pt_susp_14t=platform* stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14)
    #)
)

# OUTCOME OF INTEREST
#outcome<-"cases"
#outst<-"casest"
#outst<-"deathst"
#run deaths first so that plots order correctly

if(outst=="casest"){
xregc<-c("date","testst","susp_14t")
xregnc<-c("date","testst")
ylab<-"Cases"
serieslab<-"Cases"
}

if(outst=="deathst"){
  plotlist<-list()
  fitlist<-list()
  xregc<-c("date","susp_14t")
  xregnc<-c("date")
  ylab<-"Deaths"
  serieslab<-"Deaths"
}

train<-window(tsdf,start=c(2020,stdt),end=c(2020,stdt+255))
test<-window(tsdf,start=c(2020,stdt+256))

#LINEAR MODELS, ADDING A DATE FOR DRIFT GIVES RSQ~95%
lmnull<-tslm(data=tsdf,tsdf[,outst]~tsdf[,xregnc])
lmfit<-tslm(data=tsdf,tsdf[,outst]~tsdf[,xregc]) #same results as LM
lmpred<-predict(lmfit,xreg=tsdf[,xregc],newdata=tsdf[,outst],interval="confidence")
# autoplot(predict(lmfit), prediction.interval=TRUE)
# ggplot2::fortify(lmfit$x)
lrtest(lmnull,lmfit)

AIC(lmfit)
fclmfit<-forecast(lmfit,newdata=as.data.frame(tsdf))
mdf<-(abs(lmfit$x-lmfit$fitted)/lmfit$x)
mape<-mean(mdf[is.finite(mdf)])*100

if(FALSE){

# LIKELIHOOD RATIO TEST (COMPARE WITH AND WITHOUT SUSPECTED CASES)
# teststat<- 2*(as.numeric(logLik(lmfit))-as.numeric(logLik(lmnull)))
# teststat
# pchisq(teststat, df=1, lower.tail = FALSE ) # p-value <0.001

# lmfitc<-lm(data=as.data.frame(tsdf[60:nrow(tsdf),]),casest~testst+susp_14t+date)
# lmcon<-lm(data=as.data.frame(tsdf[60:nrow(tsdf),]),casest~testst+date+cncrn_14)
# lrtest(lmcon,lmfitc) # p-value < 0.001,  r-squared .9579 vs .9613 for time available

# INVESTIGATE OPTIMAL TIME SPLINES
# fit_segmented = segmented(lmfit, seg.Z = ~date, npsi = 1,data=tsdf)  # one change points along x 
# single spline cuts at 2020-10-29
# fits_2<-segmented(lmfit,seg.Z=~date, npsi=2, data = tsdf)
# passes LRT, but not a big change in the date spline (still around november 2020)
# summary(fit_segmented)

fcastall<-forecast(lmfit,newdata=as.data.frame(tsdf))
autoplot(fcastall,series="dfcast")


ltrain<-tslm(data=train,train[,outst]~train[,xregc])
# USEFUL PLOT OVERLAY OF LM FORECAST ON TEST VS. TRAIN
# SEEMS TO HAVE SOME INCONSITENCY WITHOVERLAY VS. SIDE BY SIDE
fcast<-forecast(ltrain,newdata=as.data.frame(test))
autoplot(fcast,series="forecast") +
  autolayer(train[,outst],series="cases (training data)")+
  autolayer(test[,outst], series="observed (test data)") +
  ggtitle("Forecasts for 3rd Wave Based on Telehealth Diagnoses (14-Day Lag)") +
  xlab("Date") + ylab(ylab)
}

# AUTOREGRESSIVE MODELS
or<-c(2,1,0)
#arautofit<-auto.arima(tsdf[,"casest"], xreg=tsdf[,c("testst","susp_14t","pt_susp_14t","platform")])
arfitlm<-Arima(tsdf[,outst],xreg=tsdf[,xregc], order=c(0,0,0))
arfitlag<-Arima(tsdf[,outst],xreg=tsdf[,xregc], order=or)
arfitnull<-Arima(tsdf[,outst], xreg=tsdf[,xregnc], order=or)
lrtar<-lrtest(arfitlag,arfitnull) #p-value <0.05 for cases,=0.0015 for deaths
lrtpv<-round(lrtar$`Pr(>Chisq)`[2],digits=3)
#  PLOT PREDICTION INTERVALS
fit<-arfitlag
arfitlag<-Arima(tsdf[,outst],order=or,xreg=tsdf[,xregc])
mdf<-(abs(arfitlag$x-arfitlag$fitted)/arfitlag$x)
mape<-mean(mdf[is.finite(mdf)])*100
arfitlm<-Arima(tsdf[,outst], order=c(0,0,0),xreg=tsdf[,xregc])

far2_xreg <- function(x, h, xreg, newxreg) {
 forecast(Arima(x, order=c(2,1,0), xreg=xreg), xreg=newxreg)
}
flm_xreg<-function(x,h,xreg,newxreg){
  forecast(Arima(x,order=c(0,0,0),xreg=xreg),xreg=newxreg)
}

xreg <- tsdf[,xregc]
e<-tsCV(tsdf[,outst],flm_xreg,h=14,xreg=xreg)
e <- tsCV(tsdf[,outst], far2_xreg, h=14, xreg=xreg)

fmat<-ts.intersect(error=e[,14],ref=fit$x)
fore14<-fmat[,"error"]+fmat[,"ref"]
fclist<-list()
fclist[outst]<-fore14
tscv_abserr<-abs(e-tsdf[,outst])/tsdf[,outst]
tscv_mse <- colMeans(e^2, na.rm = T)
tscv_mape<-100*(colMeans(tscv_abserr,na.rm=T))
rmse <-colMeans(e, na.rm=T)
# Plot the MSE values against the forecast horizon
# data.frame(h = 1:14, MSE = mse)

label<-paste0("MAPE = ",round(mape,digits=2),"\n")
label<-paste0(label,"14 Day CVMAPE = ", round(tscv_mape[14], digits=2), "\n")
label<-paste0(label,"14 Day MSE = ", round(tscv_mse[14],digits=2),"\n")
label<-paste0(label,"LRT (vs. no TH data) p<",lrtpv)
fit$label<-label
fit$upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
fit$lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
fit$fore14 <-fore14
fit$date<-as.Date(date_decimal(as.numeric(time(fit$x)))) 
fit$ypos<-max(fit$x)*.66
# plot data has to be stored or it will be overwritten


fitlist[[outst]]<-fit

#rm(fit)


# IMPORTANT for ggplot and other tidylazy evaluations
# IF YOU WANT TO OVERRIDE LAZY EVALUATION FUNCTIONS YOU MUST
# USE !!outst and not outst. Otherwise the combined plots will not render
# the original data, it will just render the current value of outst
# this took you 3+ hours to figure out
  
plotlist[[outst]]<-
  #ggplot(aes(x=time(fitlist[[!!outst]]$x)),data=NULL) +
  ggplot(aes(x=fitlist[[!!outst]]$date),data=NULL) +
  geom_line(aes(y=fitlist[[!!outst]]$x,colour="Observed"))+
              ylab(ylab)+
  geom_ribbon(aes(ymin=fitlist[[!!outst]]$lower,ymax=fitlist[[!!outst]]$upper), alpha=0.2) +
  geom_line(aes(y=fitted(fitlist[[!!outst]]),colour="1-Day Advance Forecast"))+
  #geom_line(aes(y=100*tsdf[,"susp_14t"]))+
  geom_point(aes(y=fitlist[[!!outst]]$fore14,colour="14-Day Advance Forecast"))+
  geom_text(aes(hjust=0, x=min(fitlist[[!!outst]]$date),y= fitlist[[!!outst]]$ypos,label=fitlist[[!!outst]]$label)) +
  theme(axis.text.y= element_text(angle = 90)) +
  ylim(0,NA)+
  #scale_x_date(c(start(fit$x),end(fit$x)), date_labels =  "%b %Y") +
  scale_color_manual(name = "", 
                     values = c("1-Day Advance Forecast" = "orange", "Observed" = "navy", 
                                "14-Day Advance Forecast" = "gray"))+
  theme(axis.title.x = element_blank())+
  theme(axis.ticks.x = element_blank(),
  axis.text.x = element_blank())

plotlist[["reference"]]<-
  #ggplot(aes(x=time(tsdf[,])),data=NULL)+
  ggplot(aes(x=as.Date(date_decimal(as.numeric(time(tsdf))))),data=NULL)+ 
  geom_line(aes(y=tsdf[,"testst"]), colour="orange")+
  geom_line(aes(y=2000*tsdf[,"suspt"]), colour="navy")+
  ylab("Telehealth Clinical Diagnosis") +
  scale_color_manual(name="", values =c("Telehealth Diagnoses"="navy","US Tests"="orange")) +
  scale_y_continuous(sec.axis = sec_axis(~./2000, name = "US Tests / 2000")) +
  theme(axis.title.y=element_text(angle=90),axis.title.x = element_blank())
#remove duplicative x axis
#plotlist[["deathst"]]<-plotlist[["deathst"]] + 
ggarrange(plotlist=plotlist[c("deathst","casest","reference")],ncol=1,common.legend = T, align="v")

myend
if(false){
arfitci<-ts.intersect(
  upper=ts(fitted(arfitlag)+1.96*sqrt(arfitlag$sigma2),start=c(2020,stdt),frequency=365),
  lower=ts(fitted(arfitlag)-1.96*sqrt(arfitlag$sigma2),start=c(2020,stdt),frequency=365),
  fit=ts(fitted(arfitlag),start=c(2020,stdt),frequency=365),
  date=tsdf[,"date"],
  type="ARIMA"
)

lmfitci<-data.frame(
  upper=ts(lmpred[,"upr"],start=c(2020,stdt),frequency=365),
  lower=ts(lmpred[,"lwr"],start=c(2020,stdt),frequency=365),
  fit=ts(lmpred[,"fit"],start=c(2020,stdt),frequency=365),
  type="LM",
  date=tsdf[,"date"]
)

refdata<-data.frame(
  upper=tsdf[,outst],
  lower=tsdf[,outst],
  fit=tsdf[,outst],
  type=outst,
  date=tsdf[,"date"]
)
}

# LIKELIHOOD RATIO TEST COMPARING ADDITION OF LAG OF SUSPECTED CASES TO BASIC MODEL
#resid<-checkresiduals(arfit)
teststat<- 2*(as.numeric(logLik(arfitlag))-as.numeric(logLik(arfitnull)))
teststat
pchisq(teststat, df=1, lower.tail = FALSE ) # p-value = 0.044

adf.test(tsdf[,outst])
adf.test(diff(tsdf[,outst]))


an<-auto.arima(casests[rows], trace=TRUE)
plot<-forecast(aa)
# for binding the time series together

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
