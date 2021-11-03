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
library("lubridate")


#dev.off()

sdat<-read_csv(dfile)

# CORRECTIONS S.T. WEIGHTED CASES/DEATHS/TESTS SO THAT SUM OF CASES OVER TIME MATCHES UNWEIGHTED VALUES
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
    casest=ts(sdat$MA_New_wt_Confirmed*sdat$rw_case,start=c(2020, stdt),frequency=365),
    casesuw=ts(sdat$MA_New_uw_Confirmed,start=c(2020,stdt),frequency=365),
    f14_casest=stats::lag(ts(sdat$MA_New_wt_Confirmed*sdat$rw_case, start=c(2020,stdt),frequency = 365),14),
    f14_deathst=stats::lag(ts(sdat$MA_New_wt_Deaths*sdat$rw_deaths,start=c(2020,stdt),frequency=365),14),
    f14_deathstu=stats::lag(ts(sdat$MA_New_uw_Deaths, start=c(2020,stdt),frequency=365),14),
    f14_casestu=stats::lag(ts(sdat$MA_New_uw_Confirmed, start=c(2020,stdt),frequency=365),14),
    testst=ts(sdat$MA_New_wt_Tests*sdat$rw_tests,start=c(2020,stdt), frequency=365),
    tests_14t=stats::lag(ts(sdat$MA_New_wt_Tests*sdat$rw_tests,start=c(2020,stdt),frequency=365),-14),
    deathst=ts(sdat$MA_New_wt_Deaths*sdat$rw_deaths,start=c(2020,stdt), frequency=365),
    deathsuw=ts(sdat$MA_New_uw_Deaths,start=c(2020,stdt),frequency=365),
    testsuw=ts(sdat$MA_New_uw_Tests,start=c(2020,stdt),frequency=365),
    suspt=ts(sdat$dx_suspected_7dra,start=c(2020,stdt), frequency=365),
    cncrn_14=stats::lag(ts(sdat$COVID_CONCERNS_7dra,start=c(2020,stdt),frequency = 365),-14),
    cncrn=ts(sdat$COVID_CONCERNS_7dra,start=c(2020,stdt), frequency=365),
    susp_14t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14),
    susp_30t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-30),
    pt_susp_14t=platform* stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14)
    #)
)
#ts.plot(tsdf[,"suspt"])
# OUTCOME OF INTEREST
#outcome<-"cases"
#outst<-"casest"
#outst<-"deathst"
#run deaths first so that plots order correctly

if(outst=="f14_casestu"){
xregc<-c("date","testst","suspt","casest")
xregc<-c("date","testsuw","suspt")
xregnc<-c("date","testst","casest")
xregcon<-c("date","testst","cncrn")
xregnc<-c("date","testsuw")
ylab<-"Cases"
grord<-14
serieslab<-"Cases (lead 14)"
#outst<-"casesuw"
coeft<-"suspt"
}

if(outst=="f14_deathst"){
  plotlist<-list()
  fitlist<-list()
  tabout<-list()
  xregc<-c("date","suspt","deathst")
  
  xregcon<-c("date","cncrn")
  xregc<-c("date","suspt","deathsuw")
  xregnc<-c("date","deathst")
  xregnc<-c("date","deathsuw")
  ylab<-"Deaths"
  serieslab<-"Deaths"
  #outst<-"deathsuw"
  grord<-14
  coeft<-"suspt"
}


summrows<-c("casest","deathst","testst","suspt")
sum<-summary(tsdf[,summrows])

train<-window(tsdf,start=c(2020,stdt),end=c(2020,stdt+255))
test<-window(tsdf,start=c(2020,stdt+256))

#LINEAR MODELS, ADDING A DATE FOR DRIFT GIVES RSQ~95%
lmnull<-tslm(data=tsdf,tsdf[,outst]~tsdf[,xregnc])
lmfit<-tslm(data=tsdf,tsdf[,outst]~tsdf[,xregc]) #same results as LM
lm_adj_r2<-summary(lmfit)$adj.r.squared
lm_mape<-MAPE(lmfit$x,lmfit$fitted)

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
#arfitlm<-Arima(tsdf[,outst],xreg=tsdf[,xregc], order=c(0,0,0))
arfitlag<-Arima(tsdf[,outst],xreg=tsdf[,xregc], order=or)
coef_p<-coeftest(arfitlag)[coeft,"Pr(>|z|)"]
model_mape<-MAPE(arfitlag$x, arfitlag$fitted)

#mod_mape<-MAPE(arfilag$x, fitted(arfitlag))
# A SUBSTITUTE FOR R2
ar_rsq<-cor(fitted(arfitlag),tsdf[,outst])^2
arfitnull<-Arima(tsdf[,outst], xreg=tsdf[,xregnc], order=or)

cind<-!is.na(tsdf[,"cncrn"])
arfitcn<-Arima(tsdf[cind,outst],xreg=tsdf[cind,xregcon], order=or)
arcomp<-Arima(tsdf[cind,outst], xreg=tsdf[cind,xregc], order=or)
lrtar<-lrtest(arfitlag,arfitnull) #p-value <0.05 for cases,=0.0015 for deaths
lrtpv<-round(lrtar$`Pr(>Chisq)`[2],digits=3)


lrtest(arcomp,arfitcn)



#  FOR SOME REASON REWEIGHTED MODEL DOES NOT CONVERGE
#arfitlm<-Arima(tsdf[,outst], order=c(0,0,0),xreg=tsdf[,xregc])

far2_xreg <- function(x, h, xreg, newxreg) {
 forecast(Arima(x, order=c(2,1,0), xreg=xreg), xreg=newxreg)
}
flm_xreg<-function(x,h,xreg,newxreg){
  forecast(Arima(x,order=c(0,0,0),xreg=xreg),xreg=newxreg)
}

xreg <- tsdf[,xregc]
xreg 

# TIME SERIES CROSS VALIDATION - PREDICTIONS 1 to h- timesteps into the future
#e<-tsCV(tsdf[,outst],flm_xreg,h=14,xreg=xreg,window=45)
e <- tsCV(tsdf[,outst], far2_xreg, h=14, xreg=xreg, window=45)
#e<-tsCV(tsdf[,outst],far2_xreg,h=14, xreg=tsdf[,xregnc],initial=30)
fit<-arfitlag
h<-14
fmat<-ts.intersect(error=e[,h],ref=fit$x, fore14=fit$x-e[,h],abserr=abs(e[,h]))
#fmat<-ts.intersect( ref=fit$x, fore14=fit$x-e[,h])
ind<-!is.na(fmat[,"fore14"])
fore14<-fmat[,"fore14"]
MAPE(fmat[ind,"fore14"],fmat[ind,"ref"])
ma_fore14<-#moving average of last 7 days for each data point
cor(fore14[!is.na(fore14)],fit$x[!is.na(fore14)])^2

fclist<-list()
fclist[outst]<-fore14
tscv_abspct<-abs(fmat[,"error"]/fmat[,"ref"])
tscv_mse <- colMeans(e^2, na.rm = T)
#tscv_mape<-100*(colMeans(tscv_abspct,na.rm=T))
mape<-mean(tscv_abspct,na.rm=T) 

rmse <-colMeans(e, na.rm=T)
# Plot the MSE values against the forecast horizon
gr<-grangertest(tsdf[,outst] ~ tsdf[,"suspt"], order = grord)
granger_p<-round(gr[2,4],digits=3)

# data.frame(h = 1:14, MSE = mse)
label<-paste0("Ex post MAPE = ",100*round(model_mape,digits=4),"%    |    ")
label<-paste0(label,"Ex ante MAPE (",h,"-Day) = ",100*round(mape,digits=4),"%\n")
label<-paste0(label, "AR Model Coefficient: ", round(arfitlag$coef["susp_14t"],digits=1)," (p<",round(coef_p,digits=4), ")\n")
#label<-paste0(label,"14 Day MSE = ", round(tscv_mse[14],digits=2),"    |    ")
label<-paste0(label,"LRT (vs. no TH data) p<",lrtpv,"\n")
label<-paste0(label, "LM: Adjusted R-squared "  ,round(lm_adj_r2,digits=2),", ")
label<-paste0(label, "MAPE = ", 100*round(lm_mape,digits=3),"%\n")

 #label<-paste0(label, "non-causality p<",granger_p)
fit$label<-label
fit$upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
fit$lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
fit$fore14 <-fore14
fit$date<-as.Date(date_decimal(as.numeric(time(fit$x)))) 
fit$ypos<-max(fit$x)*.85
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
  geom_ribbon(aes(ymin=fitlist[[!!outst]]$lower,ymax=fitlist[[!!outst]]$upper), fill="orange", alpha=0.2) +
  geom_line(aes(y=fitted(fitlist[[!!outst]]),colour="Ex Post"))+
  #geom_line(aes(y=100*tsdf[,"susp_14t"]))+
  geom_point(aes(y=fitlist[[!!outst]]$fore14,colour="Ex Ante"))+
  geom_text(aes(hjust=0, x=mean(fitlist[[!!outst]]$date)-40,y= fitlist[[!!outst]]$ypos,label=fitlist[[!!outst]]$label)) +
  theme(axis.text.y= element_text(angle = 90)) +
  ylim(0,NA)+
  #scale_x_date(c(start(fit$x),end(fit$x)), date_labels =  "%b %Y") +
  scale_color_manual(name = "", 
                     values = c("Ex Post" = "orange", "Observed" = "navy", 
                                "Ex Ante" = "gray"))+
  theme(axis.title.x = element_blank())+
  theme(axis.ticks.x = element_blank(),
  axis.text.x = element_blank())

plotlist[["reference"]]<-
  #ggplot(aes(x=time(tsdf[,])),data=NULL)+
  ggplot(aes(x=as.Date(date_decimal(as.numeric(time(tsdf))))),data=NULL)+ 
  geom_line(aes(y=tsdf[,"testst"]/30000), colour="orange")+ 
  geom_line(aes(y=tsdf[,"suspt"]), colour="navy")+
  ylab("Telehealth Clinical Diagnosis") +
  scale_color_manual(name="", values =c("Telehealth Diagnoses"="navy","US Tests"="orange")) +
  scale_y_continuous(sec.axis = sec_axis(~./.0003, name = "US Tests")) +
  geom_text(aes(hjust=0, y=max(tsdf[,"testst"]/30000), x=as.Date("2020-11-15"), label="US Tests"),colour="orange")+
  geom_text(aes(hjust=0,y=max(tsdf[,"suspt"]),x=as.Date("2020-04-10"),label="Telehealth Clinical Diagnosis"), colour="navy")+
  theme(axis.text.y=element_text(angle=90),axis.title.x = element_blank())
#remove duplicative x axis
#plotlist[["deathst"]]<-plotlist[["deathst"]] + 


ggarrange(plotlist=plotlist[c("f14_casest","f14_deathst","reference")],ncol=1,common.legend = T, align="v")
#ggarrange(plotlist=plotlist[c("casesuw","deathsuw","reference")],ncol=1,common.legend= T, align="v")
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
