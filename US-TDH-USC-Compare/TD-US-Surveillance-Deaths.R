if(TRUE){ #  all preparatory items 
options(repos = c(CRAN = "http://cran.rstudio.com"))
# RUN ../AHRQ-C19-DataPrep.r tot creaet TDHU data set

dfile<-"~/teladoc-ahrq/teladoc-prv/TDJHU"
#install.packages("forecast", clean=T, dependencies=T)
library(astsa)
library(readxl)
library(tidyverse)
library(tseries)
library(forecast)
library(ggplot2)
library(ggfortify)
library("segmented")
library(lmtest) #
#install.packages("MLmetrics","ggpubr")
library("ggpubr")
library("MLmetrics")
library("lubridate")
library("zoo")


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

ARFm=list()

ARFa=list()
#for(tlag in c(1:30)){ #optimizing lag values
}
if(TRUE){
tlag<-14
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
    testst=ts(sdat$MA_New_wt_Tests*sdat$rw_tests,start=c(2020,stdt), frequency=365),
    tests_14t=stats::lag(ts(sdat$MA_New_wt_Tests*sdat$rw_tests,start=c(2020,stdt),frequency=365),-14),
    deathst=ts(sdat$MA_New_wt_Deaths*sdat$rw_deaths,start=c(2020,stdt), frequency=365),
    deathsuw=ts(sdat$MA_New_uw_Deaths,start=c(2020,stdt),frequency=365),
    testsuw=ts(sdat$MA_New_uw_Tests,start=c(2020,stdt),frequency=365),
    suspt=ts(sdat$dx_suspected_7dra,start=c(2020,stdt), frequency=365),
    cncrn_14=stats::lag(ts(sdat$COVID_CONCERNS_7dra,start=c(2020,stdt),frequency = 365),-14),
    susp_14t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-1*tlag),
    susp_28t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-1*28),
    deaths_14t=stats::lag(ts(sdat$MA_New_wt_Deaths,start=c(2020,stdt), frequency=365),-1*tlag),
    cases_14t=stats::lag(ts(sdat$MA_New_wt_Confirmed,start=c(2020,stdt),frequency=365),-1*tlag),
    cases_15t=stats::lag(ts(sdat$MA_New_wt_Confirmed,start=c(2020,stdt),frequency=365),-1*(tlag+1)),
    deaths_15t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-1*(tlag+1)),
    cases_14tu=stats::lag(ts(sdat$MA_New_uw_Confirmed, start=c(2020,stdt),frequency = 365),-1*tlag),
    deaths_14tu=stats::lag(ts(sdat$MA_New_uw_Deaths,start=c(2020,stdt),frequency=365),-1*tlag),
    cases_15tu=stats::lag(ts(sdat$MA_New_wt_Confirmed, start=c(2020,stdt),frequency = 365),-1*(tlag+1)),
    deaths_15tu=stats::lag(ts(sdat$MA_New_uw_Deaths,start=c(2020,stdt),frequency=365),-1*(tlag+1)),
    cases_1t=stats::lag(ts(sdat$MA_New_wt_Confirmed,start=c(2020,stdt),frequency = 365),-1),
    deathst_1t=stats::lag(ts(sdat$MA_New_wt_Deaths,start=c(2020,stdt),frequency = 365),-1),
    susp_7t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-7),
    #susp_30t=stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-30),
    pt_susp_14t=platform* stats::lag(ts(sdat$dx_suspected_7dra,start=c(2020,stdt),frequency=365),-14)
    #)
)
} # end data setup

#ts.plot(tsdf[,"suspt"])
# OUTCOME OF INTEREST
#outcome<-"cases"
#outst<-"casest"
#outst<-"deathst"
#run deaths first so that plots order correctly
xregc<-c("susp_28t", "susp_14t","susp_7t")
#xregc<-c("suspt")
if(outst=="casesuw"){ #[1] "Ex Ante Full AR MAPE = 26.53%    |    Ex Ante Null AR MAPE = 27.58%    |    "
#if(outst=="casest"){ #[1] "Ex Ante Full AR MAPE = 32.1%    |    Ex Ante Null AR MAPE = 33.78%    |    "
  #lmlags<-c("cases_14tu","cases_15tu")
  lmlags<-c("casest","cases_1t")
  xregnc<-"testst"
  xregnc<-c()
  #xregnc<-c("testsuw")
  ylab<-"Cases"
  serieslab<-"Cases"
  coeft<-"susp_14t"
  #coeft<-"xreg"
  #tsdf[,outst]<-tsdf[,"f14_casest"]
}
#if(outst=="deathst"){ #[1] "Ex Ante Full AR MAPE = 29.99%    |    Ex Ante Null AR MAPE = 34.6%    |    "
if(outst=="deathsuw"){ #[1] "Ex Ante Full AR MAPE = 26.74%    |    Ex Ante Null AR MAPE = 30.4%    |    "
  
  plotlist<-list()
  fitlist<-list()
  tabout<-list()
  xregnc<-c()
  lmlags<-c("deaths_14t","deaths_15t")
  #lmlags<-c("deaths_14tu","deaths_15tu")
  ylab<-"Deaths"
  serieslab<-"Deaths"
  grord<-14
  coeft<-"susp_14t"
  #coeft<-"xreg"
  #coeft<-"suspt"
  #tsdf[,outst]<-tsdf[,"f14_deathst"]

}

xreglmnull<-union(xregnc,lmlags)
xreglm<-union(xreglmnull,xregc)
xregc<-union(xregc,xregnc) 

# MODEL SELECTION best lag is 11 for deaths, 14 for cases
##a<-Arima(tsdf[,outst],xreg=tsdf[,xregc],or=or)
##ARFm<-rbind(ARFm,MAPE(a$x,a$fitted))
##ARFa<-rbind(ARFa,a$aic)
##} myend

train<-window(tsdf,start=c(2020,stdt),end=c(2020,stdt+255))
test<-window(tsdf,start=c(2020,stdt+256))

#LINEAR MODELS, ADDING A DATE FOR DRIFT GIVES RSQ~95%
lmnull<-tslm(data=tsdf,tsdf[,outst]~tsdf[,xreglmnull]+0+trend)
lmfit<-tslm(data=tsdf,tsdf[,outst]~tsdf[,xreglm]+0+trend) #same results as LM
lm_adj_r2<-summary(lmfit)$adj.r.squared
lm_mape<-MAPE(lmfit$x,lmfit$fitted)

lmpred<-predict(lmfit,xreg=tsdf[,xregc],newdata=tsdf[,outst],interval="confidence")
lrtest(lmnull,lmfit)
AIC(lmfit)
#fclmfit<-forecast(lmfit,newdata=as.data.frame(tsdf[200,]),newxreg=tsdf[200,xreglm])
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
or<-c(1,1,0)
#arautofit<-auto.arima(tsdf[,"casest"], xreg=tsdf[,c("testst","susp_14t","pt_susp_14t","platform")])
#arfitlm<-Arima(tsdf[,outst],xreg=tsdf[,xregc], order=c(0,0,0))
arfitlag<-Arima(tsdf[,outst],xreg=tsdf[,xregc], order=or,include.drift=T)
coef_p<-coeftest(arfitlag)[coeft,"Pr(>|z|)"]
model_mape<-MAPE(arfitlag$x, arfitlag$fitted)
#mod_mape<-MAPE(arfilag$x, fitted(arfitlag))
# A SUBSTITUTE FOR R2
ar_rsq<-cor(fitted(arfitlag),tsdf[,outst])^2

nullxreg=tsdf[,xregnc]
if(is.null(xregnc)){
 nullxreg=NULL 
}
arfitnull<-Arima(tsdf[,outst], xreg=NULL, order=or,include.drift=T)
nullmape<-MAPE(arfitnull$x,arfitnull$fitted)
#lrtar<-lrtest(arfitlag,arfitnull) #p-value <0.05 for cases,=0.0015 for deaths
#lrtpv<-round(lrtar$`Pr(>Chisq)`[2],digits=3)
comp_mape<-round(nullmape-model_mape,digits=2)
ratio_mape<-comp_mape/nullmape
#  FOR SOME REASON REWEIGHTED MODEL DOES NOT CONVERGE
#arfitlm<-Arima(tsdf[,outst], order=c(0,0,0),xreg=tsdf[,xregc])

far2_xreg <- function(x, h,  xreg,newxreg,order) {
 #forecast(Arima(x, order=order, xreg=xreg,include.drift=T), xreg=newxreg)
  if(!is.null(xreg)){
    forecast(Arima(x, order=c(1,1,0), xreg=xreg,include.drift=T), xreg=newxreg)
  } else {
    forecast(Arima(x, order=c(1,1,0), include.drift=T), h=h, xreg=NULL)
  }
}
far_null<-function(x,h,order){
  forecast(Arima(x, order=c(1,1,0), include.drift=T),h=h)
}
far2_xregl <- function(x, h, xreg, newxreg=NULL,tlag=NULL) {
  y=x[1:(length(x)-tlag)]
  if(!is.null(xreg)){
    xreg=xreg[1:(nrow(xreg)-tlag),]
    forecast(Arima(y, order=c(2,1,0), xreg=xreg,include.drift=T), xreg=newxreg)
  } else {
    forecast(Arima(y, order=c(2,1,0), include.drift=T),h=h)
  }
}
flm_xreg<-function(x,h,xreg,newxreg){
  forecast(Arima(x,order=c(0,0,0),xreg=xreg,include.drift=T),xreg=newxreg)
}

# TIME SERIES CROSS VALIDATION - PREDICTIONS 1 to h- timesteps into the future
# LINEAR MODEL USING ONLY LAGGED PREDICTORS
win<-60
init=60
hc<-30
e<-tsCV(tsdf[,outst],far2_xreg,h=hc, xreg=tsdf[,xregc],window=win,initial=init,order=or)
#enull<-tsCV(tsdf[,outst],far0_xreg,h=7)
if(is.null(nullxreg)){
  enull<-tsCV(tsdf[,outst],far_null,h=hc,xreg=nullxreg,window=win,initial=init,order=or)
  #enull<-tsCV(tsdf[,outst],far2_xreg,h=hc, xreg=NULL,initial=0, window=win,newxreg=NULL)
} else{
  enull<-tsCV(tsdf[,outst],far2_xreg,h=hc, xreg=tsdf[,xregnc],initial=init, window=win,lag=tlag,order=or)
}
# if(is.null(xregnc)){
#   #enull<-tscv_win(tsdf[,outst],far0_xreg,h=14,initial=0,window=30,winend=0)
#   enull<-tsCV(tsdf[,outst],far0_xreg,h=7,initial=0, window=30)
# }
#e <- tsCV(tsdf[,outst], far2_xreg, h=14, xreg=tsdf[,xregc], initial=30, window=30)
#e<-tsCV(tsdf[,outst],far2_xreg,h=14, xreg=tsdf[,xregnc],initial=30)
fit<-arfitlag
fitnull<-arfitnull

getfit<-function(e,fit,h){
  fmat<-ts.intersect(error=e[,h],ref=fit$x,fore14=fit$x-e[,h],abserr=abs(e[,h]))
  ind<-!is.na(fmat[,"fore14"])
  tscv_abspct<-abs(fmat[,"error"]/fmat[,"ref"])
  tscv_mse <- colMeans(e^2, na.rm = T)
  #tscv_mape<-100*(colMeans(tscv_abspct,na.rm=T))
  ind<-!is.na(fmat[,"fore14"])
  out<- data.frame(
    mape=MAPE(fmat[ind,"fore14"],fmat[ind,"ref"]) ,
    fore14=fore14<-fmat[,"fore14"],
    cor=cor(fore14[!is.na(fore14)],fit$x[!is.na(fore14)])^2
  )
  return(out)
  
}
# if using only 14-day lagged regressors (as in TSLM), set h=1 for cross validation
# if using autoregressive terms, regressors are L1 and L2, set h=14
h<-14
arfitstats<-getfit(e=e,fit=fit,h=h)
nullfitstats<-getfit(e=enull,fit=fitnull,h=h)
# h<-14
#fmat<-ts.intersect(error=e[,h],ref=fit$x, fore14=fit$x-e[,h],abserr=abs(e[,h]))
gr<-grangertest(tsdf[,outst] ~ tsdf[,"suspt"], order = grord)
granger_p<-round(gr[2,4],digits=3)

# setting up content for plots/results
if(TRUE){
# data.frame(h = 1:14, MSE = mse)
label<-paste0("Ex Ante Full AR MAPE = ",100*round(mean(arfitstats$mape),digits=4),"%    |    ")
label<-paste0(label, "Ex Ante Null AR MAPE = ",100*round(mean(nullfitstats$mape),digits=4),"%    |    ")
#label<-paste0(label,"\n Ex ante LM MAPE rolling window (",h,"-Day) = ",100*round(mape,digits=4),"%\n")
#label<-paste0(label, "AR Model Coefficient: ", round(arfitlag$coef[coeft],digits=1)," (p<",round(coef_p,digits=4), ")\n")
#label<-paste0(label,"14 Day MSE = ", round(tscv_mse[14],digits=2),"    |    ")
#label<-paste0(label,"LRT (vs. no TH data) MAPE DIFF ", comp_mape, " p<",lrtpv,"\n")
#label<-paste0(label, "LM: Adjusted R-squared "  ,round(lm_adj_r2,digits=2),", ")
#label<-paste0(label, "MAPE = ", 100*round(lm_mape,digits=2),"\n")

 #label<-paste0(label, "non-causality p<",granger_p)
fit$label<-label
fit$upper <- fitted(fit) + 1.96*sqrt(fit$sigma2)
fit$lower <- fitted(fit) - 1.96*sqrt(fit$sigma2)
fit$fore14 <-arfitstats$fore14
fit$fore14s<-rollmean(arfitstats$fore14,7,fill=NA)
fit$fore14sup<-fit$fore14s+(1.96*rollapply(arfitstats$fore14,width=7,by=1,sd))
fit$fore14slo<-fit$fore14s-(1.96*rollapply(arfitstats$fore14,width=7,by=1,sd))
fit$fore14_null<-nullfitstats$fore14
fit$fore14_nulls<-rollmean(nullfitstats$fore14,7,fill=NA)
fit$date<-as.Date(date_decimal(as.numeric(time(fit$x)))) 
fit$ypos<-max(fit$x)*.85
fit$mean<-mean(fit$x)
fit$sd<-sd(fit$x)
fit$mape<-mean(arfitstats$mape)
fit$nullmape<-mean(nullfitstats$mape)
fit$model_mape<-model_mape
fit$n<-length(fit$x[!is.na(fit$x)])
# plot data has to be stored or it will be overwritten
}

fitlist[[outst]]<-fit

# IMPORTANT for ggplot and other tidylazy evaluations
# IF YOU WANT TO OVERRIDE LAZY EVALUATION FUNCTIONS YOU MUST
# USE !!outst and not outst. Otherwise the combined plots will not render
# the original data, it will just render the current value of outst
# this took you 3+ hours to figure out

if(TRUE){
  
plotlist[[outst]]<-
  #ggplot(aes(x=time(fitlist[[!!outst]]$x)),data=NULL) +
  ggplot(aes(x=fitlist[[!!outst]]$date),data=NULL) +
  
  #geom_point(aes(y=fitlist[[!!outst]]$fore14,colour="Full"),alpha=0.3)+
              ylab(ylab)+
  #geom_point(aes(y=fitlist[[!!outst]]$fore14_null,colour="Null"),alpha=0.6)+
  
  geom_line(aes(y=fitlist[[!!outst]]$fore14s,colour="Full"),linetype=1)+
  ylab(ylab)+
  geom_line(aes(y=fitlist[[!!outst]]$fore14_nulls,colour="Null"),linetype=1)+
  ylab(ylab)+
  
  geom_ribbon(aes(ymin=fitlist[[!!outst]]$lower,ymax=fitlist[[!!outst]]$upper), fill="darkorange3", alpha=0.2) +
  geom_line(aes(y=fitted(fitlist[[!!outst]]),colour="Ex Post"))+
  
  geom_line(aes(y=fitlist[[!!outst]]$x,colour="Observed"),linetype=2)+
 
  #geom_line(aes(y=100*tsdf[,"susp_14t"]))+
  geom_text(aes(hjust=0, x=mean(fitlist[[!!outst]]$date)-40,y= fitlist[[!!outst]]$ypos,label=fitlist[[!!outst]]$label)) +
  theme(axis.text.y= element_text(angle = 90)) +
  ylim(0,NA)+
  #scale_x_date(c(start(fit$x),end(fit$x)), date_labels =  "%b %Y") +
  scale_color_manual(name = "", 
                     values = c("Ex Post" = "darkorange3", "Observed" = "darkslategray", 
                                "Ex Ante" = "darkorange3","Full"="red","Null"="green")
                                )+
  theme(axis.title.x = element_blank())+
  theme(axis.ticks.x = element_blank(),
  axis.text.x = element_blank())

plotlist[["reference"]]<-
  #ggplot(aes(x=time(tsdf[,])),data=NULL)+
  ggplot(aes(x=as.Date(date_decimal(as.numeric(time(tsdf))))),data=NULL)+ 
  geom_line(aes(y=tsdf[,"testst"]/30000), colour="darkorange3")+ 
  geom_line(aes(y=tsdf[,"suspt"]), colour="darkslategray")+
  ylab("Telehealth Clinical Diagnosis") +
  scale_color_manual(name="", values =c("Telehealth Diagnoses"="darkslategray","US Tests"="darkorange3")) +
  scale_y_continuous(sec.axis = sec_axis(~./.0003, name = "US Tests")) +
  geom_text(aes(hjust=0, y=max(tsdf[,"testst"]/30000), x=as.Date("2020-11-15"), label="US Tests"),colour="darkorange3")+
  geom_text(aes(hjust=0,y=max(tsdf[,"suspt"]),x=as.Date("2020-04-10"),label="Telehealth\nDiagnosis"), colour="darkslategray")+
  theme(axis.text.y=element_text(angle=90),axis.title.x = element_blank())
#remove duplicative x axis
#plotlist[["deathst"]]<-plotlist[["deathst"]] + 


#ggarrange(plotlist=plotlist[c("reference","casest","deathst")],ncol=1,common.legend = T, align="v",legend="bottom")
ggarrange(plotlist=plotlist[c("casesuw","deathsuw","reference")],ncol=1,common.legend= T, legend="bottom", align="v")
save.image("~/teladoc-ahrq/TD-US-Surveillance.RData")
}
myend

if(false){
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
acf2(tsdf[,outst])
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
}