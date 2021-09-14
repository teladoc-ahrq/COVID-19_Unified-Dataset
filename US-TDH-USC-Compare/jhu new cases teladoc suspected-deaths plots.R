library(astsa)
library(readxl)
library(tidyverse)
#con<-file("e:/meeker teladoc covid/output/suspected_on_new_cases.log")
#sink(con,append=TRUE)
sdat<-data.frame(read_excel("E:/meeker teladoc covid/data/covid19_publication_rawdata_deaths_import3.xlsx"))
#
#   JHU new cases as dep var
#
casests<-as.ts(sdat$Cases_New_7dra)
date<-as.Date(sdat$date)
#  cut date into quarters for ggplot
datecat<-cut(date,breaks="quarter")
cases<-sdat$cases
deaths<-sdat$deaths
deathsts<-sdat$deaths
plot.ts(deathsts,main="New Deaths time series") #time series plot
#   teladoc suspected cases as x var
susp<-sdat$dx_suspected_7dra
suspts<-as.ts(sdat$dx_suspected_7dra)
plot.ts(suspts,cases,main="Teladoc Suspected COVID",xy.labels=F)

newdf<-data.frame(date,deaths,susp)
pdf<-newdf %>%
  gather(key="two",value=y,-date)
ggplot(pdf) +
  geom_line(aes(x=date,y=y,color=two)) +
  facet_grid(two~.,scales="free_y") +
  ggtitle("All data points")
#  delete early suspected case peak 
subdf<-subset(pdf,date>="2020-06-01")
ggplot(subdf) +
  geom_line(aes(x=date,y=y,color=two)) +
  facet_grid(two~.,scales="free_y") +
  ggtitle("Eliminate early peak in suspected cases")
#   plot of new deaths vs suspected cases
ggplot(newdf)+
  geom_line(aes(x=susp,y=deaths,color=datecat)) +
  ggtitle("All data points") +
  xlab("Suspected cases") + ylab("JHU deaths")
subdf2<-subset(newdf,date>="2020-06-01")
ggplot(subdf2)+
  geom_path(aes(x=susp,y=deaths)) +
  ggtitle("Eliminate early peak in suspected cases") +
  xlab("Suspected cases") + ylab("JHU deaths")
#   cases-susp plot by time periods (<10/1/20; >=10/1/20)
ggplot(subset(subdf2,date < "2020-10-01")) +
  geom_point(aes(x=susp,y=cases)) +
  ggtitle("Jun 1 2020 thru 9/30/20") +
  xlab("Suspected cases") + ylab("JHU cases")
ggplot(subset(subdf2,date >= "2020-10-01")) +
  geom_point(aes(x=susp,y=cases)) +
  ggtitle("Oct 1 2020 on") +
  xlab("Suspected cases") + ylab("JHU cases")
#sink()