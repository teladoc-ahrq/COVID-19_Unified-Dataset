# CREATE PLOTS FROM JHU UNIFIED DATA SET
# CLONED THIS REPOSITORY
# https://github.com/CSSEGISandData/COVID-19_Unified-Dataset
# When you clone the repo, you can use SSH instead of http with this config
# git configs remote.origin.url git@github.com:teladoc-ahrq/COVID-19_Unified-Dataset.git

td_path<-"~/teladoc-ahrq/covid19_publication_rawdata_import.xlsx"
td_st_wk<-"~/teladoc-prv/ProvWk_StateCount.csv"
# ON MAC OS THIS IS REQUIRED TO RUN sqldf
options(gsubfn.engine = "R") 
library(sqldf)
library(tidyverse)
library(readxl)
library(ggplot2)
td_c19<-read_excel(td_path)
td_c19$tddate<-as.Date(as.character(td_c19$date))
td_c19<-select(td_c19,-date)
# get state-level weights
td_wt<-read_csv(td_st_wk)
# from one row per NPI-week-state to one row per state-month-year
td_wt<-sqldf("select consult_year, consult_month, consult_state,
              sum(n) as n from td_wt
              group by consult_state, consult_year, consult_month
             ")


JHU <- readRDS('COVID-19.rds')
# Need to cast dates as strings to get month and year part of dates
JHU$DateCh<-as.character(JHU$Date)
JHU<-sqldf("Select JHU.*, strftime('%Y', JHU.DateCh) as Year, strftime('%m', JHU.DateCh) as Month from JHU")

LU <- read_csv('COVID-19_LUT.csv')
# Get US Tested, Deaths, and Cases
JHU_STATE<-sqldf("SELECT JHU.*, LU.NameID, LU.ISO2 as State from JHU inner join LU 
      on 
      (LU.id=JHU.id and LU.ISO1_2C='US' 
      and (LU.Level = 'State' or Level='Country') and 
      (
        (Source='JHU' and Type='Confirmed') or
        (Source='JHU' and Type='Deaths') or
        (Source='CTP' and Type='Tests')
      ))")


JHU_US_UNWT<-sqldf("select * from JHU_STATE where State='US'")
# clear memory
#rm(JHU,LU)
# Assign weigts by state

# remove non states from TD denom and get weighting denominator as sum over  all states

td_wt<-sqldf("select  n, consult_year , consult_month, consult_state,
             sum(n) over(partition by consult_year, consult_month)
              as month_denom
              from td_wt
             where td_wt.Consult_State in (select distinct State from JHU_STATE)")

td_wt<-sqldf("select distinct *, n/month_denom as wt from td_wt")


inspect2<-sqldf("select j.*, td.wt from JHU_STATE as j join td_wt as 
                td on j.month=td.Consult_Month and j.Year=td.Consult_Year and j.STATE!='US'")

# Not TD consults in AS, select only the state-level data
JHU_STATE_WT<-sqldf("select distinct  J.Cases*td_wt.wt as Cases_wt, 
                    (j.Cases_New*ifnull(td_wt.wt, 0)) as Cases_New_wtzero , j.Cases_New * td_wt.wt as Cases_New_wt, j.* 
                  from JHU_STATE as j left join td_wt on (
                      td_wt.Consult_State=j.State and td_wt.Consult_Month=j.Month and 
                      j.Year = td_wt.Consult_Year) where j.State not in ('AS', 'US') and j.Year=2020")

#inspect<-sqldf("select round(sum(Cases_New_wt)), sum(Cases_New), round(sum(Cases_New_wtzero)), date, type from JHU_STATE_WT group by Type, Date order by Type, Date")
JHU_US_WT<-sqldf("select distinct sum(Cases_New_wt) as Cases_New_wt, sum(cases_New) as Cases_New_uw, Date, Type  
                 from JHU_STATE_WT group by Type, Date")

JHU_US_WT<-sqldf("select distinct j.Cases as Cases_US, j.Cases_new as Cases_New_US, w.* from JHU_US_WT as w 
                  join JHU_US_UNWT as j on w.Type=j.Type and w.Date=j.Date")

# plot<-sqldf("select * from JHU_US_WT where Type='Confirmed'")
# ggplot(data=plot, aes(x=Date))+
#               geom_line(aes(y=Cases_New_US, color='blue')) +
#               geom_line(aes(y=Cases_New_wt*10, color='red'))
# scaling reduces the number of TD cases by a factor of 10, mostly same shape

# Add moving averages for J.ID=US 
JHU_US_WT_MA<-sqldf (
  "select distinct J.Date, Type, J.Cases_New_wt as New_wt, J.Cases_New_uw as New_uw,
  avg(Cases_New_wt) OVER ( PARTITION BY J.Type
                  ORDER BY J.Date asc
                  RANGE BETWEEN 7 PRECEDING
                  AND 0 FOLLOWING
              ) AS MA_New_wt,
  avg(Cases_New_uw) OVER ( PARTITION BY J.Type
                  ORDER BY J.Date asc
                  RANGE BETWEEN 7 PRECEDING
                  AND 0 FOLLOWING
              ) AS MA_New_uw
  FROM JHU_US_WT as J
  "
)
# Switch fromsql to tidyverse syntax no for reshape

JHUWTWIDE<-JHU_US_WT_MA %>% pivot_wider(names_from=Type, 
                             values_from = c(MA_New_wt, MA_New_uw, 
                                             New_wt, New_uw))

TDJHU<-sqldf("select J.*, t.* from JHUWTWIDE as J join td_c19 as t on t.tddate=j.Date")
TDJHU<-select(TDJHU,-tddate)

plot<-TDJHU%>%filter(Date>'2020-03-02')
ggplot(data=plot, aes(x=Date)) + 
  geom_line(aes(y=100*MA_New_wt_Confirmed/MA_New_wt_Tests, color='US Test Positivity % (Weighted)')) +
  geom_line(aes(y=dx_suspected_7dra, color='TD Suspected Cases')) +
  geom_line(aes(y=MA_New_wt_Confirmed/1000, color='US Case Rates/100 (% Weighted)'))
 

#rm(JHU_STATE)
JHU_US_MA<-sqldf(
"select J.Date, 
  T.MA_Type_New as TestMA, T.Cases_New as Tested,
  T.MA_Type_New_wt as TestMA_wt, T.Cases_New_wt as Tested_wt,
  D.MA_Type_New as DeathsMA, D.Cases_New as Deaths,
  D.MA_Type_New_wt as DeathsMA_wt, D.Cases_New_wt as Deaths_wt,
  C.MA_Type_New as CasesMA, C.Cases_New as Cases,
  C.MA_Type_New_wt as CasesMA_wwt, C.Cases_New_wt as Cases_wt
from JHU_US_WT as J
join JHU_US_WT as T on 
(T.Date=J.DAte and T.Type='Tests')
join JHU_US_WT as C on 
(T.Date=J.Date and C.Type='Confirmed')
join JHU_US_WT as D on 
(D.Date=J.Date and D.Type='Deaths')
")
 
save(C19WT.Rdata)

#**************************************************************************#
# END DATA PROCESSING HERE - NOTHING USEFUL BELOW THIS LINE
# RUN DATA ANALYSIS STEPS AFTER THIS
#**************************************************************************#
#write_csv(JHU_STATE_MA,'~/teladoc-prv/JHU/JHU_STATE')




TDH_data_JHU <-sqldf("SELECT td.*,D.Deaths, D.Moving_Avg_Death, TestMA, Tested, DeathMA, Deaths, FROM TDH_data as td
join JHU_STATE_MA as D
T
on TD.DATE=T.DATE"
)

TDH_data <- read_excel("~/teladoc-ahrq/TDdata/covid19_publication_rawdata_import_deaths_tests.xlsx")
TDH_WT <- read_csv('~/teladoc-prv/ProvWk_StateCount.csv')


rm(JHU,LU)

JHU_STATE_D<-sqldf("select J.*, 
    (J.Cases_New-lag(J.Cases_New, 1, null) over(partition by J.Id order by J.Date asc)) as D1_Cases_New,
    lag(J.Cases_New, 1, null) over (partition by J.ID order by J.Date asc) as L1_Cases_New,
    lag(J.Cases_New, 2, null) over (partition by J.ID order by J.Date asc) as L2_Cases_New
     from JHU_STATE as J")

JHU_STATE_D<-sqldf("select distinct J.*, 
    (J.D1_cases_New-lag(J.D1_cases_new,1,null) over  (partition by J.id order by J.date asc))
    as DD1_Cases_New
    from JHU_STATE_D as J")

save(JHU_STATE_D, file = "JHU_STATE_D.RData")

sqldf("select distinct count(Cases_New) from JHU_STATE_D group by id, date")
# Returns 1

mvavg<-
"SELECT J.*,
avg(Cases_New) OVER ( PARTITION BY J.ID
                  ORDER BY J.Date asc
                  RANGE BETWEEN 7 PRECEDING
                  AND 0 FOLLOWING
) AS Moving_Avg_Cases,

avg(D1_Cases_New) OVER ( PARTITION BY J.ID
    ORDER BY J.Date asc
    RANGE BETWEEN 7 PRECEDING
        AND 0 FOLLOWING
) AS Moving_Avg_D1_Cases
FROM 
JHU_STATE_D as J
ORDER BY J.ID, J.Date"

JHU_STATE_D<-sqldf(mvavg)


avgmvd<-"select J.*, 
  (J.Moving_Avg_Cases - lag(J.Moving_Avg_Cases, 1, NULL) 
    over (partition by J.ID order by J.Date asc)) as D1_Moving_Average from
  JHU_STATE_D as J"

JHU_STATE_D<-sqldf(avgmvd)

save(JHU_STATE_D,file="JHU_STATE_D.Rdata")


## TODO - MOVE PLOTS TO ANOTHER FILE 
library(plotly)
library(ggplotlyExtra)

temp<-sqldf("select * from JHU_STATE_D where NameID='California, United States'")

#p <- ggplot(temp, aes(Date, Moving_Avg_D1_Cases))
#p + geom_point() + stat_smooth()
#ggplotly()


ggplot(temp, aes(x=Date)) +
  geom_bar(aes(y=Cases_New), stat="identity", size=.1) + 
  geom_line( aes(y=10*D1_Moving_Average), size=.5, color="blue") +
  scale_y_continuous(
  # Features of the first axis
  name = "New Cases",
  # Add a second axis and specify its features
  sec.axis = sec_axis( ~.*.1,name="Change in Moving Average of Cases")
)

