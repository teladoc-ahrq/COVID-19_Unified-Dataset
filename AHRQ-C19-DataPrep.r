# CREATE PLOTS FROM JHU UNIFIED DATA SET
# CLONED THIS REPOSITORY
# https://github.com/CSSEGISandData/COVID-19_Unified-Dataset
# When you clone the repo, you can use SSH instead of http with this config
# git configs remote.origin.url git@github.com:teladoc-ahrq/COVID-19_Unified-Dataset.git

td_path<-"~/teladoc-ahrq/covid19_publication_rawdata_import.xlsx"
td_st_wk<-"~/teladoc-prv/ProvWk_StateCount.csv"
# ON MAC OS THIS IS REQUIRED TO RUN sqldf
options(gsubfn.engine = "R") 
#install.packages("tidyverse")
library(sqldf)
library(tidyverse)
library(readxl)
td_c19<-read_excel(td_path)
td_wt<-read_csv(td_st_wk)
td_wt<-sqldf("select consult_year, consult_month, consult_state,
              sum(n) as n from td_wt
              group by consult_state, consult_year, consult_month
             
             ")

td_wt<-sqldf("select n, consult_year , consult_month, consult_state,
             sum(n) over(partition by consult_year, consult_month)
              as month_denom
              from td_wt
order by consult_year, consult_month, consult_state
             "
             )

td_wt<-sqldf("select distinct *, n/month_denom from td_wt")

JHU <- readRDS('COVID-19.rds')
# This is really annoying and screws up partitioning
JHU$DateCh<-as.character(JHU$Date)
JHU<-sqldf("Select JHU.*, strftime('%Y', JHU.DateCh) as Year, strftime('%m', JHU.DateCh) as Month from JHU")
LU <- read_csv('COVID-19_LUT.csv')

# Get US Tested, Deaths, and Cases
JHU_STATE<-sqldf("SELECT JHU.*, LU.NameID, LU.ISO2 from JHU inner join LU 
      on 
      (LU.id=JHU.id and LU.ISO1_2C='US' 
      and (LU.Level = 'State' or Level='Country') and 
      (
        (Source='JHU' and Type='Confirmed') or
        (Source='JHU' and Type='Deaths') or
        (Source='CTP' and Type='Tested')
      ))")
# clear memory
rm(JHU,LU)
# Add moving averages
JHU_STATE_MA<-sqldf (
  "select J.*, 
  avg(Cases_New) OVER ( PARTITION BY J.ID, J.Type
                  ORDER BY J.Date asc
                  RANGE BETWEEN 7 PRECEDINGß
                  AND 0 FOLLOWING
              ) AS MA_Type_New
  FROM JHU_STATE as J
  --where J.Type='Confirmed' and J.ID='US'
  "
)
rm(JHU_STATE)
JHU_STATE_MA<-sqldf(
"select J.Date, J.Year, J.Month, J.ID, J.ISO2 as ST,
  T.MA_Type_New as TestMA, T.Cases_New as Tested,
  D.MA_Type_New as DeathsMA, D.Cases_New as Deaths,
  C.MA_Type_New as CasesMA, C.Cases_New as Cases
from JHU_STATE_MA as J
join JHU_STATE_MA as T on 
(T.ID=J.ID and T.Date=J.DAte and T.Type='Tested')
join JHU_STATE_MA as C on 
(T.ID=J.ID and T.Date=J.Date and C.Type='Confirmed')
join JHU_STATE_MA as D on 
(D.ID=J.ID and D.Date=J.Date and D.Type='Deaths')
")

save.image(file="C19.Rdata")

TDJHU<-sqldf("select J.*, t.* from JHU_STATE_MA as J join td_path as t 
             
             
             ")

#write_csv(JHU_STATE_MA,'~/teladoc-prv/JHU/JHU_STATE')




TDH_data_JHU <-sqldf("SELECT td.*,D.Deaths, D.Moving_Avg_Death, TestMA, Tested, DeathMA, Deaths, FROM TDH_data as td
join JHU_STATE_MA as D
T
on TD.DATE=T.DATE"
)

TDH_data <- read_excel("~/teladoc-ahrq/TDdata/covid19_publication_rawdata_import_deaths_tests.xlsx")
TDH_WT <- read_csv('~/teladoc-prv/ProvWk_StateCount.csv')

#**************************************************************************#
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

