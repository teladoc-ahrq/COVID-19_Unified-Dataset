# CREATE PLOTS FROM JHU UNIFIED DATA SET
# CLONED THIS REPOSITORY
# https://github.com/CSSEGISandData/COVID-19_Unified-Dataset

#git config remote.origin.url git@github.com:daniella_meeker/your_project.git

library(sqldf)
library(tidyverse)
JHU <- readRDS('COVID-19.rds')
LU <- read_csv('COVID-19_LUT.csv')


JHU_STATE<-sqldf("SELECT JHU.*, LU.NameID, LU.ISO2 from JHU inner join LU 
      on 
      (LU.id=JHU.id and LU.ISO1_2C='US' 
      and LU.Level = 'State' and Source='JHU' 
      and Type='Confirmed') 
      order by jhu.id, jhu.date")

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

