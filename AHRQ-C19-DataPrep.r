# CREATE PLOTS FROM JHU UNIFIED DATA SET
# CLONED THIS REPOSITORY
# https://github.com/CSSEGISandData/COVID-19_Unified-Dataset

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

JHU_STATE_D<-sqldf("select J.*, 
    (J.D1_cases_New-lag(J.D1_cases_new) over  (partition by J.id order by J.date asc)) as DD1_Cases_New
                   from JHU_STATE_D as J")

save(JHU_STATE_D, file = "JHU_STATE_D.RData")

## TODO - MOVE PLOTS TO ANOTHER FILE 
library(plotly)
library(ggplotlyExtra)

temp<-sqldf("select * from JHU_STATE_D where NameID='New York, United States'")
p <- ggplot(temp, aes(Date, Cases_New, D1_Cases_New))
p + geom_point() + stat_smooth()
ggplotly()

ggplot(temp, aes(x=Date)) +
  geom_bar(aes(y=Cases_New), stat="identity", size=.1) + 
  geom_line( aes(y=D1_Cases_New), size=1, color="red") +
  scale_y_continuous(
  # Features of the first axis
  name = "New Cases",
  # Add a second axis and specify its features
  sec.axis = sec_axis( ~.*1,name="Delta Cases")
)

