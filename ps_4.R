library(gt)
library(devtools)
library(tidyverse)
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
nc_poll<- read_csv("ps_4_elections-poll-nc09-3.csv")%>% clean_names()%>%
  col_types = cols(
    .default = col_character(),
    turnout_scale = col_double(),
    turnout_score = col_double(),
    w_LV = col_double(),
    w_RV = col_double(),
    final_weight = col_double(),
    timestamp = col_datetime(format = "")),
cols_label(
  race_eth = "Race",
  Dem = "DEM.",
  Rep = "REP.",
  Und = "UND."
) %>%
  fmt_percent(columns = vars(Dem, Rep, Und),
              decimals = 0)
dem<- nc_poll%>%
  filter(response=="Dem")
rep<- nc_poll%>%
  filter(response=="Rep")
und<- nc_poll%>%
  filter(response=="Und")
gendiff<-nc_poll%>% filter(gender!= gender_combined)%>% select(gender,gender_combined)
whitediff <-nc_poll%>%select(race_eth, file_race_black)%>%filter(race_eth=="White",race_eth!=file_race_black)
reptime<-nc_poll%>%select(response,timestamp)%>%filter(response=="Rep")%>%head()
reptime
reptime<-nc_poll%>%select(response,timestamp)%>%filter(response=="Rep")%>%head(n=1)
reptime
demtime<-nc_poll%>%select(response,timestamp)%>%filter(response=="Dem")%>%head(n=1)
demtime
nc_poll%>%mutate(diffpartytime= interval(reptime,demtime))


nc_poll%>%
  select(response,race_eth, final_weight)%>%
  group_by(race_eth,response)%>%
  filter(response!="3", !race_eth=="[DO NOT READ] Don't know/Refused")%>%
  summarize(total = sum(final_weight))%>%
  spread(key =  response, value = total)%>%
  mutate(all = Dem + Rep + Und) %>% 
  mutate(Dem = Dem / all) %>% 
  mutate(Rep = Rep / all) %>% 
  mutate(Und = Und / all)%>%
  select(-all)%>%
  ungroup()