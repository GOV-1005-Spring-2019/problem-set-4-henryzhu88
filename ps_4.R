library(gt)
library(devtools)
library(tidyverse)
library(readr)
library(janitor)
nc_poll<- read_csv("ps_4_elections-poll-nc09-3.csv")%>% clean_names()%>%
  col_types = cols(
    .default = col_character(),
    turnout_scale = col_double(),
    turnout_score = col_double(),
    w_LV = col_double(),
    w_RV = col_double(),
    final_weight = col_double(),
    timestamp = col_datetime(format = ""))
dem<- nc_poll%>%
  filter(response=="Dem")
count(dem)
rep<- nc_poll%>%
  filter(response=="Rep")
count(rep)
und<- nc_poll%>%
  filter(response=="Und")
count(und)