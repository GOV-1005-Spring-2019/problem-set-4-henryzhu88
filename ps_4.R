library(gt)
library(devtools)
library(tidyverse)
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
library(knitr)
nc_poll<- read_csv("ps_4_elections-poll-nc09-3.csv")%>% clean_names()%>%
  col_types = cols(
    .default = col_character(),
    turnout_scale = col_double(),
    turnout_score = col_double(),
    w_LV = col_double(),
    w_RV = col_double(),
    final_weight = col_double(),
    timestamp = col_datetime(format = "")) %>%
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
bind_cols(reptime,demtime)%>%
  mutate(diffpartytime= interval(timestamp1,timestamp))%>%
  mutate(diffpartytime_m = diffpartytime %/% minutes(1))%>%select(diffpartytime_m)


nc_poll%>%
  select(response,race_eth, final_weight)%>%
  mutate(race_eth = fct_relevel(race_eth, "White", "Black", 
                                "Hispanic", "Asian", "Other"))%>%
  group_by(race_eth,response)%>%
  filter(!race_eth=="[DO NOT READ] Don't know/Refused")%>%
  summarize(total = sum(final_weight))%>%
  spread(key =  response, value = total,fill=0)%>%
  mutate(all = Dem + Rep + Und +`3`) %>% 
  mutate(Dem = Dem / all) %>% 
  mutate(Rep = Rep / all) %>% 
  mutate(Und = Und / all)%>%
  select(-all,-`3`)%>%
  ungroup()

educ_weight <- nc_poll %>% 
  mutate(educ = fct_relevel(educ,"Grade school","High school","Some college or trade school","Bachelors' degree", "Graduate or Professional Degree")) %>% 
  filter(!educ=="[DO NOT READ] Refused")%>%
  select(educ,final_weight) %>%
ggplot(aes(x =educ, y = final_weight)) + 
  geom_violin()+
  geom_jitter(width = 0.05,alpha=0.3)+ 
  coord_flip()+
  labs(title = "More Educated Matters Less in North Carolina 9th",
       subtitle = "Poll gives more weight to people who are less likely to participate in polls",
       caption = "Source: New York Times Upshot/Siena College 2018 live polls") +
  xlab(NULL) +
  ylab("Weight Given to Respondent in Calculating Poll Results")

agephone<- nc_poll%>% 
  mutate(ager = fct_relevel(ager, "18 to 34", "35 to 49", 
                                "50 to 64", "65 and older"))%>%
  select(ager,phone_type,response)%>%
  group_by(ager,phone_type,response)%>% 
  filter(!ager=="[DO NOT READ] Refused")%>%
  summarize(N=n())%>%
  spread(key =  ager, value = N,fill=0)%>%
  ggplot(aes(x = ager, y = N, fill = response))+
  geom_col(position = "dodge2") +
  labs(x = "Age",y = "Number", fill = "Party") +
  theme(legend.position = "top")
  
  ggplot(aes(x = "N", y = "ager"))+
  geom_point()
  
  mutate(all = Dem + Rep + Und +`3`) %>%
  +
  facet_wrap(~ phone_type)

+
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 2))%>%
  