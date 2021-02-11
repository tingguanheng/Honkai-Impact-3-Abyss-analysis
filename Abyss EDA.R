library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(lubridate)

options(scipen = 99)

df1<- read_xlsx("Abyss data.xlsx")
df2<- df1 %>% filter(!is.na(Disturbance))
df2$Weather<- as.factor(df2$Weather)
df2$Date<- as.Date(df2$Date)

###Date-Outcome###
df2_day<- df2 %>%
  mutate(day = weekdays(as.Date(df2$Date)),
         Outcome = case_when(Rank == "AG" & Status == "Promote" ~ "Good",
                             Rank == "RL" & Status == "Retain" ~ "Good",
                             Rank == "AG" & Status == "Retain" ~ "Bad",
                             Rank == "RL" & Status == "Relegate" ~ "Bad")) %>%
  rename(Main_DPS = `Main DPS`) %>%
  filter(day != "Tuesday" & day != "Saturday")

day_lm<- glm(as.factor(Outcome) ~ Disturbance + as.factor(day),
             data = df2_day,
             family = binomial)
glance(day_lm)
summary(day_lm)
###Observation###
#Day of the week preliminarily does not impact Status

ggplotly(ggplot(df2_day, aes(x= day, fill = Outcome))+
           geom_bar(position = "fill")+
           facet_wrap(~Rank))

###Observation###
#Agony: Might be more successful in the weekdays
#Red Lotus: Might be more successful in the weekends
#Most likely biased and need more data

df2_outcome<- df2_day %>%
  mutate(month = month(Date)) %>%
  group_by(month, Rank, Outcome) %>%
  mutate(count = n()) %>%
  filter(month != 12) %>%
  ungroup()

ggplotly(ggplot(df2_outcome, aes(x= month, y= count, fill = Outcome))+
  geom_col(position = "fill")+
  facet_wrap(~Rank))

###Observation###
#Agony: Good outcome most of the time with some outliers
#Red Lotus: Increasing number of good outcome over the months

###Conclusion###
#There is some improvement in performance over the months.

###Date-disturbance###
df2_dist<- df2 %>%
  mutate(Month = month(Date)) %>%
  group_by(Rank, Month) %>%
  mutate(Mean = round(mean(Disturbance),0)) %>%
  ungroup()

ggplot(df2_dist, aes(x= Date, y= Disturbance))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Rank)

###Observation###
#Disturbance levels of both Agony and Red lotus abyss has been increasing 
#over the months. However, Red lotus disturbance increases far faster than Agony.
#Worryingly, current disturbance in Agony is closing in to Red lotus disturbance
#9 months back.

###Conclusion###
#Powercreep is very real. Every version releases a more powerful battlesuit or 
#increase the general power of existing battlesuits. 
#Is this business model sustainable?

###Version-weather###
versioncomm<- df2 %>%
  select(2,7) %>%
  mutate(Weathergrp = case_when(Weather == 1|Weather == 9 ~ "cold",
                             Weather == 3|Weather == 10 ~ "Lightning",
                             Weather == 2 ~ "MechBio",
                             Weather == 4|Weather == 11 ~ "Fire",
                             Weather == 5|Weather == 7 ~ "Physical",
                             Weather == 6 ~ "None",
                             Weather == 8|Weather == 13 ~ "Quantum",
                             Weather == 12 ~ "Mechpsy",
                             Weather == 14 ~ "Allele")) %>%
  group_by(Version, Weathergrp) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(Version) %>%
  mutate(percent = (round(count/n(),2))*100) %>%
  distinct() %>%  
  filter(percent == max(percent)) %>%
  ungroup()
  

###Observation###
#Most common weather by Version:
#v3.6 - Lightning weather (Consistent)
#v3.7 - Physical weather (Not Consistent)
#v3.8 - Quantum & Lightning weather (Consistent)
#v3.9 - Physical weather (Consistent)
#v4.0 - Fire weather     (Not Consistent)
#v4.1 - Lightning weather (Consistent)

###Conclusion###
#Mihoyo tend to add weather that benefits the newest battlesuit released.
#Most likely to give players a reason to gacha for them to increase revenue.

###Version-Boss###
df2_versionboss<- df2 %>%
  group_by(Version, Boss) %>%
  mutate(bosscount = n()) %>%
  select(2,5,11) %>%
  arrange(Version,desc(bosscount)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(Version)  %>%  
  filter(bosscount == max(bosscount)) %>%
  ungroup()
  

###Observation###
#Most common boss:
#v3.6 - Benares (Consistent)
#v3.7 - Jizo Mitama, Aesir Heimdall, Parvati (Consistent)
#v3.8 - Tontaniuh, Argent Knight (Consistent)
#v3.9 - Argent Knight, Jizo Mitama, Deathweb + Gigant (Consistent)
#v4.0 - Argent Knight (Consistent)
#v4.1 - Hepaestus (Consistent)

###Conclusion###
#Bosses found do not counter newly released battlesuits, does not give players 
#a reason to not invest in those battlesuits.

###Weather###
typedist<- df2 %>% 
  select(3,6,7) %>%
  mutate(Weathergrp = case_when(Weather == 1|Weather == 9 ~ "cold",
                                Weather == 3|Weather == 10 ~ "Lightning",
                                Weather == 2 ~ "MechBio",
                                Weather == 4|Weather == 11 ~ "Fire",
                                Weather == 5|Weather == 7 ~ "Physical",
                                Weather == 6 ~ "None",
                                Weather == 8|Weather == 13 ~ "Quantum",
                                Weather == 12 ~ "Mechpsy",
                                Weather == 14 ~ "Allele")) %>%
  group_by(Type, Rank, Weathergrp) %>%
  mutate(count = n())%>%
  ungroup() %>%
  distinct() %>%
  group_by(Type, Rank) %>%
  mutate(percent = scales::percent(round(count/n(),2))) %>%
  arrange(desc(count)) %>%
  filter(count == max(count)) %>%
  ungroup()
###Observation###
#Dirac Sea - Red lotus - Fire weather (5)
#Dirac Sea - Agony - Lightning & Quantum weather (3)
#Q-Singularity - Red lotus - Physical weather (4)
#Q-Singaularity - Agony - Physical weather (4)
#Q-Gateway - Red lotus - Lightning weather (4)
#Q-Gateway - Agony - Physical weather (4)

###Conclusion###
#Most common weather: Physical > Lightning > Fire
#Might be wise to invest in those valks to increase general performance.

###Boss###
df2_boss<- df2 %>%
  group_by(Boss) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(percent = scales::percent(count/n())) %>%
  select(5,11,12) %>%
  arrange(desc(count)) %>%
  distinct()

###Observation###
#Most common boss
#Argent Knight > Aesir Heimdall > Tonatiuh

###Conclusion###
#Best to invest in Physical fire, lightning psychic and Quantum valks
#Consistent with weather recommendations

###Outcome determinant###
outcome_lm<- lm(Points ~ `Main DPS` + Weather + Boss, data = df2)
summary(outcome_lm)
###Observation###
#Mostly statistically insignificant


###MainDPS-Outcome###
ggplotly(ggplot(df2_outcome, aes(x= Main_DPS, fill = Outcome))+
           geom_bar(position = "fill")+
           facet_wrap(~Rank))

###Observation###
#Agony least successfull
#Herscherr of Thunder > Flame Sakitama
#Red Lotus most successfull
#Herscherr of Void > Swallowtail Phantasm > Hawk of the Fog

###Conclusion###
#Agony: Improve least successfull battlesuits
#Red Lotus: Improve other battlesuits

###Boss-Outcome###
ggplotly(ggplot(df2_outcome, aes(x= Boss, fill = Outcome))+
           geom_bar(position = "fill")+
           facet_wrap(~Rank)+
           theme(axis.text.x = element_text(angle = 90)))

###Conclusion###
#Agony least successfull
#Argent Knight > Hepaestus > Tonatiuh
#Red lotus most successful
#Assaka > Flame/Frost Emperor > Rime/Baldur > Rime/Hiero

###Conclusion###
#Agony: Improve on fight these bosses
#Red Lotus: Improve everything else

###Weather-Outcome###
df2_weatheroutcome<- df2 %>%
  mutate(Weathergrp = case_when(Weather == 1|Weather == 9 ~ "cold",
                                Weather == 3|Weather == 10 ~ "Lightning",
                                Weather == 2 ~ "MechBio",
                                Weather == 4|Weather == 11 ~ "Fire",
                                Weather == 5|Weather == 7 ~ "Physical",
                                Weather == 6 ~ "None",
                                Weather == 8|Weather == 13 ~ "Quantum",
                                Weather == 12 ~ "Mechpsy",
                                Weather == 14 ~ "Allele"),
         Outcome = case_when(Rank == "AG" & Status == "Promote" ~ "Good",
                             Rank == "RL" & Status == "Retain" ~ "Good",
                             Rank == "AG" & Status == "Retain" ~ "Bad",
                             Rank == "RL" & Status == "Relegate" ~ "Bad"))

ggplotly(ggplot(df2_weatheroutcome, aes(x= Weathergrp, fill = Outcome))+
           geom_bar(position = "fill")+
           facet_wrap(~Rank)+
           theme(axis.text.x = element_text(angle = 90)))

###Observation###
#Agony least successfull:
#Fire > Quantum > Lightning
#Red Lotus most successful:
#MechBio > Mechpsy

###Conclusion###
#Make improvements based on observation above