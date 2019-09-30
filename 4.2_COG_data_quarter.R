#### Description: this script calculates and plots the yearly center of gravity
#### of the PS fishery 1991-2017, 1x1, quarter and determines if it shifts
#### INPUT: "1.1_ICCAT_data.csv"
#### OUTPUT: -"4_COG_graph.pdf" pdf with COG results
####         -"4_lat_COG_N.csv"
####         -"4_lat_COG_S.csv"
#### Date: 10/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(tidyverse)

#1.Set variables
latitude1 <- -30 #study area
latitude2 <- 29 #study area
longitude1 <- -35 #study area
longitude2 <- 19 #study area

#2.Read data
data <- read.csv("data/1.1_ICCAT_data.csv", header = T)

#3.Filter data with variables from 1.
PS1 <- filter(data, 
              #YearC <= 2004,
              Lat >= latitude1, 
              Lat< latitude2, 
              Lon >= longitude1, 
              Lon < longitude2) %>%
  group_by(YearC, TimePeriodID, Lat,Lon) %>%
  summarise(Eff = sum(Eff1, na.rm = T))

#4.Create a quarter column
PS1$quarter <- 1
PS1$quarter[PS1$TimePeriodID %in% c(4,5,6)] <- 2
PS1$quarter[PS1$TimePeriodID %in% c(7,8,9)] <- 3
PS1$quarter[PS1$TimePeriodID %in% c(10,11,12)] <- 4

#5.Sum catches and effort by quarter
PS <- PS1 %>%
  group_by(YearC,quarter,Lat,Lon) %>%
  summarise(effort = sum(Eff,na.rm = T))

#effort (hour)

cp1.N <- filter(PS, Lat >= 0) 
cp1.S <- filter(PS, Lat < 0)

cp0.N <- cp1.N %>% 
  group_by(YearC, quarter) %>%
  summarise(COG_N = sum(effort*Lat, na.rm = TRUE)/sum(effort, na.rm = TRUE),#COG
            effort = sum(effort),
            Lat = mean(Lat),
            Lon = mean(Lon)) 
#calculation of effort to include variable in ggplot
#COG called lat for graphical purpose in ggplot

cp0.S <- cp1.S %>% 
  group_by(YearC, quarter) %>%
  summarise(COG.S = sum(effort*Lat, na.rm = TRUE)/sum(effort, na.rm = TRUE),#COG
            effort = sum(effort),
            Lat = mean(Lat),
            Lon = mean(Lon)) 
#calculation of effort to include variable in ggplot
#COG called lat for graphical purpose in ggplot

write.csv(cp0.N, "data/4_lat_COG_N.csv", row.names = F)
write.csv(cp0.S, "data/4_lat_COG_S.csv", row.names = F)