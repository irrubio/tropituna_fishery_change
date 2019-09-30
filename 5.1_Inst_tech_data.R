#### Description: this script creates a database for technological/
#### management analysis through Random Forest 
#### INPUT: -"2.1a_matrix_ssta_evol.csv"
####        -"2.2_effort_total.csv"
####        -"agreement_database.csv" from literature
####        -"cons_tech_database.csv" from literature
####        -"1.1_ICCAT_data.csv" FADs data from the ICCAT website
####        -"4_lat_COG_N.csv"
####        -"4_lat_COG_S.csv"
#### OUTPUT: "5.1_data_long_FAD.csv" 
#### Date: 11/09/2018
#### Author: Iratxe Rubio
#####################################################################

#ESFUERZONOESPACIAL!!!!!!!!!!!!!!!!!!

library(tidyverse)
library(reshape2) #melt function

#1.Set variables
y1 <- 1991
y2 <- 2017
latitude1 <- -30 #study area
latitude2 <- 29 #study area
longitude1 <- -35 #study area
longitude2 <- 19 #study area
year <- y2 - y1 + 1

#2.SSTA DATABASE
sstaraw <- read.csv("data/2.1a_matrix_ssta_evol.csv", header = T)

sstaraw$t <- as.Date(sstaraw$t)
ssta <- sstaraw[which(sstaraw$t >= paste(y1,"/01/01",sep="")),]

rm(sstaraw)

#3.effort database
effort <- read.csv("data/2.2_effort_total.csv", header = T,
                      stringsAsFactors = F)

colnames(effort) <- "effort"

effort$t <- 1:dim(effort)[1]
effort$t2 <- effort$t * effort$t
effort$t3 <- effort$t2 * effort$t

#effort$quarter <- rep(c(1:4), year)
#effort$year <- rep(c(y1:y2), each = 4)

#Calculate seasonality (means by eff, mean through the whole dataset)
len<-1:dim(effort)[1]
na <- len[seq(1, length(len), 4)]
ne <- len[seq(2, length(len), 4)]
ni <- len[seq(3, length(len), 4)]
no <- len[seq(4, length(len), 4)]
jan <- mean(effort$effort[na])
apr <- mean(effort$effort[ne])
jul <- mean(effort$effort[ni])
oct <- mean(effort$effort[no])

#Calculate anomalies (data - seasonality)
#Substract to eff matrix mean of "jan...", same substraction for every t
effort$efforta <- NA
effort$efforta[na] <- effort$effort[na] - jan
effort$efforta[ne] <- effort$effort[na] - apr
effort$efforta[ni] <- effort$effort[na] - jul
effort$efforta[no] <- effort$effort[na] - oct

#4.Conservation database
cons.raw <- read.csv("data/cons_tech_database.csv", header = T,
                 stringsAsFactors = F, sep = ";")

cons.raw <- filter(cons.raw, year >= y1)
cons <- cons.raw[, c(1,2,3,7)]
cons <- cons[rep(row.names(cons), each = 4),]
cons$quarter <- rep(1:4, year)

cons$TAC <- 0
cons$TAC[cons$TAC_BET == 1 | cons$TAC_YFT == 1] <- 1
cons <- cons[, -c(2,3)]

rm(cons.raw)

#change values by hand looking at ICCAT recommendations
#"they enter into force 1 year later
cons$closure <- 0
#from rec 1999-2003 November-Janyary
cons$closure[cons$year %in% c(2000:2004) & cons$quarter %in% c(1,4)] <- 1 
#from rec 2004-2010 only November
cons$closure[cons$year %in% c(2005:2011) & cons$quarter == 4] <- 1 
#from 2011-2017 only January-Feb
cons$closure[cons$year %in% c(2012:2017) & cons$quarter == 1] <- 1 

#5.Agreements database
agr_raw <- read.csv("data/agreement_database.csv")

agr_yy<- agr_raw %>%
          filter(year >= y1) %>%
          group_by(year, mm, country) %>%
          summarise(seiner_num = mean(seiner_num, na.rm = T),
                    seiners_GRT = mean(seiners_GRT, na.rm = T),
                    tuna_quota_t = mean(tuna_quota_t, na.rm = T),
                    country_num = mean(country_num))

agr <- agr_yy %>% 
        group_by(year,mm) %>% 
        summarise(agr_num = n(),
                  agr_vessel = round(mean(seiner_num, na.rm = T), 0))
agr <- as.matrix(agr)

agreem <- matrix(NA, nrow = nrow(agr)/3, ncol = ncol(agr))
agreem[,1] <- rep(y1:y2, each = 4)
agreem[,2] <- rep(1:4)

a <- 1
for (j in 3:4) { #from monthly to quarterly variables
    for (i in seq(1,nrow(agr), 3)) { 
      agreem[a,j] <- round(mean(agr[i:(i+2), j]), 0)
      a <- a + 1
    }
a <- 1
}

agreem <- as.data.frame(agreem)
colnames(agreem) <- c("year", "quarter", "agr_num", "agr_vessel")

data <- cbind(cons, agreem[, -c(1:2)])
rm(agr, agr_raw, agr_yy, cons, agreem)

#6.Technology database
#%catch by FAD
dat <- read.csv("data/1.1_ICCAT_data.csv")

FAD1 <- filter(dat, 
               Lat >= latitude1, 
               Lat< latitude2, 
               Lon >= longitude1, 
               Lon < longitude2) %>%
  group_by(YearC, TimePeriodID, Lat,Lon) %>%
  summarise(BETfssum = sum(BETfs, na.rm = T),
            YFTfssum = sum(YFTfs, na.rm = T),
            SKJfssum = sum(SKJfs, na.rm = T),
            BETfdsum = sum(BETfd, na.rm = T),
            YFTfdsum = sum(YFTfd, na.rm = T),
            SKJfdsum = sum(SKJfd, na.rm = T),
            Eff = sum(Eff1, na.rm = T))

FAD1$quarter <- 1
FAD1$quarter[FAD1$TimePeriodID %in% c(4, 5, 6)] <- 2
FAD1$quarter[FAD1$TimePeriodID %in% c(7, 8, 9)] <- 3
FAD1$quarter[FAD1$TimePeriodID %in% c(10, 11, 12)] <- 4

FAD <- FAD1 %>% 
  group_by(YearC, quarter) %>%
  summarise(BETfdsum = sum(BETfdsum, na.rm = T),
            BETfssum = sum(BETfssum, na.rm = T),
            YFTfdsum = sum(YFTfdsum, na.rm = T),
            YFTfssum = sum(YFTfssum, na.rm = T),
            SKJfdsum = sum(SKJfdsum, na.rm = T),
            SKJfssum = sum(SKJfssum, na.rm = T),
            Eff = sum(Eff, na.rm = T),
            totcatch_FAD = BETfdsum + YFTfdsum + SKJfdsum,
            totcatch_FREE = BETfssum + YFTfssum + SKJfssum,
            totcatch = totcatch_FAD + totcatch_FREE,
            FAD_prop = round(((totcatch_FAD*100)/totcatch), 1)
            )
rm(dat, FAD1)

#7. Lat_COG
COG_N <- read.csv("data/4_lat_COG_N.csv", header = T,
                   stringsAsFactors = F)[, c(3,5,6)]
colnames(COG_N) <- c("COG_N", "Lat_N", "Lon_N")
COG_S <- read.csv("data/4_lat_COG_S.csv", header = T,
                stringsAsFactors = F)[, c(3,5,6)]
colnames(COG_S) <- c("COG_S", "Lat_S", "Lon_S")

#8.Final LONG format database for random forest analysis
data_long <- cbind(ssta[, 2], data, effort, FAD[, 13], COG_N, COG_S)
colnames(data_long)[1] <- "SSTA"

write.csv(data_long, row.names = F, file = "data/5.1_data_long.csv")