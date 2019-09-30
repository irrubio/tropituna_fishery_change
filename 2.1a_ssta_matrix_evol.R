#### Description: this script downloads sea surface temperature anomaly 
#### (ssta) data from the NOAA website and creates a mean ssta matrix 
#### for temporal analysis ploting of ssta (5x5 and quarterly)
#### https://www.esrl.noaa.gov/psd/data/gridded/data.kaplan_sst.html
#### INPUT: "sst.mon.anom.nc" from the NOAA website
#### OUTPUT: - "2.1a_matrix_ssta_evol.csv"
#### Date: 09/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(ncdf4)
library(tidyverse)

#0.Download sea surface temperature anomaly
#from the NOAA website (accessed 20/12/2018)

#download.file(url = "ftp://ftp.cdc.noaa.gov/Datasets/kaplan_sst/sst.mon.anom.nc",
#              destfile = "data/sst.mon.anom.nc", 
#              mode     = "wb")

#1.Set variables
latitude1 <- -30 #study area
latitude2 <- 29 #study area
longitude1 <- -35 #study area
longitude2 <- 19 #study area
date1 <- "1856-01-01"
date2 <- "2017-12-01"
y1 <- 1856
y11 <- 1991
y2 <- 2017
data.criteria <- 0.75 #selection of good years, >75% of the data

#2.Extract variables from .nc subseting them with values from 1. 
nc <- nc_open("data/sst.mon.anom.nc")
ssta.raw <- ncvar_get(nc, "sst") #ssta variable
ssta1 <- array(NaN, dim = dim(ssta.raw)) #map correction
ssta1[37:72, , ] <- ssta.raw[1:36, , ] #map correction
ssta1[1:36, , ] <- ssta.raw[37:72, , ] #map correction

#subset ssta variable with values from 1
latnc <- nc$dim$lat$vals-2.5
lonnc <- (nc$dim$lon$vals-2.5)-180
LatIdx <- which((latnc) >= latitude1 & (latnc) <= latitude2)
LonIdx <- which((lonnc) >= longitude1 & (lonnc) <= longitude2)
TimeIdx <- which(as.Date(nc$dim$time$vals,origin = "1800/1/1", by = "month") 
                      >= as.Date(date1) &
                 as.Date(nc$dim$time$vals,origin = "1800/1/1", by = "month") 
                      <= as.Date(date2))
ssta <- ssta1[LonIdx, LatIdx, TimeIdx]
ti <- ncvar_get(nc, "time")
time <- as.Date(ti, origin = "1800/1/1", by = "month")[TimeIdx]
nc_close(nc)

#3.Create quarterly ssta array 
quarter <- array(NaN, c(dim(ssta)[1], dim(ssta)[2], dim(ssta)[3]/3)) #
  l <- 1
  for (k in seq(1,dim(ssta)[3], 3)) {
    for (i in seq(dim(ssta)[1])) {
      for (j in seq(dim(ssta)[2])) {
        if (all(is.na(ssta[i, j, k:(k+2)]))){
          quarter[i, j, l] <- NA}
        else{
          quarter[i, j, l] <- mean(ssta[i, j, k:(k+2)], na.rm = T)
        }}
    }
    l <- l + 1
  }
  
#4. Create a matrix for temporal analysis
matr1 <- matrix(NA, nrow = dim(quarter)[3], ncol = 1)  
for (x in 1:dim(quarter)[3]) {
  matr1[x] <- mean(quarter[, , x], na.rm = T)
}
matr2 <- as.data.frame(matr1)
t <- as.character(time[seq(1, length(time), 3)])
matr <- cbind(t, matr2, stringsAsFactors = F)
names(matr)[2] <- "ssta"

write.csv(matr, row.names = F, "data/2.1a_matrix_ssta_evol.csv")