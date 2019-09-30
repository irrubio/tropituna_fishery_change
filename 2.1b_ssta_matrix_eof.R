#### Description: this script opens sea surface temperature anomaly (ssta)
#### data from the NOAA and creates a 2D ssta matrix (S form) 
#### for EOF analysis 
#### https://www.esrl.noaa.gov/psd/data/gridded/data.kaplan_sst.html
#### INPUT: "sst.mon.anom.nc" from the NOAA website
#### OUTPUT: "2.1b_matrix_ssta_eof.csv"
#### Date: 09/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(ncdf4)

#1.Set variables
latitude1 <- -30 #study area
latitude2 <- 29 #study area
longitude1 <- -35 #study area
longitude2 <- 19 #study area
date1 <- "1991-01-01"
date2 <- "2017-12-01"
y1 <- 1991
y2 <- 2017
data.criteria <- 0.75 #selection of good years, >75% of the data

#2.EXTRACT VARIABLES FROM NC SUBSETING THEM WITH VALUES FROM 1. 
nc <- nc_open("data/sst.mon.anom.nc")
ssta.raw <- ncvar_get(nc, "sst")#ssta variable
ssta1 <- array(NaN, dim = dim(ssta.raw))#map correction
ssta1[37:72, , ] <- ssta.raw[1:36, , ]#map correction
ssta1[1:36, , ] <-ssta.raw[37:72, , ]#map correction

#subset ssta variable with values from 1
latnc <- nc$dim$lat$vals-2.5
lonnc <- (nc$dim$lon$vals-2.5)-180 #change value to -180-180 degrees
LatIdx <- which((latnc) >= latitude1 & (latnc) <= latitude2)
LonIdx <- which((lonnc) >= longitude1 & (lonnc) <= longitude2)
TimeIdx <- which(as.Date(nc$dim$time$vals, origin = "1800/1/1", by = "month") 
                 >= as.Date(date1) &
                 as.Date(nc$dim$time$vals, origin = "1800/1/1", 
                          by = "month") <= as.Date(date2))
ssta<-ssta1[LonIdx, LatIdx, TimeIdx]
ti <- ncvar_get(nc, "time")
time <-as.Date(ti, origin = "1800/1/1", by = "month")[TimeIdx]
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
  
#4.Create 2D ssta matrix (1row = 1quarter), S form
ssta_m <- matrix(NaN,dim(quarter)[3], dim(quarter)[1]*dim(quarter)[2]) 
  for (value in 1:dim(quarter)[3]) {
    ssta_m[value, ] <- as.vector(quarter[, , value])#values in columns into rows
  }
  
a <- ssta_m
a[a>=0] <- 1 #ssta >= 0 kg (0s are also good data)
a[a<0] <-1 #negative values when substracting anomaly
a[which(is.finite(a)==F)] <- 0 #NA values are not good data
b<-colSums(a,na.rm=T)
#sum column values (2D to 1D) to then determine good data series
  
i<-which(b>dim(ssta_m)[1]*data.criteria) #good years selection

colnames(ssta_m)<-c(1:dim(a)[2])
brut<-ssta_m[,i] #column selection with good data to use

write.csv(brut, row.names=F, #Save S form matrix for EOF analysis
          file = "data/2.1b_matrix_ssta_eof.csv")
