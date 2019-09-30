#### Description: this script creates an effort quarterly array 
#### and an effort anomaly matrix with S form for EOF analysis of tropical 
#### tunas
#### INPUT: "1.2_PS_catch_effort.nc" 
#### OUTPUT: - "1.3_graph_effort.rda": effort 3D array (by quarter, 1x1)
####         - "1.3_matrix.csv": efforta S form matrix (by quarter, 5x5)
####         - "1.3_i.csv": vector containing good data positions 
#### 1991-2017, East Atlatic Ocean, quarterly data, PS, tropical tunas
#### Date: 03/07/2018
#### Author: Iratxe Rubio
############################################################################

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
degree <- 5
data.criteria <- 0.75 #selection of good years, >75% of the data

#2.Extract variables from .nc subseting them with values from 1. 
nc <- nc_open("data/1.2_PS_catch_effort.nc")

LatIdx <- which(nc$dim$lat$vals >= latitude1 & 
                nc$dim$lat$vals <= latitude2)

LonIdx <- which(nc$dim$lon$vals >= longitude1 & 
                nc$dim$lon$vals <= longitude2)

TimeIdx <- which(as.Date(nc$dim$time$vals, origin = "1970-01-01", 
                         by = "month") >= as.Date(date1) &
                 as.Date(nc$dim$time$vals, origin = "1970-01-01",
                         by = "month") <= as.Date(date2))

Eff <- ncvar_get(nc, "Effort")[LonIdx, LatIdx, TimeIdx]
ti <- ncvar_get(nc, "time")
time <- as.Date(ti, origin = "1970-01-01", by = "month")[TimeIdx] 

nc_close(nc)

#3. Array transformations, calculations and good data selection
    
#Create quarterly catch and effort arrays (summing catch/effort matrix every 3 months)
quarter_eff <- array(NaN, c(dim(Eff)[1], dim(Eff)[2], dim(Eff)[3]/3)) #
    
l <- 1
for (k in seq(1,dim(Eff)[3], 3)) {
    for (i in seq(dim(Eff)[1])) {
      for (j in seq(dim(Eff)[2])) {
        if (all(is.na(Eff[i, j, k:(k+2)]))){
          quarter_eff[i, j, l] <- NA
        }
        else{
          quarter_eff[i, j, l] <- sum(Eff[i, j, k:(k+2)], na.rm=T)
        }
      }
    }
    l <- l + 1
}
    
saveRDS(quarter_eff, file = paste("data/1.3_graph_effort.Rda", sep=""))
    
#Change array resolution, 1x1degree to 5x5
if(degree == 5){
  x2_cp <- array(NaN, c(dim(quarter_eff)[1]/degree, dim(quarter_eff)[2]/degree, 
                        dim(quarter_eff)[3]))
    for (k in 1:dim(quarter_eff)[3]) {
      a<-1
      for (i in seq(1, dim(quarter_eff)[1], degree)) {
        b<-1
        for (j in seq(1, dim(quarter_eff)[2], degree)) { 
          if (all(is.na(quarter_eff[i:(i+(degree-1)), j:(j+(degree-1)), k]))){
            x2_cp[a, b, k] <- NA
          }
          else{
            x2_cp[a, b, k] <- sum(quarter_eff[i:(i+(degree-1)), j:(j+(degree-1)), k], na.rm = T)
          }
          b <- b + 1
        } 
        a <- a + 1
      }
    }
  quarter_eff <- x2_cp
}
    
#effort calculation
eff <- quarter_eff #effort from kg to tones / fishing hour
     
#Calculate seasonality (means by eff, mean through the whole dataset)
len<-1:dim(eff)[3]
na <- len[seq(1, length(len), 4)]
ne <- len[seq(2, length(len), 4)]
ni <- len[seq(3, length(len), 4)]
no <- len[seq(4, length(len), 4)]
jan <- rowMeans(eff[,,na], dims = 2, na.rm = T)
apr <- rowMeans(eff[,,ne], dims = 2, na.rm = T)
jul <- rowMeans(eff[,,ni], dims = 2, na.rm = T) 
oct <- rowMeans(eff[,,no], dims = 2, na.rm = T)
    
#Calculate anomalies (data - seasonality)
#Substract to eff matrix mean of "jan...", same substraction for every t
eff[,,na] <- sweep(eff[,,na], c(1,2), jan, "-") 
eff[,,ne] <- sweep(eff[,,ne], c(1,2), apr, "-")
eff[,,ni] <- sweep(eff[,,ni], c(1,2), jul, "-")
eff[,,no] <- sweep(eff[,,no], c(1,2), oct, "-")

    
#Create 2D effort matrix (1row = 1eff), S form
eff_m <- matrix(NaN, dim(eff)[3], dim(eff)[1]*dim(eff)[2]) 
for (value in 1:dim(eff)[3]) {
   eff_m[value,] <- as.vector(eff[ , , value]) #values in columns into rows
}
    
a <- eff_m
a[a>=0] <- 1 #effort >= 0 kg (0s are also good data)
a[a<0] <- 1 #negative values when substracting anomaly
a[which(is.finite(a) == F)] <- 0 #NA values are not good data
b <- colSums(a, na.rm = T) 
#sum column values (2D to 1D) to then determine good data series
    
i <- which(b > dim(eff_m)[1]*data.criteria) #good years selection
write.csv(i, row.names = F, #save googd positions
            file = "data/1.3_i.csv")
    
colnames(eff_m) <- c(1:dim(a)[2])
brut <- eff_m[ , i] #column selection with good data to use

brut2 <- scale(brut) #changing scale to see normality
    
iif <- which(is.finite(brut2) == F)#selecting positions of infinite and missing values
brut2[iif] <- NaN
    
write.csv(brut2, row.names = F, #Save S form matrix for EOF analysis
          file = "data/1.3_matrix.csv")