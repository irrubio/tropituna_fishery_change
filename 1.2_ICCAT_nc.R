#### Description: this script creates a catch and effort nc of tropical 
#### tuna purse seiners (PS) in the Atlantic Ocean from 1991 to 2017
#### INPUT: "1.1_ICCAT_data.csv"
#### OUTPUT: "1.2_PS_catch_effort.nc" tropical tuna catch and effort nc: 
#### 1991-2017, Atlatic Ocean, monthly data, 1x1, PS, tropical tunas
#### Date: 03/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(ncdf4)

#1.Read data
data <- read.csv("data/1.1_ICCAT_data.csv", header = T)

#2.Create a PS data 3D array (kg per 1x1 pixel)
maxY <- 2017
minY <- 1991
datei <- "1991/1/1"
datef <- "2017/12/1"
yy <- c(minY:maxY)
m <- 12
mmm <- c(1:m)
lat <- seq(from = -30, to = 29, by = 1) #study area for speed (idea for whole Atlantic)
lon <- seq(from = -35, to = 19, by = 1) #study area for speed (idea for whole Atlantic)
nlat <- length(lat) 
nlon <- length(lon)

YFTfd <- array(NaN, dim = c(nlon, nlat, (m*length(yy))))
BETfd <- array(NaN, dim = c(nlon, nlat, (m*length(yy))))
SKJfd <- array(NaN, dim = c(nlon, nlat, (m*length(yy))))
YFTfs <- array(NaN, dim = c(nlon, nlat, (m*length(yy))))
BETfs <- array(NaN, dim = c(nlon,nlat, (m*length(yy))))
SKJfs <- array(NaN, dim = c(nlon, nlat, (m*length(yy))))
Eff1 <- array(NaN, dim = c(nlon, nlat, (m*length(yy))))

t <- 1
for (year in yy) {
  for (month in mmm) {
    for (ilat in 1:nlat) {
      for (ilon in 1:nlon) {
        i <- which(data$TimePeriodID == month &
                   data$YearC == year & 
                   data$Lat >= lat[ilat] &
                   data$Lat < lat[ilat+1] &
                   data$Lon >= lon[ilon] &
                   data$Lon < lon[ilon+1]) 
        suYFTfd <- sum(data$YFTfd[i], na.rm=T)
        suBETfd <- sum(data$BETfd[i], na.rm=T)
        suSKJfd <- sum(data$SKJfd[i], na.rm=T)
        suYFTfs <- sum(data$YFTfs[i], na.rm=T)
        suBETfs <- sum(data$BETfs[i], na.rm=T)
        suSKJfs <- sum(data$SKJfs[i], na.rm=T)
        suEffort <- sum(data$Eff1[i], na.rm=T)
        
        if (length(i) == 0) { 
          suYFTfd <- NaN
          suBETfd <- NaN
          suSKJfd <- NaN
          suYFTfs <- NaN
          suBETfs <- NaN
          suSKJfs <- NaN
          suEffort <- NaN
        }
        
        YFTfd[ilon, ilat, t] <- suYFTfd 
        BETfd[ilon, ilat, t] <- suBETfd
        SKJfd[ilon, ilat, t] <- suSKJfd
        YFTfs[ilon, ilat, t] <- suYFTfs 
        BETfs[ilon, ilat, t] <- suBETfs
        SKJfs[ilon, ilat, t] <- suSKJfs
        Eff1[ilon, ilat, t] <- suEffort
        
      }
    }
    print(c(year, month, t))
    t <- t + 1
  }
}

YFTfd.PS <- YFTfd
BETfd.PS <- BETfd
SKJfd.PS <- SKJfd
YFTfs.PS <- YFTfs
BETfs.PS <- BETfs
SKJfs.PS <- SKJfs
Eff.PS <- Eff1
YFTfd.PS[is.nan(YFTfd.PS)] = 99999.
BETfd.PS[is.nan(BETfd.PS)] = 99999.
SKJfd.PS[is.nan(SKJfd.PS)] = 99999.
YFTfs.PS[is.nan(YFTfs.PS)] = 99999.
BETfs.PS[is.nan(BETfs.PS)] = 99999.
SKJfs.PS[is.nan(SKJfs.PS)] = 99999.
Eff.PS[is.nan(Eff.PS)] = 99999.

u <- seq(as.Date(datei), as.Date(datef), "month")

#3.Create and save a .nc
dim_x <- ncdim_def( name = "lon", units = "degree",
                    vals = as.double(lon))
dim_y <- ncdim_def( name = "lat", units = "degree", 
                    vals = as.double(lat))
dim_t <- ncdim_def( name = "time", units = "month", 
                    vals = as.double(u), unlim = TRUE)

def_YFTfdcatch <- ncvar_def(name = "YFTfd.catch", units ="kg", 
                          dim = list(dim_x,dim_y,dim_t), 
                          missval = 99999.)
def_BETfdcatch <- ncvar_def(name = "BETfd.catch", units = "kg", 
                          dim = list(dim_x, dim_y, dim_t), 
                          missval = 99999.)
def_SKJfdcatch <- ncvar_def(name = "SKJfd.catch", units = "kg", 
                          dim = list(dim_x, dim_y, dim_t), 
                          missval = 99999.)
def_YFTfscatch <- ncvar_def(name = "YFTfs.catch", units = "kg", 
                          dim = list(dim_x, dim_y, dim_t), 
                          missval = 99999.)
def_BETfscatch <- ncvar_def(name = "BETfs.catch", units = "kg", 
                          dim = list(dim_x, dim_y, dim_t), 
                          missval = 99999.)
def_SKJfscatch <- ncvar_def(name = "SKJfs.catch", units = "kg", 
                          dim = list(dim_x, dim_y, dim_t), 
                          missval = 99999.)
def_Eff <- ncvar_def(name = "Effort", units = "fish.hour", 
                     dim = list(dim_x, dim_y, dim_t), 
                     missval = 99999.)

ncnew <- nc_create(filename = "data/1.2_PS_catch_effort.nc",
                   vars = list(def_YFTfdcatch, def_BETfdcatch, 
                               def_SKJfdcatch, def_YFTfscatch, 
                               def_BETfscatch, def_SKJfscatch, 
                               def_Eff),
                   verbose = FALSE)

ncvar_put(nc = ncnew, varid = def_YFTfdcatch, vals = YFTfd.PS)
ncvar_put(nc = ncnew, varid = def_BETfdcatch, vals = BETfd.PS)
ncvar_put(nc = ncnew, varid = def_SKJfdcatch, vals = SKJfd.PS)
ncvar_put(nc = ncnew, varid = def_YFTfscatch, vals = YFTfs.PS)
ncvar_put(nc = ncnew, varid = def_BETfscatch, vals = BETfs.PS)
ncvar_put(nc = ncnew, varid = def_SKJfscatch, vals = SKJfs.PS)
ncvar_put(nc = ncnew, varid = def_Eff, vals = Eff.PS)

nc_close(ncnew)