#### Description: this script downloads Task II Catch & Effort by operation
#### mode (FAD/FREE SCHOOL only tropicals) data from the ICCAT website
#### https://www.iccat.int/en/accesingdb.html, cleans it and creates a 
#### database for studying spatiotemporal catch and effort data of tropical 
#### tuna purse seiners in the Atlantic Ocean from 1991 to 2017
#### INPUT: "t2ce_PS91-17_bySchool.xlsx" [Excel; version 12/2018]
#### OUTPUT: "1.1_ICCAT_data.csv" tropical tuna catch and effort database:
#### 1991-2017, Atlatic Ocean, monthly data, 1x1, PS, tropical tunas
#### Date: 03/07/2018
#### Author: Iratxe Rubio
#######################################################################

library(tidyverse)
library(readxl) #read_xlsx function
library(sp) #spatial objects
library(rgdal) #readOgr
library(rgeos) # gBuffer, gOverlap

#1.Download Task II catch and effort by operation mode database 
#from the ICCAT website to data folder (accessed 20/12/2018)
#download.file(url = "https://www.iccat.int/Data/t2ce_PS91-17_bySchool.7z",
#              destfile = "data/ICCATdata_FAD_web.7z", 
#              mode     = "wb")

#2.Unzip the data manually!

#3.READ the data
table1 <- read_xlsx("data/t2ce_PS91-17_bySchool.xlsx", sheet = 1, skip = 6)

#4.Convert lat&lon to their corresponding sign (N+S-E+O-)
table <- table1
table$Lat <- case_when(
              table$QuadID == 1 ~  (table$Lat + 0.5),
              table$QuadID == 2 ~ -(table$Lat + 0.5),
              table$QuadID == 3 ~ -(table$Lat + 0.5),
              table$QuadID == 4 ~  (table$Lat + 0.5))

table$Lon <- case_when(
              table$QuadID == 1 ~  (table$Lon + 0.5),
              table$QuadID == 2 ~  (table$Lon + 0.5),
              table$QuadID == 3 ~ -(table$Lon + 0.5),
              table$QuadID == 4 ~ -(table$Lon + 0.5))

#5.Delete species we are not interested in
table$ALBfd <- NULL
table$BLFfd <- NULL
table$LTAfd <- NULL
table$FRIfd <- NULL
table$ALBfs <- NULL
table$BLFfs <- NULL
table$LTAfs <- NULL
table$FRIfs <- NULL
table$TOTfd <- NULL
table$TOTfs <- NULL

#6.Calcul percentage of EU PS in the dataset and study area
latitude1 <- -30
latitude2 <- 29
longitude1 <- -35
longitude2 <- 19

total <- table %>% #filtering data to keep our sudy area
          filter(Lat >= latitude1, 
                 Lat < latitude2, 
                 Lon >= longitude1, 
                 Lon < longitude2)

i <- grep("EU", unique(table$Flag)) #determine EU flagged fleets

euPS <- total %>% #new database on EU flags
        filter(Flag %in% unique(table$Flag)[i])

tot <- sum(total$BETfd, na.rm = T) + #total catch of tropical tunas in our study area
       sum(total$YFTfd, na.rm = T) + 
       sum(total$SKJfd, na.rm = T) +
       sum(total$BETfs, na.rm = T) +
       sum(total$YFTfs, na.rm = T) +
       sum(total$SKJfs, na.rm = T)

tot_BETfs <- round((sum(total$BETfs, na.rm = T)/tot)*100, 0) #catch proportions on FS/FAD by species
tot_YFTfs <- round((sum(total$YFTfs, na.rm = T)/tot)*100, 0)
tot_SKJfs <- round((sum(total$SKJfs, na.rm = T)/tot)*100, 0)
tot_BETfd <- round((sum(total$BETfd, na.rm = T)/tot)*100, 0)
tot_YFTfd <- round((sum(total$YFTfd, na.rm = T)/tot)*100, 0)
tot_SKJfd <- round((sum(total$SKJfd, na.rm = T)/tot)*100, 0)

round(sum(total$BETfs, na.rm = T)*10^-3, 0) #catch in tones on FS/FAD by species
round(sum(total$YFTfs, na.rm = T)*10^-3, 0)
round(sum(total$SKJfs, na.rm = T)*10^-3, 0)
round(sum(total$BETfd, na.rm = T)*10^-3, 0)
round(sum(total$YFTfd, na.rm = T)*10^-3, 0)
round(sum(total$SKJfd, na.rm = T)*10^-3, 0)

EU <- sum(euPS$BETfd, na.rm = T) + #total catch by EU flagged vessels in our study area
      sum(euPS$YFTfd, na.rm = T) +
      sum(euPS$SKJfd, na.rm = T) +
      sum(euPS$BETfs, na.rm = T) +
      sum(euPS$YFTfs, na.rm = T) +
      sum(euPS$SKJfs, na.rm = T)

round((EU*100)/tot, 0) #Result: percentage of EU PS in the dataset and study area

#7.Filter effort
table <- filter(table, Eff1Type == "FISH.HOUR") #effort: fishing hour

#8.Plot of catch points in our study area (MAP) and delete land points
ICCAT <- map_data("world")
ggplot(table, aes(Lon, Lat)) + 
    coord_quickmap(xlim = c(-35,19), ylim = c(-30,29)) +
    geom_polygon(data = ICCAT, aes(long, lat, group = group), fill = "grey") +
    geom_point(colour = "red", size = 0.3) 

p <- table #save table data into p and turn it into a SpatialPointsDataFrame
coordinates(p) <- c("Lon", "Lat")
# tell R that catch coordinates are in the same lat/lon reference system
# as the oceans data (for over function, both points and polygon need to have
#the same ref system)
proj4string(p) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#download oceans shape file into ocean folder inside data folder
#download.file(url = "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_ocean.zip",
#              destfile = "data/ocean/ocean.zip")
#unzip("data\\ocean\\ocean.zip", exdir="data/ocean")
oceans <- readOGR("data/ocean/ne_110m_ocean.shp",
                  stringsAsFactors=FALSE, verbose=FALSE)

# combine is.na() with over() to do the containment test; note that we
# need to "demote" oceans to a SpatialPolygons object first
inside.ocean <- !is.na(over(p, as(oceans, "SpatialPolygons")))

# use 'over' again, this time with oceans as a SpatialPolygonsDataFrame object,
# to determine which observation (if any) is outside the ocean, and
# store if the observation is ocean as an attribute of the "points" data
p$ok <- over(p, oceans)$featurecla

#draw a map
plot(coordinates(p), type = "n")
plot(oceans, border = "black", add = TRUE)
legend("topright", cex = 0.85,
       c("Points ocean", "Points land"),
       pch = c(16, 1),
       col = c("blue", "red"), bty = "n")
title(expression(paste(italic("Tunas"), ", onland data")))

#Now plot points with separate colors inside and outside of oceans
points(p[inside.ocean, ], pch = 16, col = "blue")
points(p[!inside.ocean, ], pch = 1, col = "red")
land <- p[!inside.ocean, ] #save points inside continent in an object
land <- SpatialPoints(land)

squares<-gBuffer(land, byid = T, capStyle = "SQUARE", width = 0.5)
#Create squares of 1x1 degrees around catch data
proj4string(squares)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(squares, add = T)

#Download the continents "shapefile" from
#https://www.arcgis.com/home/item.html?id=a3cb207855b348a297ab85261743351d      
#into "continent" folder
#unzip it with 7 zip in "continent" folder, extract v104
cont <- readOGR(dsn = "data/continent/v104/continent.gdb")
afr <- cont[cont$CONTINENT == "Africa",]#select Africa for case study

#visualizing intersection between land and squares (clip in blue)
plot(squares)
plot(afr, add = T)
plot(land,pch = 1,col="red",size=3, add = T)

i <- gOverlaps(afr, squares, byid = TRUE) #index including what points of the polygons overlap

true_land <- land[!i, ] #determine TRUE land points
plot(true_land,col = "blue", add = T)
land <- as.data.frame(true_land) #data.frame with coordinates of land points

p_land <- semi_join(table, land) #points land
tablenew <- anti_join(table, land) #save table without points inland

#Final map without points land
plot(tablenew %>% 
       ggplot(aes(Lon, Lat)) + #only change species!
       coord_quickmap(xlim = c(-35,19), ylim = c(-30,29)) +
       geom_polygon(data = ICCAT, aes(long, lat, group = group), fill = "grey")) +
       geom_point(colour = "red", size = 0.3) 

#8.Save the final "clean" database into data folder
write.csv(tablenew, row.names = F, file = "data/1.1_ICCAT_data.csv")