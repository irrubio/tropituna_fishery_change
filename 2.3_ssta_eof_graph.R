#### Description: this script creates a pdf with the EOF results for
#### ssta
#### INPUT: -"2.1_matrix_ssta_eof.csv"
####        -"1.3_i.csv"
####        -"1.3_matrix.csv"
####        -"1.4_dineof.csv"
####        -"1.5_EOF.rda"
####        -"1.5_PC.rda"
#### OUTPUT: - "2.3_ssta_eof_graph.pdf" pdf with EOF results for ssta
####         - "2.3_sst_PCx.csv" (x2)
#### Date: 12/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(reshape2) #melt function
library(plyr) #count function
library(ggplot2)
library(maps) #ICCAT map
library(grid)
library(sinkr) #eof function
library(scales) #date_format function 
source("function_multiplot.R")

#1.Set variables
latitude1 <- -30 #study area
latitude2 <- 29 #study area
longitude1 <- -35 #study area
longitude2 <- 19 #study area
degree <- 5
xlim.eof <- c(-35,19)#c(-39,10)
ylim.eof <- c(-30,29)#c(-5,24)
y1 <- 1991
y2 <- 2017

#2.Read data
ssta <- read.csv("data/2.1b_matrix_ssta_eof.csv", header = T)

i <- read.csv("data/1.3_i.csv", header = T)[,1]
effort <- read.csv("data/1.4_dineof.csv")
nombrecol <- read.csv("data/1.3_matrix.csv")
colnames(effort) <- paste(colnames(nombrecol))

c1 <- colnames(ssta)
c2 <- colnames(effort)
data <- ssta[which(c1 %in% c2 == T)] #select ssta pixels = efforta pixels

#3.EOF analysis
c <- eof(data, scaled = F)

PoV <-round((c$Lambda/sum(c$Lambda))*100, 3) #total EOF variance (%)
PCplot <- list()
EOFplot <- list()

pdf("paper_figures/2.3_ssta_eof_graph.pdf")
for (e in 1:4) { #include results only for EOF 1 and 2
  #3.1.PC plot (temporal)
  PC.raw <- c$A[, e] #save principal components result from eof()

  PC <- as.data.frame(scale(PC.raw))
  colnames(PC)<-"PC"
  
  time <- seq(as.Date(paste(y1, "-01-01", sep = "")), 
              as.Date(paste(y2, "-12-01", sep = "")), 
              by = "months")
  PC$year <- as.character(time[seq(1, length(time), 3)])

  #xSeqLabels <- substr(PC$year, 1, 4) #labels for ggplot
  #xSeqLabels[c(rep(F, 1), rep(T, 3))] <- '' #blank label every 3 t
  
  PC$PC <- PC$PC*-1 #sign change for easier interpretation
  
  write.csv(PC[, 1], row.names = F, #save PC
            file = paste("data/2.3_ssta_PC", e, ".csv", sep = ""))
  
  PCplot[[e]] <- ggplot(PC, aes(as.Date(year), PC, group = 1)) + #plot PC
                 geom_line() +
                 geom_point(size = 1) +
                 geom_hline(yintercept = 0, linetype = "dashed", color = 'grey') +
                 #scale_x_discrete(labels = xSeqLabels, 
                 #                 breaks = PC$year) +
                 scale_x_date(breaks = seq(as.Date("1991/01/01"), 
                              as.Date(paste(y2, "/10/01", sep = "")),
                              "2 years"),
                             labels = date_format("%Y"),
                             expand = c(0,0)) +
                 theme_bw()+
                 theme(plot.background = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line.x = element_line(color = "black", size = 0.5),
                       axis.line.y = element_line(color = "black", size = 0.5),
                       axis.text.x = element_text(angle = 270, hjust = 0, 
                                                  vjust = 0.3, size = 17),
                       axis.text.y = element_text(size = 17),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 17)) +
                 labs(y = paste(e, "PC -","SSTA"))
  
  #3.2.EOF plot (spatial)
  eof <- c$u[, e] #save eof (eigenvector) result from eof()
  eof.raw <- c$u

  #calculations for local variance (see Ganzedo et al. 2013)
  eo <- list()
  lambdas <- (c$Lambda^2)/dim(data)[1] # singular values, /n (temporal=27y*4s=108)
  
  for (i2 in 1:dim(eof.raw)[2]) {
    eo[[i2]] <- eof.raw[, i2]
  }
  suma <- 0
  for (w in 1:length(eo)) {
    suma <- suma + (eo[[w]]^2)*lambdas[w]
  }
  
  var.frac <- (((eof^2)*lambdas[e])/suma)*100 #fraction of variance by pixel
  
  #preparing data for ploting
  dim1 <- length(longitude1:longitude2)/degree
  dim2 <- length(latitude1:latitude2)/degree
  
  mask11 <- matrix(NaN, dim1*dim2)
  mask11[i] <- var.frac #save the local variance in the corresponding positions
    
  mask <- mask11
  mask[i] <- eof #save eof value in the corresponding positions
  
  EOF11 <- matrix(mask, dim1, dim2, byrow = F) #for plot, needs 1mask for eof value by pixel
  EOF12 <- matrix(mask11, dim1, dim2, byrow = F) #for plot, needs 1mask for variance value by pixel
    
  colnames(EOF11) <- seq(latitude1, latitude2, degree)
  rownames(EOF11) <- seq(longitude1, longitude2, degree)
  longData<-melt(EOF11, na.rm = T)
  names(longData)[1] <- "lon"
  names(longData)[2] <- "lat"
  longData$lon <- longData$lon+degree/2
  longData$lat <- longData$lat+degree/2
    
  colnames(EOF12) <- seq(latitude1, latitude2, degree)
  rownames(EOF12) <- seq(longitude1, longitude2, degree)
  longData2 <- melt(EOF12, na.rm = T)

  longData <- cbind(longData,longData2$value)
  longData[,4] <-round(longData[, 4], 0)
  colnames(longData)[4] <-c ("value2")
  
  longData$value <- longData$value*-1 #sign change for easier interpretation
  
  longData$brea <- cut(longData$value, breaks = c(-Inf,-0.2,-0.1,0,0.1,0.2,+Inf))
    
  ICCAT <- map_data("world")
    
  EOFplot[[e]] <- ggplot(longData, aes(x = lon, y = lat)) + 
                    geom_raster(aes(fill = brea)) +
                    geom_text(aes(label = value2), colour = "white", size = 3.5, fontface = "bold") +
                    scale_fill_manual(values = c("(-Inf,-0.2]" = "black",
                                                 "(-0.2,-0.1]" = "dodgerblue4",
                                                 "(-0.1,0]" = "turquoise3",
                                                 "(0,0.1]" = "lightpink",
                                                 "(0.1,0.2]" = "hotpink1",
                                                 "(0.2, Inf]" = "deeppink2"),
                                      drop = FALSE,
                                      guide = guide_legend(reverse = TRUE))+
                    labs(x="lon (ºE)", y="lat (ºN)", title= paste("(a)",e, "EOF", "-", "SSTA -", round(PoV[e], 0),
                                                                   "%")) +
                    theme_bw() + theme(axis.text.x = element_text(size = 17),
                                       axis.text.y = element_text(size = 17),
                                       plot.title = element_text(size = 17),
                                       legend.key.height = unit(0.7, "cm"),
                                       legend.key.width = unit(0.5, "cm"),
                                       legend.title = element_blank(),
                                       legend.text = element_text(size = 12),
                                       axis.title = element_text(size = 17)) +
                    coord_quickmap(xlim = xlim.eof, ylim = ylim.eof) + 
                    geom_polygon(data = ICCAT, aes(long, lat, group = group), 
                                 fill = "grey")
  
  multiplot(PCplot[[e]], EOFplot[[e]], cols = 1)
  
  #PAPER FIGURES
  if(e == 1){
    png(file = "paper_figures_submitted/Figure_3a.png", 
        width = 10, height = 5, units = 'in', res = 600)
    multiplot(EOFplot[[e]], PCplot[[e]], cols = 2)
    dev.off()
    pl_EOF <- EOFplot[[e]]
    pl_PC <- PCplot[[e]]
  }
  ##############
  
}
dev.off()

#PAPER FIGURES
load(file = "data/1.5_EOF.rda")
load(file = "data/1.5_PC.rda")

png(file = "paper_figures_submitted/Figure_3.png", 
    width = 16, height = 7, units = 'in', res = 600)
multiplot(pl_EOF, EOFplot[[1]], pl_PC, PCplot[[1]], cols = 2)
dev.off()