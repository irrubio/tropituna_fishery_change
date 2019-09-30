#### Description: this script creates a pdf including the evolution of
#### ssta (with lm fitted) and effort over time and a total effort matrix
#### INPUT: -"2.1a_matrix_ssta_evol.csv"
####        -"1.3_graph_effort.rda"
#### OUTPUT: -"2.2_ssta_evolution_graph.pdf" pdf with graphs 
####         -"2.2_effort_total.csv" total effort by species and fishing 
####          mode, quarterly, from 1x1
#### Date: 10/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(lubridate)
library(broom)
library(modelr) #add_predictions/predictors
library(ggplot2)
library(scales) #date_format function
source("function_multiplot.R")

#1. Set variables
y1 <- 1991
y2 <- 2017

#2.Read data
matr <- read.csv("data/2.1a_matrix_ssta_evol.csv", header = T,
                 sep=",", dec = ".", na.strings = "NA", 
                 stringsAsFactors = F)

matr$time<-c(1:nrow(matr)) #Add time column

#3.Fit a lm model
fit <- lm(ssta ~ time, data = matr)
summary(fit)

dat <- matr %>% #add predictions and residuals to the data
  add_predictions(fit) %>% 
  add_residuals(fit) 
  
round(mean(dat$ssta), 1) #Calculate ssta mean to check if it's zero
round(dim(matr)[1]*fit[["coefficients"]][2],2) #Calculate degrees increased from 1856

##4.Plot predictions
thm <- theme(plot.background = element_blank(), #Set a theme for ggplot
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             axis.line.x = element_line(color = "black", size = 0.5),
             axis.line.y = element_line(color = "black", size = 0.5),
             axis.text.x = element_text(angle = 270, hjust = 0, 
                                        vjust = 0.3, size = 18),
             axis.text.y = element_text(size = 18),
             axis.title.x = element_blank(),
             axis.title = element_text(size = 18),
             plot.title = element_text(size = 18))

dat$t <- as.Date(dat$t, origin = "1856-01-01", by = "month")

g1 <- ggplot(dat,aes(t, ssta))+
        scale_x_date(breaks = seq(as.Date("1856/01/01"), 
                                  as.Date(paste(y2,"/10/01", sep = "")),
                                  "10 years"),
                     labels = date_format("%Y"),
                     expand = c(0,0)) +
        labs(y = "SST anomaly (Cº)")+
        ggtitle("(a)") +
        theme_bw()+
        thm +
        geom_point()+
        geom_line()+ 
        geom_line(data = dat[which(dat$t >= paste(y1, "-01-01", sep = "")), ], 
                  aes(t ,ssta), color = "red")+
        geom_point(data = dat[which(dat$t >= paste(y1, "-01-01", sep = "")), ],
                  aes(t, ssta), color = "red")+
        geom_line(aes(y = pred), col = 'blue', size = 1)

##5.Plot residuals
g2 <- dat %>% 
        ggplot(aes(resid)) + geom_histogram()

#6.Create a matrix to graph efforts with ssta evolution (total)
dat2 <- dat[which(dat$t >= paste(y1,"/01/01",sep="")),]
dat2$eff<-0
sum <- c()

quarter_eff <- readRDS(paste("data/1.3_graph_effort.rda", sep = ""))
matr_sp_raw <- matrix(NA, nrow = dim(quarter_eff)[3], ncol = 1)  
for (x in 1:dim(quarter_eff)[3]) {
  matr_sp_raw[x] <- sum(quarter_eff[, , x], na.rm = T) 
}
dat2[, 6] <- matr_sp_raw
  
dat2$t<-as.Date(dat2$t, origin = paste(y1,"-01-01", sep=""), by ="month")
sum(dat2$eff)

save <- dat2[, 6]
colnames(save) <- "eff"

write.csv(save, row.names = F, file = "data/2.2_effort_total.csv")

#7.Plots
#anotations
thm <- theme(axis.line.x = element_line(colour = c("black")),
             axis.line.y.left = element_line(color = "black"), #,size = 0.5
             #axis.ticks.y.left = element_line(size = 0.5),
             panel.background = element_rect(fill = 'white'),
             axis.title.x = element_blank(),
             axis.text.x = element_text(angle = 270, hjust = 0, 
                                        vjust = 0.3, size = 18),
             axis.text.y = element_text(size = 18),
             axis.title = element_text(size = 18),
             plot.title = element_text(size = 18))
           
g3 <- ggplot(dat2, aes(t, eff)) +  
        geom_line(col = 'black') + 
        ggtitle("(b)") +
        scale_x_date(breaks = seq(as.Date("1856/01/01"), 
                                  as.Date(paste(y2, "/10/01", sep = "")),
                                  "2 years"),
                     labels = date_format("%Y"),
                     expand = c(0,0)) +
        labs(y = "Total effort (hour)") +
        thm

pdf("paper_figures/2.2_ssta_evolution_graph.pdf")
multiplot(g1,g2,cols = 1)
multiplot(g3, 0)
dev.off()

#PAPER FIGURES
png(file = "paper_figures_submitted/Figure_1a.png", 
    width = 8, height = 5, units = 'in', res = 600)
plot(g1)
dev.off()

png(file = "paper_figures_submitted/Figure_1b.png", 
    width = 8, height = 5, units = 'in', res = 600)
plot(g3)
dev.off()

source("function_multiplot.R")
png(file = "paper_figures_submitted/Figure_1.png", 
    width = 18, height = 5, units = 'in', res = 600)
multiplot(g1, g3, cols = 2)
dev.off()