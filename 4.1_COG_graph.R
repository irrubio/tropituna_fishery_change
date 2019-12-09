#### Description: this script calculates and plots the yearly center of gravity
#### of the PS fishery 1991-2017, 1x1, quarter and determines if it shifts
#### INPUT: "1.1_ICCAT_data.csv"
#### OUTPUT: "4_COG_graph.pdf" pdf with COG results
#### Date: 10/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(tidyverse)
library(ggridges)
library(lubridate)
library(broom)
library(modelr) #add_predictions/predictors
library(gridExtra)

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
  summarise(Eff = sum(Eff,na.rm = T))

pdf("paper_figures/4_COG_graph.pdf")
#effort (hour)

cp1 <- PS %>% 
          group_by(YearC, Lat) %>% 
          summarise(sumEff = sum(Eff)) %>% 
          mutate(effort = sumEff)#total effort per year and lat
  
cp1.N <- filter(cp1, Lat >= 0) 
cp1.S <- filter(cp1, Lat < 0)
  
cp0.N <- cp1.N %>% 
            group_by(YearC) %>%
            summarise(Lat = sum(effort*Lat, na.rm = TRUE)/sum(effort, na.rm = TRUE),#COG
                      effort = sum(effort)) 
            #calculation of effort to include variable in ggplot
            #COG called lat for graphical purpose in ggplot
  
cp0.S <- cp1.S %>% 
            group_by(YearC) %>%
            summarise(Lat = sum(effort*Lat, na.rm = TRUE)/sum(effort, na.rm = TRUE),#COG
                      effort = sum(effort)) 
            #calculation of effort to include variable in ggplot
            #COG called lat for graphical purpose in ggplot
  
cp1 <- filter(cp1, effort > 0)

lab1 <- paste(seq(0, 30, by = 5), "ºN", sep = "")
lab2 <- paste(seq(5, 25, by = 5), "ºS", sep = "")
lat_labels <- c(rev(lab2), lab1)

#6.Plot effort "distribution" by year and Lat COG
g0 <- ggplot(cp1, aes(x = Lat, y = as.factor(YearC), height = effort)) + 
          geom_density_ridges(stat = "identity", scale = 1, na.rm = T,
                              rel_min_height = 0.00001) +
          theme_ridges(font_size = 30) +#grid = TRUE
          ylab("") +
          xlab("latitude") +
          scale_x_continuous(limits = c(-25,latitude2), breaks = seq(-25, 30, 5),
                             labels = lat_labels) +
          theme(axis.text.x = element_text(angle = 270, hjust = 0, 
                                           vjust = 0.25, size = 11),
                axis.text.y = element_text(size = 11),
                axis.title = element_text(size = 11),
                axis.title.y = element_text(hjust = 0.5),
                plot.title = element_text(size = 11),
                panel.grid.major.x = element_blank()) +
          coord_flip()  +
          geom_point(data = cp0.N, aes(x = Lat, y = as.factor(YearC)), color = "blue") + #COG called lat
          geom_point(data = cp0.S, aes(x = Lat, y = as.factor(YearC)), color = "red") + #COG called lat
          geom_line(data = cp0.N, group = 1, color = "blue") +
          geom_line(data = cp0.S, group = 1, color = "red") +
          annotate("label", x = 27, y = as.factor(2002), label = "Northern COG",
                   size = 4, color = "blue") +
          annotate("label", x = -18, y = as.factor(2002), label = "Southern COG",
                   size = 4, color = "red") 
  
  plot(g0)
  
#write.csv(cp0.N, "data/4_COG_N.csv", row.names = F)
#write.csv(cp0.S, "data/4_COG_S.csv", row.names = F)
  
#7.STATS, analyse whether COG shifts N or S and if it is significant
fit.N <- lm(Lat ~ YearC, data = cp0.N)
print(summary(fit.N))
dat <- cp0.N %>% 
          add_predictions(fit.N) %>% 
          add_residuals(fit.N) 
  ## plot predictions
g1 <- dat %>%   
          ggplot(aes(YearC, Lat)) + geom_point() +
          geom_line() + geom_line(aes(y = pred), col = 'blue') +
          labs(title = paste("pvalue=", summary(fit.N)$coefficients[, 4]))
## plot residuals
g2 <- dat %>% 
          ggplot(aes(resid)) + geom_histogram()
  
plot(g1)
plot(g2)
  
fit.S <- lm(Lat ~ YearC, data = cp0.S)
print(summary(fit.S))
dat <- cp0.S %>% 
          add_predictions(fit.S) %>% 
          add_residuals(fit.S) 
## plot predictions
g3 <- dat %>%   
          ggplot(aes(YearC, Lat)) + geom_point() +
          geom_line() + geom_line(aes(y = pred), col = 'blue')+
          labs(title = paste("pvalue=", summary(fit.S)$coefficients[2, 4]))
## plot residuals
g4 <- dat %>% 
          ggplot(aes(resid)) + geom_histogram()
  
plot(g3)
plot(g4)

dev.off()
  
#PAPER FIGURES
png(file = "paper_figures_submitted/Figure_2.png", 
      width = 7, height = 5, units = 'in', res = 600)
plot(g0)
dev.off()
#######

#plotlist <- list()  
#plotlist[[1]] <- g0 

#PAPER FIGURES
#p <- grid.arrange(grobs = plotlist, ncol = 1)
#ggsave("paper_figures_submitted/Figure_2.png", plot = p,
#       width = 15, height = 7, units = 'in', dpi = 600)
#######