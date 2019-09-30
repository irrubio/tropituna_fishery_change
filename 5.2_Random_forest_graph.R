#### Description: this script creates a pdf with Random Forest results 
#### INPUT: "5.1_data_long.csv" and "1.5_effort_PC1.csv"
#### OUTPUT: "5.2_Random_forest.pdf" 
#### Date: 20/09/2018
#### Author: Iratxe Rubio
#####################################################################

library(car) #for VIFs
library(tidyverse)
library(randomForest)
source("function_partialPlot2.R")

#1.Read data

data <- read.csv("data/5.1_data_long.csv")[,-c(15,16,18,19)]

pc1efforta <- read.csv("data/1.5_effort_PC1.csv", header = T)
colnames(pc1efforta) <- "pc1efforta"
data <- cbind(data, pc1efforta)

data$TAC <- as.factor(data$TAC)
data$closure <- as.factor(data$closure)
data$quarter <- as.factor(data$quarter)


######RF with PC1 effortA
#2.Check collinearity
v <- lm(pc1efforta ~.-year-t-t2-t3-effort-efforta-COG_N-COG_S, data)
as.data.frame(vif(v))

set.seed(1000)
#3.RANDOM FOREST
rf <- randomForest(pc1efforta ~.-year-t-t2-t3-effort-efforta-COG_N-COG_S
                   , data, ntree = 1000, importance = T
                   ); rf #na.action = na.omit

pdf("paper_figures/5_Random_forest_pc1effortA.pdf", width = 8, height = 4)
varImpPlot(rf, main = "PC1 effortA ", type = 1 )
plot(rf, main = paste("Pseudo R2 (",round(rf[["rsq"]][1000], 2),")"))

#varUsed(rf1)
imp <- as.data.frame(importance(rf))

partialPlot(rf, data, agr_vessel, xlab = "Number of vessels allowed by SFPAs") 
partialPlot2(rf, data, SSTA, xlab = "SSTA (ºC)")
partialPlot2(rf, data, TAC, xlab = "TAC presence (1) or absence (0)")

dev.off()

#PAPER FIGURES
png(file = "paper_figures_submitted/Figure_4a.png", 
    width = 7, height = 5, units = 'in', res = 600)
varImpPlot(rf, main = "(a) PC1 effortA", type = 1 )
dev.off()

png(file = "paper_figures_submitted/Figure_5a.png", 
    width = 10, height = 3, units = 'in', res = 600)
par(mfrow = c(1, 3))
partialPlot(rf, data, agr_vessel, 
            main = "(a) Partial Dependence on agr_vessel",
            xlab = "Number of vessels allowed by SFPAs") 
partialPlot2(rf, data, SSTA, 
             main = "Partial Dependence on SSTA",
             xlab = "SSTA (ºC)")
partialPlot2(rf, data, TAC, 
             main = "Partial Dependence on TAC",
             xlab = "TAC presence (1) or absence (0)")
dev.off()


######RF with effort
#4.Check collinearity
v <- lm(effort ~.-year-t-t2-t3-pc1efforta-efforta-COG_N-COG_S, data)
as.data.frame(vif(v))

set.seed(1000)
#5.RANDOM FOREST
rf_e <- randomForest(effort ~.-year-t-t2-t3-pc1efforta-efforta-COG_N-COG_S
                   , data, ntree = 1000, importance = T
); rf_e #na.action = na.omit

pdf("paper_figures/5_Random_forest_effort.pdf", width = 8, height = 4)
varImpPlot(rf_e, main = "Effort", type = 1 )
plot(rf_e, main = paste("Pseudo R2 (",round(rf_e[["rsq"]][1000], 2),")"))

#varUsed(rf1)
imp <- as.data.frame(importance(rf_e))

partialPlot(rf_e, data, FAD_prop, xlab = "Proportion of catch on FADs (%)") 
partialPlot(rf_e, data, agr_vessel, xlab = "Number of vessels allowed by SFPAs")

dev.off()

#PAPER FIGURES
png(file = "paper_figures_submitted/Figure_4b.png", 
    width = 7, height = 5, units = 'in', res = 600)
varImpPlot(rf_e, main = "(b) Effort", type = 1 )
dev.off()

png(file = "paper_figures_submitted/Figure_5b.png", 
    width = 10, height = 3, units = 'in', res = 600)
par(mfrow = c(1, 3))
partialPlot(rf_e, data, FAD_prop, 
            main = "(b) Partial Dependence on FAD_prop",
            xlab = "Proportion of catch on FADs (%)") 
partialPlot(rf_e, data, agr_vessel, 
            main = "Partial Dependence on agr_vessel",
            xlab = "Number of vessels allowed by SFPAs") 
dev.off()

png(file = "paper_figures_submitted/Figure_4.png", 
    width = 11, height = 5, units = 'in', res = 600)
par(mfrow = c(1, 2))
varImpPlot(rf, main = "(a) PC1 effortA", type = 1 )
varImpPlot(rf_e, main = "(b) effort", type = 1 )
dev.off()

png(file = "paper_figures_submitted/Figure_5.png", 
    width = 11, height = 5, units = 'in', res = 600)
par(mfrow = c(2, 3))
partialPlot(rf, data, agr_vessel, 
            main = "(a) Partial Dependence on agr_vessel",
            xlab = "Number of vessels allowed by SFPAs") 
partialPlot2(rf, data, SSTA, 
             main = "(a) Partial Dependence on SSTA",
             xlab = "SSTA (ºC)")
partialPlot2(rf, data, TAC, 
             main = "(a) Partial Dependence on TAC",
             xlab = "TAC presence (1) or absence (0)")
partialPlot(rf_e, data, FAD_prop, 
            main = "(b) Partial Dependence on FAD_prop",
            xlab = "Proportion of catch on FADs (%)") 
partialPlot(rf_e, data, agr_vessel, 
            main = "(b) Partial Dependence on agr_vessel",
            xlab = "Number of vessels allowed by SFPAs") 
dev.off()