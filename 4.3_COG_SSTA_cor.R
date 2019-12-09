#correlation between COG and SSTA

library(tidyverse)
library(corrplot)#for corplots
library(Hmisc)#for corrplot function
library(pracma) #detrend function
library(modelr) #add_predictions/predictors

####1.2) LOAD CPUE_COG data
COGN <- read.csv("data/4_lat_COG_N.csv")
COGS <- read.csv("data/4_lat_COG_S.csv")


data <- cbind(COGN[,3], COGS[,3])
colnames(data) <- c("COG_N", "COG_S")


sstaraw <- read.csv("data/2.1a_matrix_ssta_evol.csv", header = T)

sstaraw$t <- as.Date(sstaraw$t)
ssta <- sstaraw[which(sstaraw$t >= paste(1991,"/01/01",sep="")),]


data <- cbind(ssta[, 2], data)
colnames(data)[1] <- "ssta"
data <- as.data.frame(data)

corr <- rcorr(as.matrix(data))
M <- round(corr$r, 1) #cor values
p_mat <- corr$P #p_values

corrplot(M, tl.cex =0.7, cl.cex = 0.7, p.mat = p_mat, sig.level = 0.05,
         addCoef.col = "black", number.cex = 0.7, type = "upper") #corplot.mixed

data$year <- c(1991:2017)
data$col <- "blue"
data$col[data$year >= 2008] <- "red"

valcol <- data$col

plot(data$COG_S, data$ssta, 
     xlab = "COGS", ylab = "ssta", col = valcol)

# fit <- lm(COG_S ~ ssta, data = data)
# print(summary(fit))
# dat <- data %>%
#   add_predictions(fit) %>%
#   add_residuals(fit)
# ## plot predictions
# dat %>%
#   ggplot(aes(ssta, COG_S)) + geom_point() +#geom_line() +
#   geom_line(aes(y = pred), col = 'blue')+
#   labs(title = paste("pvalue=", summary(fit)$coefficients[2, 4]))
# ## plot residuals
# dat %>%
#   ggplot(aes(resid)) + geom_histogram()