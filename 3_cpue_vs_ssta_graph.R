#### Description: this script creates a pdf showing correlations between
#### PCs of ssta and efforta from the EOF analysis
#### INPUT: -"1.5_effort_PCx.csv" (x4)
####        -"2.3_sst_PCx.csv" (x4)
            -"2.2_effort_total.csv"
#### OUTPUT: "3_effort_vs_ssta_graph.pdf" 
#### Date: 13/07/2018
#### Author: Iratxe Rubio
#####################################################################

library(Hmisc) #function rcorr
library(corrplot)
library(utils) #glob2rx function
library(ggplot2)
library(gridExtra) #grid.table function
library(grid)
library(modelr) #add_predictions/predictors

#1.Read data
name <- list.files("data",pattern = "1.5_effort|2.3_ssta")
data.raw <- list()

for (pc in name) {
  data.raw[[pc]] <- read.csv(paste("data/", pc, sep = ""), header = T)
}

data_trend <- as.data.frame(matrix(unlist(data.raw), nrow = dim(data.raw[[1]])[1]))
colnames(data_trend) <- substr(name, 5, 19)

data <- as.data.frame(data_trend)
colnames(data) <- substr(name,5,19)

df <- read.csv("data/2.2_effort_total.csv", header = T)

data <- cbind(data,df)

#2.temporal correlation
corr <- rcorr(as.matrix(data))
M <- round(corr$r, 1) #cor values
p_mat <- corr$P #p_values

#Select all correlations of ssta with other variables and make a table to see them
i <- which(p_mat > 0.05, arr.ind = TRUE) #non significant values
M2 <- M #correlation matrix
M2[i] <- NA #replace non significan correlations by NAs
M2[M2 == 1] <- NA #replace perfect correlations by NAs (the diagonal)

#keep significant values
#select columns from M2 that contain ssta in the column name
M3 <- M2[!grepl(glob2rx('ssta*'), colnames(M2)), grepl(glob2rx('ssta*'), colnames(M2))]
keep.columns <- which(colSums(!is.na(M3)) > 0) #delete column with all its values = NAs
keep.rows <- which(rowSums(!is.na(M3)) > 0) #keep rows without NAs
M4 <- M3[keep.rows, keep.columns, drop = F] #keep significant correlations with ssta

pdf("paper_figures/3_effort_vs_ssta_graph.pdf",width=10,height=7)

corrplot(M, tl.cex =0.7, cl.cex = 0.7, p.mat = p_mat, sig.level = 0.05,
         addCoef.col = "black", number.cex = 0.7, type = "upper") #corplot.mixed

if (dim(M4)[1] > 0) {
  grid.newpage()
  grid.table(M4)
  
  fit <- lm(effort_PC1.csv ~ ssta_PC1.csv, data = data)
  summary(fit)
  
  dat <- data %>% #add predictions and residuals to the data
    add_predictions(fit) %>% 
    add_residuals(fit) 
  
  thm <- theme(plot.background = element_blank(), #Set a theme for ggplot
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               axis.line.x = element_line(color = "black", size = 0.5),
               axis.line.y = element_line(color = "black", size = 0.5),
               axis.text.x = element_text(angle = 270, hjust = 0, 
                                          vjust = 0.3, size = 11),
               axis.text.y = element_text(size = 11))
  dat$YearC <-  rep(c(1991:2017), each = 4)
    
  plot(ggplot(dat, aes(ssta_PC1.csv, effort_PC1.csv)) +
        labs(y = "efforta PC1") +
        labs(x = "ssta PC1") +
        #ggtitle("(a)") +
        theme_bw()+
        thm +
        geom_point() +
        geom_point(data = dat[which(dat$YearC >= 2000), ],
                   aes(ssta_PC1.csv, effort_PC1.csv), color = "red", size = 3) +
        geom_point(data = dat[which(dat$YearC < 2000), ],
                   aes(ssta_PC1.csv, effort_PC1.csv), color = "blue", size = 3) +
        geom_line(aes(y = pred), col = 'black', size = 1)
  )
  dat %>% 
    ggplot(aes(resid)) + geom_histogram()
  
}

dev.off()
