#### Description: this script creates a reconstructed data matrix of 
#### effort anomaly (5x5) by DINEOF for tropical tunas
#### INPUT: "1.3_matrix.csv"
#### OUTPUT: - "1.4_dineof.csv" reconstructed effort data matrix
#### 1991-2017, East Atlatic Ocean, quarterly data, PS, tropical tunas
#### Date: 10/07/2018
#### Author: Iratxe Rubio
#####################################################################

#devtools::install_github("marchtaylor/sinkr")
library(sinkr)

#1.Read data
tuna <- read.csv("data/1.3_matrix.csv", 
                   header = T,
                   stringsAsFactors = F)
  
tuna <- as.matrix(tuna)

#2.Dineof reconstruction
RES <- dineof(tuna, delta.rms = 1e-02)
    
write.csv(RES$Xa, row.names=F, 
          file = "data/1.4_dineof.csv")
