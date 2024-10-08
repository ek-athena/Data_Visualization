library(alluvial)
library(magrittr)
library(tidyverse)
library(dplyr)

data <- read.csv("E:/Github/Alluvial_plot/data_freq.csv")
fdata <- as.data.frame(data, stringsAsFactors = FALSE)

fdata %>% group_by(Source, ST, GPSC) %>% summarise(n = sum(Freq)) -> data3d
view(data3d)

alluvial(data3d[,1:3], freq=data3d$n)




alluvial(data3d[,1:3], 
         freq=data3d$n, 
         col = ifelse(data3d$Source == "Blood" | data3d$Source == "CSF", "lightpink", "lightblue"),
         border = "white", 
         alpha = 0.7, 
         blocks=TRUE,
         hide=data3d$n == 1)
