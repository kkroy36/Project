rm(list=ls()) #clear workspace
data <- na.omit(read.csv("data.csv")) #read data
data[,1] <- NULL #remove false ordering