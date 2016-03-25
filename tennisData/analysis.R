rm(list=ls())
source("read.R")
djok <- djok[,c(32:50)]

#given players stat what is the probability of winning against djokovic??
#perform logisitc regression
model <- glm(Result ~.,family=binomial(link='logit'),data=djok)
y <- as.matrix(djok[1:18])%*%as.matrix(model$coefficients[2:19])
y <- y - 0.59037852
sigma <- function(x) return(1/(1+exp(-x)))
p <- sigma(y)