rm(list=ls())
source("read.R")
djok <- djok[,c(32:50)]

#given players stat what is the probability of winning against djokovic??
#perform logisitc regression
model <- glm(Result ~.,family=binomial(link='logit'),data=djok)
y <- as.matrix(djok[1:18])%*%as.matrix(model$coefficients[2:19]) #y <- w*x
y <- y + model$coefficients[1] #subtract intercept b i.e. y <- w*x + b
sigma <- function(x) return(1/(1+exp(-x))) #calculate probability of y as sigma(y)
p <- sigma(y) #this is the probability of djokovic winning given the training data
accuracy <- (1 - sum(abs(p - djok$Result))/length(djok$Result)) * 100
