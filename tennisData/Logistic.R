rm(list=ls()) #clear workspace
y <- readline("Enter which year: ") #read year for analysis from user
ty <- paste("atp_matches_",y,".csv",sep="") 
ly <- paste("atp_matches_",as.integer(y)-1,".csv",sep="")
tyd <- na.omit(read.csv(ty)) #this year data
lyd <- na.omit(read.csv(ly)) #last year data
p1 <- readline("Enter player 1: ") #get player 1 name from user
p2 <- readline("Enter player 2: ") #get player 2 name from user
players <- c(p1,p2) #list of players 
h2hly <- lyd[which(lyd$winner_name %in% players & lyd$loser_name %in% players),] #get head to head for last year
h2hty <- tyd[which(tyd$winner_name %in% players & tyd$loser_name %in% players),] #get head to head for this year
classVect <- as.numeric(h2hly$winner_name == p1) #binary class vector 1 means won, 0 means lost
dataly <- h2hly[32:49] #data last year with only match stats
dataly[,"result"] <- classVect #binary class vector appended
model <- glm(result~.,family=binomial(link="logit"),data=dataly) #perform logistic regression 
w <- model$coefficients[!is.na(model$coefficients)] #gather weights
n <- length(w) #length of weight vector
what <- as.matrix(w[2:n]) #weight vector minus the intercept
dataty <- as.matrix(h2hty[32:(32+n-2)]) #this year data with only match stats
sigmoid <- function(x) return(1/(1+exp(-x))) #sigmoid definition
yhat <- (dataty%*%what) + w[1] #estimated probability of player 1 winning
probs <- as.numeric(sigmoid(yhat) > 0.5) #bin to 0,1, 1 means player one wins and 0 means otherwise.