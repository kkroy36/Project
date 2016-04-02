rm(list=ls()) #clear workspace
y <- readline("Enter which year: ") #read year for analysis from user
ty <- paste("atp_matches_",y,".csv",sep="") 
ly <- paste("atp_matches_",as.integer(y)-1,".csv",sep="")
tyd <- na.omit(read.csv(ty)) #this year data
lyd <- na.omit(read.csv(ly)) #last year data
p1 <- readline("Enter player 1: ") #get player 1 name from user
p2 <- readline("Enter player 2: ") #get player 2 name from user
players <- c(p1,p2) #list of players
h2hly <- lyd[which(lyd$winner_name %in% players & lyd$loser_name %in% players),c(11,21)] #get head to head for last year
h2hty <- tyd[which(tyd$winner_name %in% players & tyd$loser_name %in% players),c(11,21)] #get head to head for this year
a <- dim(subset(h2hly,winner_name==p1))[1] #how many times player 1 won
b <- dim(h2hly)[1] - a #how many times player 2 won
probsMLE <- c(a/(a+b),b/(a+b)) #probability of each player winning by MLE
probsMAP <- c((a-1)/(a+b-2),(b-1)/(a+b-2)) #probability of each player winning by MAP
probsMSE <- probsMLE #probability of each player winning by MSE
n <- dim(h2hty)[1] #number of rows in this years data
wc <- c(0,0) #vector of win counts this year
pwMLE <- players[which.max(probsMLE)] #predicted Winner MLE
pwMAP <- players[which.max(probsMAP)] #predicted Winner MAP
pwMSE <- players[which.max(probsMSE)] #predicted Winner MSE
winnersMLE <- NULL
winnersMAP <- NULL
winnersMSE <- NULL
winners <- NULL
par(mfrow=c(1,4)) #partition plotting window into 1 row and 4 columns
for (i in 1:n){
	winnersMLE <- append(winnersMLE,which.max(probsMLE))
	winnersMAP <- append(winnersMAP,which.max(probsMAP))
	winnersMSE <- append(winnersMSE,which.max(probsMSE))
	winners <- append(winners,match(h2hty$winner[i],players))
	plot(winnersMLE,col="red",cex=2,pch=16) #plot and print predicted winners as per each method
	plot(winnersMAP,col="red",cex=2,pch=16)
	plot(winnersMSE,col="red",cex=2,pch=16)
	plot(winners,col="green",cex=2,pch=16)
	print(paste("MATCH NUMBER",i))
	print(paste("Predicted Winner MLE: ",pwMLE))
	print(paste("Predicted Winner MAP: ",pwMAP))
	print(paste("Predicted Winner MSE: ",pwMSE))
	print(paste("Actual Winner: ",h2hty$winner[i]))
	readline("Press Enter for next match info..")
	print("--------------------")
	if (winners[i] == p1) wc[1] <- wc[1] + 1
	else wc[2] <- wc[2] + 1
	a <- a + wc[1] #recalculate paramters of Beta distribution
	b <- b + wc[1]
	probsMLE <- c(wc[1]/sum(wc),wc[2]/sum(wc))
	probsMAP <- c((a-1)/(a+b-2),(b-1)/(a+b-2))
	probsMSE <- c(a/(a+b),b/(a+b))
	pwMLE <- players[which.max(probsMLE)] #predicted Winner MLE
	pwMAP <- players[which.max(probsMAP)] #predicted Winner MAP
	pwMSE <- players[which.max(probsMSE)] #predicted Winner MSE
}
print(paste("Error MLE: ",sum(abs(winners-winnersMLE)*100/n),"%")) #calculate error rate for each prediction
print(paste("Error MAP: ",sum(abs(winners-winnersMAP)*100/n),"%"))
print(paste("Error MSE: ",sum(abs(winners-winnersMSE)*100/n),"%")) 