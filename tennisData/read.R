#clear workspace
rm(list=ls())

#read all data from 2007 to 2015
d2007 <- na.omit(read.csv("atp_matches_2007.csv"))
d2008 <- na.omit(read.csv("atp_matches_2008.csv"))
d2009 <- na.omit(read.csv("atp_matches_2009.csv"))
d2010 <- na.omit(read.csv("atp_matches_2010.csv"))
d2011 <- na.omit(read.csv("atp_matches_2011.csv"))
d2012 <- na.omit(read.csv("atp_matches_2012.csv"))
d2013 <- na.omit(read.csv("atp_matches_2013.csv"))
d2014 <- na.omit(read.csv("atp_matches_2014.csv"))
d2015 <- na.omit(read.csv("atp_matches_2015.csv"))



#merge all data sets
djok <- rbind(d2007,d2008,d2009,d2010,d2011,d2012,d2013,d2014,d2015)
djokW <- sqldf("select * from djok where winner_name='Novak Djokovic'")
djokW[,"Result"] = rep(c(1),237)
djokL <- sqldf("select * from djok where loser_name = 'Novak Djokovic'")
djokL[,"Result"] = rep(c(0),length(djokL[,1]))
djok <- rbind(djokW,djokL)
indices <- sample.int(313,313)
djok <- djok[indices,]