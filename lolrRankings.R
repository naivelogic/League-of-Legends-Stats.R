sR <- getlolr(sandy, sR, champs, "El Sandyman")
aR <- getlolr(archie, aR, champs, "Archie Falls")
cR <- getlolr(charles, cR, champs, "Carlitito")
tR <- getlolr(twiggy, tR, champs, "Twiggy")
kR <- getlolr(kobalt, kR, champs, "Kobalt")
lolr <- rbind(sR, aR, cR, tR, kR)

library(reshape2)
library(ggplot2)
library(plyr) 

#lol to lol.m and KD and WinRate 
lolr.m <- ddply(lolr, c("name", "champions"), transform, KD = round(mean((mostChampionKillsPerSession /totalDeathsPerSession), na.rm = TRUE),2), WinRate = round(mean((totalSessionsWon /totalSessionsPlayed), na.rm = TRUE),2))

lolr.m <- arrange(lolr.m, desc(KD), desc(WinRate)) #Column Ordering

# Rankings by KD/Kills/CS

        lolr.r <- ddply(lolr.m, c("name", "Position"), summarise, KD.R = round(mean(KD, na.rm = TRUE),2), Kills.R = round(mean((totalChampionKills/totalSessionsPlayed), na.rm = TRUE),2), CS.R = round(mean((totalMinionKills/totalSessionsPlayed), na.rm = TRUE),2))
        lolr.r <- arrange(lolr.r, desc(KD.R), desc(Kills.R), desc(CS.R))
        lolr.rankP <- ddply(lolr.r, c("Position"), head, n=2)
