
str(y)

#Here I recover the geometry of the list
ni <- seq_along(y[[3]])
nj <- seq_along(y[[c(3, 1, 2)]])
nij <- as.matrix(expand.grid(3, ni=ni, 2, nj=nj))

#then extract the relevant variable information using the rows of nij 
#as an index into the nested list

stat <- apply(cbind(nij[,1:2], 2), 1, function(ij) y[[ij]])
id <- apply(cbind(nij[,1:2],1), 1, function(ij) y[[ij]])

#and make it into a more friendly structure

dat <- data.frame(id, do.call(rbind, stat))
#s <- dat[1:13,] or s <- head(dat, 13)
#dat <- data.frame(id, stat)

# aR <- data.frame(lapply(archieR, as.numeric)
                # cR <- data.frame(lapply(charles, as.numeric))
                 
#champs
#champs <- getChampion()
#y <- champs
#dat <- data.frame(id, stat)
#champs <- dat
#sandch <- merge(sand, champs, by = "id")
#or library(plyr)   sandch <- join(sand, ch, type="inner")


#sandy["name"] <- "El Sandyman"
#> archieR["name"] <- "Archie Falls"
#> charlesR["name"] <- "Carlitito"
#> lolr <- rbind(sandyR, archieR, charlesR)
#lolranked <- lolr[, c("name", "stat", "totalChampionKills", "totalDamageDealt", "totalDamageTaken", "totalMinionKills", "normalGamesPlayed", "rankedSoloGamesPlayed")]
