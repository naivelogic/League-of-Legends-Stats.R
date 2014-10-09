require(RCurl)
require(rjson)
require(stringr)
require(rgexf)
library(igraph)
library(RCurl)
library(rjson)

options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
options(riot.key= "0dc1dcba-4705-438b-b2d8-92215239a656") # PLoS  
getRiotKey <- function() {
        key <- getOption("riot.key")
        if (is.null(key))
                stop("getOption(\"riot.key\") returned NULL.")
        key
}

getRIOT <- function(version, base, type=NULL, id=NULL, attr=NULL, params=NULL, region="na", static=TRUE, parse=TRUE, ...) {
        if (static)
                uri <- sprintf("https://na.api.pvp.net/api/lol/static-data/%s", region)
        else
                uri <- sprintf("https://na.api.pvp.net/api/lol/%s", region)
        uri <- sprintf("%s/%s/%s",uri,version,base)
        if (!is.null(type))
                uri <- sprintf("%s/%s",uri,type)
        if (!is.null(id))
                uri <- sprintf("%s/%s",uri,id)
        if (!is.null(attr))
                uri <- sprintf("%s/%s",uri,attr)
        uri <- sprintf("%s?api_key=%s", uri, getRiotKey())
        if (!is.null(params))
                uri <- sprintf("%s%s",uri,params)
        j <- getURI(uri)
        if (parse)
                fromJSON(j)
        else
                j
}

getChampion <- function(id=NULL, static=TRUE) getRIOT(version="v1.2", base="champion", id=id, static=static)
getItem <- function(id=NULL) getRIOT(version="v1.2", base="item", id=id)
getMastery <- function(id=NULL) getRIOT(version="v1.2", base="mastery", id=id)
getRealm <- function() getRIOT(version="v1.2", base="realm")
getRune <- function(id=NULL) getRIOT(version="v1.2", base="rune", id=id)
getSummonerSpell <- function(id=NULL) getRIOT(version="v1.2", base="summoner-spell", attr=id)
getVersions <- function() getRIOT(version="v1.2", base="versions")

getTeam <- function(id, ...) getRIOT(version="v2.2", base="team", id=id, static=FALSE, ...)
getTeamBySummoner <- function(id, ...) getRIOT(version="v2.2", base="team/by-summoner", id=id, static=FALSE, ...)

getSummonerBySummonerID <- function(id, attr=NULL, ...) {
        if (!is.null(attr) && !(attr %in% c("masteries","name","runes")))
                stop("Invalid attr.")
        getRIOT(version="v1.4", base="summoner", id=id, static=FALSE, attr=attr, ...)
}
getSummonerByName <- function(summonerName, ...) getRIOT(version="v1.4", base="summoner/by-name", id=str_replace_all(tolower(summonerName)," ",""), static=FALSE, ...)

getStatsBySummonerID.Ranked <- function(id, ...) getRIOT(version="v1.3", base="stats/by-summoner", id=id, attr="ranked", static=FALSE, ...)
getStatsBySummonerID.Summary <- function(id, ...) getRIOT(version="v1.3", base="stats/by-summoner", id=id, attr="summary", static=FALSE, ...)

getLeagueBySummonerID <- function(id, ...) getRIOT(version="v2.3", base="league/by-summoner", id=id, static=FALSE, ...)
getLeagueEntryBySummonerID <- function(id, ...) getRIOT(version="v2.3", base="league/by-summoner", id=id, attr="entry", static=FALSE, ...)

getLeagueByTeamID <- function(id, ...) getRIOT(version="v2.3", base="league/by-team", id=id, static=FALSE, ...)
getLeagueEntryByTeamID <- function(id, ...) getRIOT(version="v2.3", base="league/by-team", id=id, attr="entry", static=FALSE, ...)

getLeagueChallenger <- function(type, ...) {
        if (type %in% c("RANKED_SOLO_5x5","RANKED_TEAM_3x3","RANKED_TEAM_5x5")) {
                challengers <- getRIOT(version="v2.3", base="league/challenger", static=FALSE, params=sprintf("&type=%s", type), ...)
                attr(challengers, "class") <- "riot_league_challengers"
                challengers
        } else
                NULL
}
fortify.riot_league_challengers <- function(data) {
        d <- do.call("rbind", lapply(data$entries, function(x) as.data.frame(t(x))))
        d$playerOrTeamId <- as.character(d$playerOrTeamId)
        d$playerOrTeamName <- as.character(d$playerOrTeamName)
        d$leagueName <- as.character(d$leagueName)
        d$queueType <- as.character(d$queueType)
        d$tier <- as.character(d$tier)
        d$rank <- as.character(d$rank)
        d$leaguePoints <- as.numeric(d$leaguePoints)
        d$wins <- as.numeric(d$wins)
        d$isHotStreak <- as.logical(d$isHotStreak)
        d$isVeteran <- as.logical(d$isVeteran)
        d$isFreshBlood <- as.logical(d$isFreshBlood)
        d$isInactive <- as.logical(d$isInactive)
        d$lastPlayed <- as.numeric(d$lastPlayed)
        d
}

getGameBySummonerID <- function(id, ...) getRIOT(version="v1.3", base="game/by-summoner", id=id, attr="recent", static=FALSE, ...)


challengers <- getLeagueChallenger("RANKED_TEAM_5x5")
graph <- graph.empty() + vertex("RANKED_TEAM_5x5", id="RANKED_TEAM_5x5", kind="league", txt=challengers$name)
for (team in challengers$entries) {
        cat(sprintf("%s\n",team$playerOrTeamId))
        graph <- graph + vertex(team$playerOrTeamId, id=team$playerOrTeamId, kind="team", txt=team$playerOrTeamName)
        graph <- graph + edge(team$playerOrTeamId, "RANKED_TEAM_5x5")
        teamInfo <- getTeam(team$playerOrTeamId)[[1]]
        for (member in teamInfo$roster$memberList) {
                summoner <- getSummonerBySummonerID(member$playerId)[[1]]
                id <- sprintf("%s+%s",team$playerOrTeamId,summoner$id)
                cat(sprintf("  %s %s\n",summoner$name, id))
                graph <- graph + vertex(id, id=summoner$id, kind="summoner", txt=summoner$name, level=summoner$summonerLevel)
                graph <- graph + edge(id, team$playerOrTeamId)
                Sys.sleep(1)
        }
}
print(igraph.to.gexf(graph), file="RANKED_TEAM_5x5.gexf", replace=T)