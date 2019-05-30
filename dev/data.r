library(RCurl)
library(rjson)
library(stringr)
library(rgexf)

myKey <- paste(readLines("riotkey.txt"), collapse=" ")
options(riot.key = myKey)

getRiotKey <- function() {
  key <- getOption("riot.key")
  if (is.null(key))
    stop("getOption(\"riot.key\") returned NULL.")
  key
}
