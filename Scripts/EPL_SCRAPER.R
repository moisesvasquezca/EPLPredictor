
# Libraries
#install.packages("rvest")
library(rvest)
library(tidyverse)

# Get Resuts Table
url <- "https://fbref.com/en/comps/9/schedule/Premier-League-Fixtures"
epl2019 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="sched_ks_3232_1"]') %>%
  html_table()
epl2019 <- epl2019[[1]]

# Transform Data Frame

epl2019$Date<- as.Date(epl2019$Date)
epl <- epl2019[ , !duplicated(colnames(epl2019))]
epl <- epl %>% filter(Date < Sys.Date()) %>% select(Wk,Day,Date,Time,Home,Score,Away)
epl$HomeGoals <- as.numeric(substr(epl$Score,1,1))
epl$AwayGoals <- as.numeric(substr(epl$Score,3,3))
epl$Result <- "D"
epl$Result[epl$HomeGoals > epl$AwayGoals] <- "H"
epl$Result[epl$HomeGoals < epl$AwayGoals] <- "A"
epl$HomePoints <- 1
epl$HomePoints[epl$HomeGoals > epl$AwayGoals] <- 3
epl$HomePoints[epl$HomeGoals < epl$AwayGoals] <- 0
epl$AwayPoints <- 1
epl$AwayPoints[epl$HomeGoals > epl$AwayGoals] <- 0
epl$AwayPoints[epl$HomeGoals < epl$AwayGoals] <- 3

# Get Average for Last 5 Games

aveHomeGoals <- numeric(nrow(sampleEPL))
dfrange <- c(1:nrow(sampleEPL))
for (i in dfrange) {
  aveGOALS <- sampleEPL %>% filter(Date < sampleEPL[i,]$Date) %>% filter(Home == sampleEPL[i,]$Home | Away == sampleEPL[i,]$Home)
  aveGOALS <- head(aveGOALS[ order(aveGOALS$Date, decreasing = TRUE ),],5)
  aveGOALS <- sum(aveGOALS$HomeGoals[aveGOALS$Home == sampleEPL[i,]$Home]) + sum(aveGOALS$AwayGoals[aveGOALS$Away == sampleEPL[i,]$Home])
  aveHomeGoals[i]<- aveGOALS/5
}
aveHomeGoals <- data.frame(aveHomeGoals = aveHomeGoals)

aveAwayGoals <- numeric(nrow(sampleEPL))
dfrange <- c(1:nrow(sampleEPL))
for (i in dfrange) {
  aveGOALS <- sampleEPL %>% filter(Date < sampleEPL[i,]$Date) %>% filter(Home == sampleEPL[i,]$Away | Away == sampleEPL[i,]$Away)
  aveGOALS <- head(aveGOALS[ order(aveGOALS$Date, decreasing = TRUE ),],5)
  aveGOALS <- sum(aveGOALS$HomeGoals[aveGOALS$Home == sampleEPL[i,]$Away]) + sum(aveGOALS$AwayGoals[aveGOALS$Away == sampleEPL[i,]$Away])
  aveAwayGoals[i]<- aveGOALS/5
}
aveAwayGoals <- data.frame(aveAwayGoals = aveAwayGoals)

aveAwayPoints <- numeric(nrow(sampleEPL))
dfrange <- c(1:nrow(sampleEPL))
for (i in dfrange) {
  avePOINTS <- sampleEPL %>% filter(Date < sampleEPL[i,]$Date) %>% filter(Home == sampleEPL[i,]$Away | Away == sampleEPL[i,]$Away)
  avePOINTS <- head(avePOINTS[ order(avePOINTS$Date, decreasing = TRUE ),],5)
  avePOINTS <- sum(avePOINTS$HomePoints[avePOINTS$Home == sampleEPL[i,]$Away]) + sum(avePOINTS$AwayPoints[avePOINTS$Away == sampleEPL[i,]$Away])
  aveAwayPoints[i]<- avePOINTS/5
}
aveAwayPoints <- data.frame(aveAwayPoints = aveAwayPoints)

aveHomePoints <- numeric(nrow(sampleEPL))
dfrange <- c(1:nrow(sampleEPL))
for (i in dfrange) {
  avePOINTS <- sampleEPL %>% filter(Date < sampleEPL[i,]$Date) %>% filter(Home == sampleEPL[i,]$Home | Away == sampleEPL[i,]$Home)
  avePOINTS <- head(avePOINTS[ order(avePOINTS$Date, decreasing = TRUE ),],5)
  avePOINTS <- sum(avePOINTS$HomePoints[avePOINTS$Home == sampleEPL[i,]$Home]) + sum(avePOINTS$AwayPoints[avePOINTS$Away == sampleEPL[i,]$Home])
  aveHomePoints[i]<- avePOINTS/5
}
aveHomePoints <- data.frame(aveHomePoints = aveHomePoints)

aveAwayPoints <- numeric(nrow(sampleEPL))
dfrange <- c(1:nrow(sampleEPL))
for (i in dfrange) {
  avePOINTS <- sampleEPL %>% filter(Date < sampleEPL[i,]$Date) %>% filter(Home == sampleEPL[i,]$Away | Away == sampleEPL[i,]$Away)
  avePOINTS <- head(avePOINTS[ order(avePOINTS$Date, decreasing = TRUE ),],5)
  avePOINTS <- sum(avePOINTS$HomePoints[avePOINTS$Home == sampleEPL[i,]$Away]) + sum(avePOINTS$AwayPoints[avePOINTS$Away == sampleEPL[i,]$Away])
  aveAwayPoints[i]<- avePOINTS/5
}
aveAwayPoints <- data.frame(aveAwayPoints = aveAwayPoints)

aveTeams <- cbind(epl,aveHomeGoals,aveAwayGoals,aveHomePoints,aveAwayPoints)

library(tidyverse)
library(rvest)
library(purrr)
library(magrittr)
urls <- read_table2(file = "Match List.txt",col_names  = FALSE) %>%
  .[[1]]

table_extract <- function(url){
  tb <- read_html(x = as.character(url),encoding = 'UTF-8') 
  tb_information <- tb %>%
    html_nodes('h1') %>%
    html_text() %>%
    str_replace_all(c('vs.' = '-','Match Report' = '-')) %>%
    str_replace_all(c('- –' = '-')) %>%
    str_split(pattern = "-") %>%
    unlist() %>%
    str_trim()
  
  tb_team <- tb_information[1:2]
  
  tb_time <- tb_information[3]
  
  
  tb_score <- tb %>%
    html_nodes('.score') %>%
    html_text() %>%
    as.numeric()
  
  tb_table <- tb %>%
    html_table(trim = TRUE) %>%
    .[4:7] %>%
    map( function(x){
      x$Nation <- x$Nation %>%
        str_extract_all('[A-Z]+') %>%
        unlist()
      return(x)
    }
    ) 
 
  tb_table_rbind <- tb_table %>% {
    rbind(full_join(x = .[[1]],y = .[[2]],by = c("Player","Nation","Min")) %>%
            mutate(Team = tb_team[1]),
          full_join(x = .[[3]],y = .[[4]],by = c("Player","Nation","Min")) %>%
            mutate(Team = tb_team[2])
    ) %>%
      mutate(Time = tb_time)
  }
  
  tb_title <- tb %>%
    html_nodes('caption') %>%
    html_text() 
  return(list(tb_score,tb_information,tb_table_rbind))
}

t1 <- Sys.time()
premiership <- lapply(X = 1:3, FUN = function(i){
  message("The ",i,"th",": ","elapsed time: ",system.time(table_result <- table_extract(url = urls[i]))[3])
  return(table_result)
} )

table_all <- premiership %>%
  map(function(x) x[[3]]) %>%
  bind_rows()

players <- table_all

# Missin Merging Part.
