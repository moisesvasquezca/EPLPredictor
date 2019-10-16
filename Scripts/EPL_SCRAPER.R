
# Libraries
#install.packages("rvest")
library(rvest)
library(tidyverse)
library(purrr)
library(magrittr)

#Teams

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

aveHomeGoals <- numeric(nrow(epl))
dfrange <- c(1:nrow(epl))
for (i in dfrange) {
  aveGOALS <- epl %>% filter(Date < epl[i,]$Date) %>% filter(Home == epl[i,]$Home | Away == epl[i,]$Home)
  aveGOALS <- head(aveGOALS[ order(aveGOALS$Date, decreasing = TRUE ),],5)
  aveGOALS <- sum(aveGOALS$HomeGoals[aveGOALS$Home == epl[i,]$Home]) + sum(aveGOALS$AwayGoals[aveGOALS$Away == epl[i,]$Home])
  aveHomeGoals[i]<- aveGOALS/5
}
aveHomeGoals <- data.frame(aveHomeGoals = aveHomeGoals)

aveAwayGoals <- numeric(nrow(epl))
dfrange <- c(1:nrow(epl))
for (i in dfrange) {
  aveGOALS <- epl %>% filter(Date < epl[i,]$Date) %>% filter(Home == epl[i,]$Away | Away == epl[i,]$Away)
  aveGOALS <- head(aveGOALS[ order(aveGOALS$Date, decreasing = TRUE ),],5)
  aveGOALS <- sum(aveGOALS$HomeGoals[aveGOALS$Home == epl[i,]$Away]) + sum(aveGOALS$AwayGoals[aveGOALS$Away == epl[i,]$Away])
  aveAwayGoals[i]<- aveGOALS/5
}
aveAwayGoals <- data.frame(aveAwayGoals = aveAwayGoals)

aveAwayPoints <- numeric(nrow(epl))
dfrange <- c(1:nrow(epl))
for (i in dfrange) {
  avePOINTS <- epl %>% filter(Date < epl[i,]$Date) %>% filter(Home == epl[i,]$Away | Away == epl[i,]$Away)
  avePOINTS <- head(avePOINTS[ order(avePOINTS$Date, decreasing = TRUE ),],5)
  avePOINTS <- sum(avePOINTS$HomePoints[avePOINTS$Home == epl[i,]$Away]) + sum(avePOINTS$AwayPoints[avePOINTS$Away == epl[i,]$Away])
  aveAwayPoints[i]<- avePOINTS/5
}
aveAwayPoints <- data.frame(aveAwayPoints = aveAwayPoints)

aveHomePoints <- numeric(nrow(epl))
dfrange <- c(1:nrow(epl))
for (i in dfrange) {
  avePOINTS <- epl %>% filter(Date < epl[i,]$Date) %>% filter(Home == epl[i,]$Home | Away == epl[i,]$Home)
  avePOINTS <- head(avePOINTS[ order(avePOINTS$Date, decreasing = TRUE ),],5)
  avePOINTS <- sum(avePOINTS$HomePoints[avePOINTS$Home == epl[i,]$Home]) + sum(avePOINTS$AwayPoints[avePOINTS$Away == epl[i,]$Home])
  aveHomePoints[i]<- avePOINTS/5
}
aveHomePoints <- data.frame(aveHomePoints = aveHomePoints)

aveAwayPoints <- numeric(nrow(epl))
dfrange <- c(1:nrow(epl))
for (i in dfrange) {
  avePOINTS <- epl %>% filter(Date < epl[i,]$Date) %>% filter(Home == epl[i,]$Away | Away == epl[i,]$Away)
  avePOINTS <- head(avePOINTS[ order(avePOINTS$Date, decreasing = TRUE ),],5)
  avePOINTS <- sum(avePOINTS$HomePoints[avePOINTS$Home == epl[i,]$Away]) + sum(avePOINTS$AwayPoints[avePOINTS$Away == epl[i,]$Away])
  aveAwayPoints[i]<- avePOINTS/5
}
aveAwayPoints <- data.frame(aveAwayPoints = aveAwayPoints)

match <- cbind(epl,aveHomeGoals,aveAwayGoals,aveHomePoints,aveAwayPoints)

#match <- aveEPL2019
i = 1
while( i < nrow(match)+1){
  if(match$Home[i] == "Man City"){
    match$Home[i] <- "Manchester City"
  }  
  
  if(match$Away[i] == "Man City"){
    match$Away[i] <- "Manchester City"
  }  
  
  if(match$Home[i] == "Wolves"){
    match$Home[i] <- "Wolverhampton Wanderer"
  }  
  
  if(match$Away[i] == "Wolves"){
    match$Away[i] <- "Wolverhampton Wanderer"
  }  
  
  if(match$Home[i] == "Leicester"){
    match$Home[i] <- "Leicester City"
  }  
  
  if(match$Away[i] == "Leicester"){
    match$Away[i] <- "Leicester City"
  }
  if(match$Home[i] == "Manchester Utd"){
    match$Home[i] <- "Manchester United"
  }  
  
  if(match$Away[i] == "Manchester Utd"){
    match$Away[i] <- "Manchester United"
  }
  
  if(match$Home[i] == "West Ham"){
    match$Home[i] <- "West Ham United"
  }  
  
  if(match$Away[i] == "West Ham"){
    match$Away[i] <- "West Ham United"
  }
  if(match$Home[i] == "Tottenham"){
    match$Home[i] <- "Tottenham Hotspur"
  }  
  
  if(match$Away[i] == "Tottenham"){
    match$Away[i] <- "Tottenham Hotspur"
  }
  if(match$Home[i] == "Brighton"){
    match$Home[i] <- "Brighton and Hove Albion"
  }  
  
  if(match$Away[i] == "Brighton"){
    match$Away[i] <- "Brighton and Hove Albion"
  }
  if(match$Home[i] == "Newcastle Utd"){
    match$Home[i] <- "Newcastle United"
  }  
  
  if(match$Away[i] == "Newcastle Utd"){
    match$Away[i] <- "Newcastle United"
  }
  if(match$Home[i] == "Sheffield Utd"){
    match$Home[i] <- "Sheffield United"
  }  
  
  if(match$Away[i] == "Sheffield Utd"){
    match$Away[i] <- "Sheffield United"
  }
  
  
  i <- i +1
}

#Players
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


players <- players %>% select(Player,Pos,Min,Gls,Ast,Sh,SoT,Fls,Fld,Crs,Tkl,Int,CrdY,CrdR,GA,Saves,`Save%`,Team,Time)
players[is.na(players)] <- 0
players$Date <- as.Date(players$Time, format = "%A %B %d, %Y")

# Merging Part

Hlast5PlayerAveMin <- as.numeric(nrow(match))
Hlast5PlayerAveMin <- as.numeric(nrow(match))
Hlast5PlayerAveGls <- as.numeric(nrow(match))
Hlast5PlayerAveAst <- as.numeric(nrow(match))
Hlast5PlayerAveSh <- as.numeric(nrow(match))
Hlast5PlayerAveSoT <- as.numeric(nrow(match))
Hlast5PlayerAveFls <- as.numeric(nrow(match))
Hlast5PlayerAveFld <- as.numeric(nrow(match))
Hlast5PlayerAveCrs <- as.numeric(nrow(match))
Hlast5PlayerAveTkl <- as.numeric(nrow(match))
Hlast5PlayerAveInt <- as.numeric(nrow(match))
Hlast5PlayerAveCrdY <- as.numeric(nrow(match))
Hlast5PlayerAveCrdR <- as.numeric(nrow(match))
Hlast5PlayerAveGA <- as.numeric(nrow(match))
Hlast5PlayerAveSaves <- as.numeric(nrow(match))
`Hlast5PlayerAveSave%` <- as.numeric(nrow(match))
for(i in c(1:nrow(match))){
  samplePlayers <- players %>% filter(Date < match[i,]$Date) %>% filter(Team == match[i,]$Home)
  samplePlayers <- head(samplePlayers[ order(samplePlayers$Date, decreasing = TRUE ),],5)
  thisPlayer <- samplePlayers %>% group_by(Player,Team) %>% summarize(aveMin = mean(Min),aveGls = mean(Gls),aveAst = mean(Ast),aveSh = mean(Sh),aveSoT = mean(SoT),aveFls = mean(Fls),aveFld = mean(Fld),aveCrs = mean(Crs),aveTkl = mean(Tkl),aveInt = mean(Int),aveCrdY = mean(CrdY),aveCrdR = mean(CrdR),aveGA = mean(GA),aveSaves = mean(Saves),`aveSave%` = mean(`Save%`))
  Hlast5PlayerAveMin[i] <- mean(thisPlayer$aveMin)
  Hlast5PlayerAveGls[i] <- mean(thisPlayer$aveGls)
  Hlast5PlayerAveAst[i] <- mean(thisPlayer$aveAst)
  Hlast5PlayerAveSh[i] <- mean(thisPlayer$aveSh)
  Hlast5PlayerAveSoT[i] <- mean(thisPlayer$aveSoT)
  Hlast5PlayerAveFls[i] <- mean(thisPlayer$aveFls)
  Hlast5PlayerAveFld[i] <- mean(thisPlayer$aveFld)
  Hlast5PlayerAveCrs[i] <- mean(thisPlayer$aveCrs)
  Hlast5PlayerAveTkl[i] <- mean(thisPlayer$aveTkl)
  Hlast5PlayerAveInt[i] <- mean(thisPlayer$aveInt)
  Hlast5PlayerAveCrdY[i] <- mean(thisPlayer$aveCrdY)
  Hlast5PlayerAveCrdR[i] <- mean(thisPlayer$aveCrdR)
  Hlast5PlayerAveGA[i] <- mean(thisPlayer$aveGA)
  Hlast5PlayerAveSaves[i] <- mean(thisPlayer$aveSaves)
  `Hlast5PlayerAveSave%`[i] <- mean(thisPlayer$`aveSave%`)
}

HomeAveMin <- data.frame(HomeAveMin = Hlast5PlayerAveMin)
HomeAveMin[HomeAveMin == "NaN"] <- 0
HomeAveGls <- data.frame(HomeAveGls = Hlast5PlayerAveGls)
HomeAveGls[HomeAveGls == "NaN"] <- 0
HomeAveAst <- data.frame(HomeAveAst = Hlast5PlayerAveAst)
HomeAveAst[HomeAveAst == "NaN"] <- 0
HomeAveSh <- data.frame(HomeAveSh = Hlast5PlayerAveSh)
HomeAveSh[HomeAveSh == "NaN"] <- 0
HomeAveSoT <- data.frame(HomeAveSoT = Hlast5PlayerAveSoT)
HomeAveSoT[HomeAveSoT == "NaN"] <- 0
HomeAveFls <- data.frame(HomeAveFls = Hlast5PlayerAveFls)
HomeAveFls[HomeAveFls == "NaN"] <- 0
HomeAveFld <- data.frame(HomeAveFld = Hlast5PlayerAveFld)
HomeAveFld[HomeAveFld == "NaN"] <- 0
HomeAveCrs <- data.frame(HomeAveCrs = Hlast5PlayerAveCrs)
HomeAveCrs[HomeAveCrs == "NaN"] <- 0
HomeAveTkl <- data.frame(HomeAveTkl = Hlast5PlayerAveTkl)
HomeAveTkl[HomeAveTkl == "NaN"] <- 0
HomeAveInt <- data.frame(HomeAveInt = Hlast5PlayerAveInt)
HomeAveInt[HomeAveInt == "NaN"] <- 0
HomeAveCrdY <- data.frame(HomeAveCrdY = Hlast5PlayerAveCrdY)
HomeAveCrdY[HomeAveCrdY == "NaN"] <- 0
HomeAveCrdR <- data.frame(HomeAveCrdR = Hlast5PlayerAveCrdR)
HomeAveCrdR[HomeAveCrdR == "NaN"] <- 0
HomeAveGA <- data.frame(HomeAveGA = Hlast5PlayerAveGA)
HomeAveGA[HomeAveGA == "NaN"] <- 0
HomeAveSaves <- data.frame(HomeAveSaves = Hlast5PlayerAveSaves)
HomeAveSaves[HomeAveSaves == "NaN"] <- 0
`HomeAveSave%` <- data.frame(`HomeAveSave%` = `Hlast5PlayerAveSave%`)
`HomeAveSave%`[`HomeAveSave%` == "NaN"] <- 0

Alast5PlayerAveMin <- as.numeric(nrow(match))
Alast5PlayerAveMin <- as.numeric(nrow(match))
Alast5PlayerAveGls <- as.numeric(nrow(match))
Alast5PlayerAveAst <- as.numeric(nrow(match))
Alast5PlayerAveSh <- as.numeric(nrow(match))
Alast5PlayerAveSoT <- as.numeric(nrow(match))
Alast5PlayerAveFls <- as.numeric(nrow(match))
Alast5PlayerAveFld <- as.numeric(nrow(match))
Alast5PlayerAveCrs <- as.numeric(nrow(match))
Alast5PlayerAveTkl <- as.numeric(nrow(match))
Alast5PlayerAveInt <- as.numeric(nrow(match))
Alast5PlayerAveCrdY <- as.numeric(nrow(match))
Alast5PlayerAveCrdR <- as.numeric(nrow(match))
Alast5PlayerAveGA <- as.numeric(nrow(match))
Alast5PlayerAveSaves <- as.numeric(nrow(match))
`Alast5PlayerAveSave%` <- as.numeric(nrow(match))
for(i in c(1:nrow(match))){
  samplePlayers <- players %>% filter(Date < match[i,]$Date) %>% filter(Team == match[i,]$Away)
  samplePlayers <- head(samplePlayers[ order(samplePlayers$Date, decreasing = TRUE ),],5)
  thisPlayer <- samplePlayers %>% group_by(Player,Team) %>% summarize(aveMin = mean(Min),aveGls = mean(Gls),aveAst = mean(Ast),aveSh = mean(Sh),aveSoT = mean(SoT),aveFls = mean(Fls),aveFld = mean(Fld),aveCrs = mean(Crs),aveTkl = mean(Tkl),aveInt = mean(Int),aveCrdY = mean(CrdY),aveCrdR = mean(CrdR),aveGA = mean(GA),aveSaves = mean(Saves),`aveSave%` = mean(`Save%`))
  Alast5PlayerAveMin[i] <- mean(thisPlayer$aveMin)
  Alast5PlayerAveGls[i] <- mean(thisPlayer$aveGls)
  Alast5PlayerAveAst[i] <- mean(thisPlayer$aveAst)
  Alast5PlayerAveSh[i] <- mean(thisPlayer$aveSh)
  Alast5PlayerAveSoT[i] <- mean(thisPlayer$aveSoT)
  Alast5PlayerAveFls[i] <- mean(thisPlayer$aveFls)
  Alast5PlayerAveFld[i] <- mean(thisPlayer$aveFld)
  Alast5PlayerAveCrs[i] <- mean(thisPlayer$aveCrs)
  Alast5PlayerAveTkl[i] <- mean(thisPlayer$aveTkl)
  Alast5PlayerAveInt[i] <- mean(thisPlayer$aveInt)
  Alast5PlayerAveCrdY[i] <- mean(thisPlayer$aveCrdY)
  Alast5PlayerAveCrdR[i] <- mean(thisPlayer$aveCrdR)
  Alast5PlayerAveGA[i] <- mean(thisPlayer$aveGA)
  Alast5PlayerAveSaves[i] <- mean(thisPlayer$aveSaves)
  `Alast5PlayerAveSave%`[i] <- mean(thisPlayer$`aveSave%`)
}

AwayAveMin <- data.frame(AwayAveMin = Alast5PlayerAveMin)
AwayAveMin[AwayAveMin == "NaN"] <- 0
AwayAveGls <- data.frame(AwayAveGls = Alast5PlayerAveGls)
AwayAveGls[AwayAveGls == "NaN"] <- 0
AwayAveAst <- data.frame(AwayAveAst = Alast5PlayerAveAst)
AwayAveAst[AwayAveAst == "NaN"] <- 0
AwayAveSh <- data.frame(AwayAveSh = Alast5PlayerAveSh)
AwayAveSh[AwayAveSh == "NaN"] <- 0
AwayAveSoT <- data.frame(AwayAveSoT = Alast5PlayerAveSoT)
AwayAveSoT[AwayAveSoT == "NaN"] <- 0
AwayAveFls <- data.frame(AwayAveFls = Alast5PlayerAveFls)
AwayAveFls[AwayAveFls == "NaN"] <- 0
AwayAveFld <- data.frame(AwayAveFld = Alast5PlayerAveFld)
AwayAveFld[AwayAveFld == "NaN"] <- 0
AwayAveCrs <- data.frame(AwayAveCrs = Alast5PlayerAveCrs)
AwayAveCrs[AwayAveCrs == "NaN"] <- 0
AwayAveTkl <- data.frame(AwayAveTkl = Alast5PlayerAveTkl)
AwayAveTkl[AwayAveTkl == "NaN"] <- 0
AwayAveInt <- data.frame(AwayAveInt = Alast5PlayerAveInt)
AwayAveInt[AwayAveInt == "NaN"] <- 0
AwayAveCrdY <- data.frame(AwayAveCrdY = Alast5PlayerAveCrdY)
AwayAveCrdY[AwayAveCrdY == "NaN"] <- 0
AwayAveCrdR <- data.frame(AwayAveCrdR = Alast5PlayerAveCrdR)
AwayAveCrdR[AwayAveCrdR == "NaN"] <- 0
AwayAveGA <- data.frame(AwayAveGA = Alast5PlayerAveGA)
AwayAveGA[AwayAveGA == "NaN"] <- 0
AwayAveSaves <- data.frame(AwayAveSaves = Alast5PlayerAveSaves)
AwayAveSaves[AwayAveSaves == "NaN"] <- 0
`AwayAveSave%` <- data.frame(`AwayAveSave%` = `Alast5PlayerAveSave%`)
`AwayAveSave%`[`AwayAveSave%` == "NaN"] <- 0

matchAll <- cbind(match,HomeAveMin,
                  HomeAveGls, 
                  HomeAveAst, 
                  HomeAveSh, 
                  HomeAveSoT, 
                  HomeAveFls, 
                  HomeAveFld, 
                  HomeAveCrs, 
                  HomeAveTkl, 
                  HomeAveInt, 
                  HomeAveCrdY, 
                  HomeAveCrdR, 
                  HomeAveGA, 
                  HomeAveSaves, 
                  `HomeAveSave%`,
                  AwayAveMin, 
                  AwayAveGls, 
                  AwayAveAst, 
                  AwayAveSh, 
                  AwayAveSoT, 
                  AwayAveFls, 
                  AwayAveFld, 
                  AwayAveCrs, 
                  AwayAveTkl, 
                  AwayAveInt, 
                  AwayAveCrdY, 
                  AwayAveCrdR,
                  AwayAveGA,
                  AwayAveSaves, 
                  `AwayAveSave%`)

write.csv(matchAll,"epl2019ave.csv")
