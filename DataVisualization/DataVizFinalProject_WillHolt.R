library(rvest)
library(ggplot2)



#GolfStat Function to scrape results from golfstat.com
golfstat <- function(site, tournament) {
  x <- read_html(site)
  y <- x %>% html_table(fill = TRUE)
  y <- as.data.frame(y)
  colnames(y) <- y[2,]
  y <- y[-c(1:2),-c(2,8:10,13:16)]
  y$Player <- gsub("Playoff:.*","",y$Player)
  y$Player <- gsub("Round:.*","",y$Player)
  y$Player <- gsub("Scores:.*","",y$Player)
  made_cut <- subset(y, !(y$Place %in% c("CUT", "WD", "DQ")))
  made_cut$Finish <- gsub("T","",made_cut$Place)
  made_cut$Finish <- gsub("Win",1,made_cut$Finish)
  made_cut <- made_cut[which(made_cut$R1 !="E"),]
  made_cut <- made_cut[which(made_cut$R2 !="E"),]
  made_cut <- made_cut[which(made_cut$R3 !="E"),]
  made_cut <- made_cut[which(made_cut$R4 !="E"),]
  made_cut <- subset(made_cut, made_cut$R1 %in% c(58:120))
  made_cut$Finish <- as.numeric(made_cut$Finish)
  missed_cut <- subset(y, y$Place %in% "CUT")
  missed_cut$Finish <- rank(missed_cut$`Total Score`, ties.method = "min")+nrow(made_cut)
  y_final <- rbind(made_cut,missed_cut)
  colnames(y_final)[8] <- "TotalScore"
  cols <- names(y_final)[3:9]
  y_final$ToPar <- gsub("E",0,y_final$ToPar)
  y_final$R4 <- gsub("E",0,y_final$R4)
  y_final[cols] <- lapply(y_final[cols], as.numeric)
  y_final <- as.data.frame(y_final)  
  a <- as.data.frame(table(y_final$Finish))
  colnames(a) <- c("Finish", "Count")
  b <- merge(y_final, a, by.x = "Finish", by.y = "Finish")
  b$Count <- b$Count - 1
  b$Wins <- nrow(b) - b$Finish - b$Count
  b$Ties <- b$Count
  b$Losses <- (nrow(b)-1) - b$Wins - b$Ties
  b <- b[,c(2:9,1,11,13,12)]
  c <- b
  c$TWplay <- ifelse(c$Player == "Tiger Woods", 1, 0)
  c$TWwin <- ifelse(c$TWplay == 0, c$Finish < c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWLoss <- ifelse(c$TWplay == 0, c$Finish > c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWTie <- ifelse(c$TWplay == 0, c$Finish == c[which(c$Player == "Tiger Woods"),9],0)
  d <- merge(b,c)
  d[is.na(d)] <- 0
  d <- d[order(d$Finish),]
  e <- data.frame(Tournament = tournament)
  final <- cbind(d,e)
}

#Match play function to extract results specifically for match play
matchplay <- function(site, tournament) {
  x <- read_html(site)
  y <- x %>% html_table(fill = TRUE)
  y <- as.data.frame(y)
  colnames(y) <- y[2,]
  y <- y[-c(1:2),-c(2,8:10,13:16)]
  y$Player <- gsub("Group.*","",y$Player)
  y$Player <- gsub("Round.*","",y$Player)
  y <- subset(y, !(y$Place %in% c("CUT", "WD", "DQ")))
  y$Finish <- gsub("T","",y$Place)
  y$Finish <- gsub("Win",1,y$Finish)
  y <- y[which(y$R1 !="E"),]
  y <- y[which(y$R2 !="E"),]
  y <- y[which(y$R3 !="E"),]
  y <- y[which(y$R4 !="E"),]
  y$Finish <- as.numeric(y$Finish)
  colnames(y)[8] <- "TotalScore"
  cols <- names(y)[3:9]
  y$ToPar <- gsub("E",0,y$ToPar)
  y[cols] <- lapply(y[cols], as.numeric)
  a <- as.data.frame(table(y$Finish))
  colnames(a) <- c("Finish", "Count")
  b <- merge(y, a, by.x = "Finish", by.y = "Finish")
  b$Count <- b$Count - 1
  b$Wins <- nrow(b) - b$Finish - b$Count
  b$Ties <- b$Count
  b$Losses <- (nrow(b)-1) - b$Wins - b$Ties
  b <- b[,c(2:9,1,11,13,12)]
  c <- b
  c$TWplay <- ifelse(c$Player == "Tiger Woods", 1, 0)
  c$TWwin <- ifelse(c$TWplay == 0, c$Finish < c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWLoss <- ifelse(c$TWplay == 0, c$Finish > c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWTie <- ifelse(c$TWplay == 0, c$Finish == c[which(c$Player == "Tiger Woods"),9],0)
  d <- merge(b,c)
  d[is.na(d)] <- 0
  d <- d[order(d$Finish),]
  e <- data.frame(Tournament = tournament)
  final <- cbind(d,e)
}

#Barracuda function
#Removes WDs, DQs and takes into account events where players play 3 of 4 rounds
barracuda <- function(site, tournament) {
  x <- read_html(site)
  y <- x %>% html_table(fill = TRUE)
  y <- as.data.frame(y)
  colnames(y) <- y[2,]
  y <- y[-c(1:2),-c(2,8:10,13:16)]
  made_cut <- subset(y, !(y$Place %in% c("CUT", "WD", "DQ")))
  made_cut$Finish <- gsub("T","",made_cut$Place)
  made_cut$Finish <- gsub("Win",1,made_cut$Finish)
  made_cut$`Total Score` <- sub(".*=","",made_cut$Player)
  made_cut$`Total Score` <-  gsub("[()]", "", made_cut$`Total Score`)
  made_cut$`Total Score` <- as.numeric(made_cut$`Total Score`)
  made_cut$Player <- gsub("Playoff:.*","",made_cut$Player)
  made_cut$Player <- gsub("Round:.*","",made_cut$Player)
  made_cut$Player <- gsub("Scores:.*","",made_cut$Player)
  made_cut$Finish <- as.numeric(made_cut$Finish)
  missed_cut <- subset(y, y$Place %in% "CUT")
  missed_cut$`Total Score` <- sub(".*=","",missed_cut$Player)
  missed_cut$`Total Score` <-  gsub("[()]", "", missed_cut$`Total Score`)
  missed_cut$`Total Score` <- as.numeric(missed_cut$`Total Score`)
  missed_cut$Finish <- rank(-missed_cut$`Total Score`, ties.method = "min")+nrow(made_cut)
  missed_cut <- missed_cut[order(missed_cut$Finish),]
  missed_cut$Player <- gsub("Scores:.*","",missed_cut$Player)
  y_final <- rbind(made_cut,missed_cut)
  colnames(y_final)[8] <- "TotalScore"
  cols <- names(y_final)[3:9]
  y_final[cols] <- lapply(y_final[cols], as.numeric)
  y_final <- as.data.frame(y_final)  
  a <- as.data.frame(table(y_final$Finish))
  colnames(a) <- c("Finish", "Count")
  b <- merge(y_final, a, by.x = "Finish", by.y = "Finish")
  b$Count <- b$Count - 1
  b$Wins <- nrow(b) - b$Finish - b$Count
  b$Ties <- b$Count
  b$Losses <- (nrow(b)-1) - b$Wins - b$Ties
  b <- b[,c(2:9,1,11,13,12)]
  c <- b
  c$TWplay <- ifelse(c$Player == "Tiger Woods", 1, 0)
  c$TWwin <- ifelse(c$TWplay == 0, c$Finish < c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWLoss <- ifelse(c$TWplay == 0, c$Finish > c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWTie <- ifelse(c$TWplay == 0, c$Finish == c[which(c$Player == "Tiger Woods"),9],0)
  d <- merge(b,c)
  d[is.na(d)] <- 0
  d <- d[order(d$Finish),]
  e <- data.frame(Tournament = tournament)
  final <- cbind(d,e)
}


#Function for Stableford events, which scores based on points not strokes
international <- function(site, tournament) {
  x <- read_html(site)
  y <- x %>% html_table(fill = TRUE)
  y <- as.data.frame(y)
  colnames(y) <- y[2,]
  y <- y[-c(1:2),-c(2,8:10,13:16)]
  y$Player <- gsub("Playoff:.*","",y$Player)
  y$Player <- gsub("Round:.*","",y$Player)
  y$Player <- gsub("Scores:.*","",y$Player)
  made_cut <- subset(y, !(y$Place %in% c("CUT", "WD", "DQ")))
  made_cut$Finish <- gsub("T","",made_cut$Place)
  made_cut$Finish <- gsub("Win",1,made_cut$Finish)
  made_cut$Finish <- as.numeric(made_cut$Finish)
  missed_cut <- subset(y, y$Place %in% "CUT")
  missed_cut$`Total Score` <- as.numeric(missed_cut$`Total Score`)
  missed_cut$Finish <- rank(-missed_cut$`Total Score`, ties.method = "min")+nrow(made_cut)
  y_final <- rbind(made_cut,missed_cut)
  colnames(y_final)[8] <- "TotalScore"
  cols <- names(y_final)[3:9]
  y_final$ToPar <- gsub("E",0,y_final$ToPar)
  y_final$R4 <- gsub("E",0,y_final$R4)
  y_final[cols] <- lapply(y_final[cols], as.numeric)
  y_final <- as.data.frame(y_final)  
  a <- as.data.frame(table(y_final$Finish))
  colnames(a) <- c("Finish", "Count")
  b <- merge(y_final, a, by.x = "Finish", by.y = "Finish")
  b$Count <- b$Count - 1
  b$Wins <- nrow(b) - b$Finish - b$Count
  b$Ties <- b$Count
  b$Losses <- (nrow(b)-1) - b$Wins - b$Ties
  b <- b[,c(2:9,1,11,13,12)]
  c <- b
  c$TWplay <- ifelse(c$Player == "Tiger Woods", 1, 0)
  c$TWwin <- ifelse(c$TWplay == 0, c$Finish < c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWLoss <- ifelse(c$TWplay == 0, c$Finish > c[which(c$Player == "Tiger Woods"),9], 0)
  c$TWTie <- ifelse(c$TWplay == 0, c$Finish == c[which(c$Player == "Tiger Woods"),9],0)
  d <- merge(b,c)
  d[is.na(d)] <- 0
  d <- d[order(d$Finish),]
  e <- data.frame(Tournament = tournament)
  final <- cbind(d,e)
}

#1996 Season
MercedesBenzChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_96 ")
ChryslerClassicTucson_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_96 ")
BobHopeChryslerClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_96 ")
FBROpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_96 ")
BuickInvitational_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_96 ")
SonyOpeninHawaii_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_96 ")
NorthernTrustOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_96 ")
FordChampionshipatDoral_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_96 ")
HondaClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_96 ")
ArnoldPalmerInvitational_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_96 ")
ZurichClassicofNewOrleans_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_96 ")
ThePlayersChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_96 ")
ATTClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_96 ")
Masters_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Masters&player=&tour=PGA&submit=go","Masters_96 ")
VerizonHeritage_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_96 ")
WyndhamChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_96 ")
ShellHoustonOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_96 ")
EDSByronNelsonClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_96 ")
CrownePlazaInvitationalatColonial_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_96 ")
BoozAllenClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_96 ")
MemorialTournament_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_96 ")
BarclaysClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_96 ")
USOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_96 ")
StanfordStJudeClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_96 ")
TravelersChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_96 ")
BMWChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_96 ")
MichelobChampionshipatKingsmill_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_96 ")
BritishOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_96 ")
VikingClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_96 ")
CVSCharityClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=CVS+Charity+Classic&player=&tour=PGA&submit=go","CVSCharityClassic_96 ")
BuickOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_96 ")
PGAChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_96 ")
TheInternational_96 <- international("https://www.golfstats.com/search/?yr=1996&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_96 ")
NECWorldSeriesofGolf_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=NEC+World+Series+of+Golf&player=&tour=PGA&submit=go","NECWorldSeriesofGolf_96 ")
AirCanadaChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_96 ")
USBankChampionshipinMilwaukee_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_96 ")
RBCCanadianOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_96 ")
JohnDeereClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_96 ")
BCOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_96 ")
BuickChallenge_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_96 ")
JustinTimberlakeShrinersHospitals_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_96 ")
ValeroTexasOpen_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_96 ")
ChildrensMiracleNetworkClassic_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Children's+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_96 ")
TourChampionship_96 <- golfstat("https://www.golfstats.com/search/?yr=1996&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_96 ")

#1997 Season)
MercedesBenzChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_97 ")
BobHopeChryslerClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_97 ")
FBROpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_97 ")
ATTPebbleBeach_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_97 ")
BuickInvitational_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_97 ")
SonyOpeninHawaii_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_97 ")
ChryslerClassicTucson_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_97 ")
NorthernTrustOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_97 ")
FordChampionshipatDoral_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_97 ")
HondaClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_97 ")
ArnoldPalmerInvitational_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_97 ")
ThePlayersChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_97 ")
ZurichClassicofNewOrleans_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_97 ")
Masters_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Masters&player=&tour=PGA&submit=go","Masters_97 ")
VerizonHeritage_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_97 ")
WyndhamChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_97 ")
ShellHoustonOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_97 ")
ATTClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_97 ")
EDSByronNelsonClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_97 ")
CrownePlazaInvitationalatColonial_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_97 ")
MemorialTournament_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_97 ")
BoozAllenClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_97 ")
USOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_97 ")
BarclaysClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_97 ")
StanfordStJudeClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_97 ")
BMWChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_97 ")
JohnDeereClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_97 ")
BritishOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_97 ")
VikingClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_97 ")
TravelersChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_97 ")
TheInternational_97 <- international("https://www.golfstats.com/search/?yr=1997&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_97 ")
BuickOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_97 ")
PGAChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_97 ")
NECWorldSeriesofGolf_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=NEC+World+Series+of+Golf&player=&tour=PGA&submit=go","NECWorldSeriesofGolf_97 ")
AirCanadaChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_97 ")
USBankChampionshipinMilwaukee_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_97 ")
RBCCanadianOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_97 ")
CVSCharityClassic_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=CVS+Charity+Classic&player=&tour=PGA&submit=go","CVSCharityClassic_97 ")
ValeroTexasOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_97 ")
BCOpen_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_97 ")
BuickChallenge_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_97 ")
BuickChallenge_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_97 ")
MichelobChampionshipatKingsmill_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_97 ")
JustinTimberlakeShrinersHospitals_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_97 ")
TourChampionship_97 <- golfstat("https://www.golfstats.com/search/?yr=1997&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_97 ")

#1998 Season
MercedesBenzChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_98 ")
BobHopeChryslerClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_98 ")
FBROpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_98 ")
BuickInvitational_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_98 ")
SonyOpeninHawaii_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_98 ")
ChryslerClassicTucson_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_98 ")
NorthernTrustOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_98 ")
FordChampionshipatDoral_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_98 ")
HondaClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_98 ")
ArnoldPalmerInvitational_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_98 ")
ThePlayersChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_98 ")
ZurichClassicofNewOrleans_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_98 ")
Masters_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Masters&player=&tour=PGA&submit=go","Masters_98 ")
VerizonHeritage_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_98 ")
WyndhamChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_98 ")
ShellHoustonOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_98 ")
ATTClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_98 ")
EDSByronNelsonClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_98 ")
CrownePlazaInvitationalatColonial_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_98 ")
MemorialTournament_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_98 ")
BoozAllenClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_98 ")
BarclaysClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_98 ")
USOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_98 ")
BMWChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_98 ")
TravelersChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_98 ")
JohnDeereClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_98 ")
BritishOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_98 ")
VikingClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_98 ")
CVSCharityClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=CVS+Charity+Classic&player=&tour=PGA&submit=go","CVSCharityClassic_98 ")
StanfordStJudeClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_98 ")
BuickOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_98 ")
PGAChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_98 ")
ATTPebbleBeach_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_98 ")
TheInternational_98 <- international("https://www.golfstats.com/search/?yr=1998&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_98 ")
NECWorldSeriesofGolf_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=NEC+World+Series+of+Golf&player=&tour=PGA&submit=go","NECWorldSeriesofGolf_98 ")
AirCanadaChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_98 ")
USBankChampionshipinMilwaukee_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_98 ")
RBCCanadianOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_98 ")
BCOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_98 ")
ValeroTexasOpen_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_98 ")
BuickChallenge_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_98 ")
MichelobChampionshipatKingsmill_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_98 ")
JustinTimberlakeShrinersHospitals_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_98 ")
ChildrensMiracleNetworkClassic_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_98 ")
TourChampionship_98 <- golfstat("https://www.golfstats.com/search/?yr=1998&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_98 ")

#1999 Season
MercedesBenzChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_99 ")
SonyOpeninHawaii_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_99 ")
BobHopeChryslerClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_99 ")
FBROpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_99 ")
ATTPebbleBeach_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_99 ")
BuickInvitational_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_99 ")
NorthernTrustOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_99 ")
ChryslerClassicTucson_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_99 ")
WGCAccentureMatchPlayChampionship_99 <- matchplay("https://www.golfstats.com/search/?yr=1999&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_99 ")
FordChampionshipatDoral_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_99 ")
HondaClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_99 ")
ArnoldPalmerInvitational_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_99 ")
ThePlayersChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_99 ")
ATTClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_99 ")
Masters_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Masters&player=&tour=PGA&submit=go","Masters_99 ")
VerizonHeritage_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_99 ")
WyndhamChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_99 ")
ShellHoustonOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_99 ")
ZurichClassicofNewOrleans_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_99 ")
EDSByronNelsonClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_99 ")
CrownePlazaInvitationalatColonial_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_99 ")
BoozAllenClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_99 ")
MemorialTournament_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_99 ")
StanfordStJudeClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_99 ")
USOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_99 ")
BarclaysClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_99 ")
BMWChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_99 ")
USBankChampionshipinMilwaukee_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_99 ")
BritishOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_99 ")
JohnDeereClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_99 ")
TravelersChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_99 ")
BuickOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_99 ")
PGAChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_99 ")
TheInternational_99 <- international("https://www.golfstats.com/search/?yr=1999&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_99 ")
WGCBridgestoneInvitational_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_99 ")
LegendsRenoTahoeOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_99 ")
AirCanadaChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_99 ")
RBCCanadianOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_99 ")
BCOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_99 ")
ValeroTexasOpen_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_99 ")
BuickChallenge_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_99 ")
MichelobChampionshipatKingsmill_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_99 ")
JustinTimberlakeShrinersHospitals_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_99 ")
ChildrensMiracleNetworkClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_99 ")
TourChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_99 ")
VikingClassic_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_99 ")
WGCCAChampionship_99 <- golfstat("https://www.golfstats.com/search/?yr=1999&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_99 ")

#2000 Season
MercedesBenzChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_00 ")
SonyOpeninHawaii_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_00 ")
BobHopeChryslerClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_00 ")
FBROpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_00 ")
ATTPebbleBeach_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_00 ")
BuickInvitational_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_00 ")
NorthernTrustOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_00 ")
ChryslerClassicTucson_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_00 ")
WGCAccentureMatchPlayChampionship_00 <- matchplay("https://www.golfstats.com/search/?yr=2000&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_00 ")
FordChampionshipatDoral_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_00 ")
HondaClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_00 ")
ArnoldPalmerInvitational_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_00 ")
ThePlayersChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_00 ")
ATTClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_00 ")
Masters_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Masters&player=&tour=PGA&submit=go","Masters_00 ")
VerizonHeritage_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_00 ")
WyndhamChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_00 ")
ShellHoustonOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_00 ")
ZurichClassicofNewOrleans_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_00 ")
EDSByronNelsonClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_00 ")
CrownePlazaInvitationalatColonial_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_00 ")
MemorialTournament_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_00 ")
BoozAllenClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_00 ")
BarclaysClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_00 ")
USOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_00 ")
StanfordStJudeClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_00 ")
TravelersChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_00 ")
BMWChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_00 ")
USBankChampionshipinMilwaukee_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_00 ")
BCOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_00 ")
BritishOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_00 ")
JohnDeereClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_00 ")
TheInternational_00 <- international("https://www.golfstats.com/search/?yr=2000&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_00 ")
BuickOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_00 ")
PGAChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_00 ")
WGCBridgestoneInvitational_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_00 ")
LegendsRenoTahoeOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_00 ")
AirCanadaChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_00 ")
RBCCanadianOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_00 ")
LumberClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_00 ")
ValeroTexasOpen_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_00 ")
BuickChallenge_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_00 ")
MichelobChampionshipatKingsmill_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_00 ")
JustinTimberlakeShrinersHospitals_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_00 ")
PODSChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_00 ")
ChildrensMiracleNetworkClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_00 ")
VikingClassic_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_00 ")
TourChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_00 ")
WGCCAChampionship_00 <- golfstat("https://www.golfstats.com/search/?yr=2000&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_00 ")

#2001 Season
WGCAccentureMatchPlayChampionship_01 <- matchplay("https://www.golfstats.com/search/?yr=2001&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_01 ")
MercedesBenzChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_01 ")
ChryslerClassicTucson_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_01 ")
SonyOpeninHawaii_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_01 ")
FBROpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_01 ")
ATTPebbleBeach_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_01 ")
BuickInvitational_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_01 ")
BobHopeChryslerClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_01 ")
NorthernTrustOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_01 ")
FordChampionshipatDoral_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_01 ")
HondaClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_01 ")
ArnoldPalmerInvitational_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_01 ")
ThePlayersChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_01 ")
ATTClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_01 ")
Masters_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Masters&player=&tour=PGA&submit=go","Masters_01 ")
VerizonHeritage_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_01 ")
ShellHoustonOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_01 ")
WyndhamChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_01 ")
ZurichClassicofNewOrleans_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_01 ")
EDSByronNelsonClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_01 ")
CrownePlazaInvitationalatColonial_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_01 ")
BoozAllenClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_01 ")
MemorialTournament_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_01 ")
StanfordStJudeClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_01 ")
USOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_01 ")
BarclaysClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_01 ")
TravelersChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_01 ")
BMWChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_01 ")
USBankChampionshipinMilwaukee_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_01 ")
BCOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_01 ")
BritishOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_01 ")
JohnDeereClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_01 ")
TheInternational_01 <- international("https://www.golfstats.com/search/?yr=2001&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_01 ")
BuickOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_01 ")
PGAChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_01 ")
WGCBridgestoneInvitational_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_01 ")
LegendsRenoTahoeOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_01 ")
AirCanadaChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_01 ")
RBCCanadianOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_01 ")
LumberClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_01 ")
ValeroTexasOpen_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_01 ")
MichelobChampionshipatKingsmill_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_01 ")
JustinTimberlakeShrinersHospitals_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_01 ")
ChildrensMiracleNetworkClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_01 ")
BuickChallenge_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_01 ")
VikingClassic_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_01 ")
TourChampionship_01 <- golfstat("https://www.golfstats.com/search/?yr=2001&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_01 ")

#2002 Season
MercedesBenzChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_02 ")
SonyOpeninHawaii_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_02 ")
BobHopeChryslerClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_02 ")
FBROpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_02 ")
ATTPebbleBeach_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_02 ")
BuickInvitational_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_02 ")
NorthernTrustOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_02 ")
ChryslerClassicTucson_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_02 ")
WGCAccentureMatchPlayChampionship_02 <- matchplay("https://www.golfstats.com/search/?yr=2002&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_02 ")
FordChampionshipatDoral_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_02 ")
HondaClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_02 ")
ArnoldPalmerInvitational_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_02 ")
ThePlayersChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_02 ")
ShellHoustonOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_02 ")
ATTClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_02 ")
Masters_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Masters&player=&tour=PGA&submit=go","Masters_02 ")
VerizonHeritage_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_02 ")
WyndhamChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_02 ")
ZurichClassicofNewOrleans_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_02 ")
EDSByronNelsonClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_02 ")
CrownePlazaInvitationalatColonial_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_02 ")
MemorialTournament_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_02 ")
BoozAllenClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_02 ")
BarclaysClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_02 ")
USOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_02 ")
TravelersChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_02 ")
StanfordStJudeClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_02 ")
BMWChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_02 ")
USBankChampionshipinMilwaukee_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_02 ")
BCOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_02 ")
BritishOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_02 ")
JohnDeereClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_02 ")
TheInternational_02 <- international("https://www.golfstats.com/search/?yr=2002&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_02 ")
BuickOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_02 ")
PGAChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_02 ")
WGCBridgestoneInvitational_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_02 ")
LegendsRenoTahoeOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_02 ")
AirCanadaChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Air+Canada+Championship&player=&tour=PGA&submit=go","AirCanadaChampionship_02 ")
RBCCanadianOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_02 ")
LumberClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_02 ")
WGCCAChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_02 ")
PODSChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_02 ")
ValeroTexasOpen_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_02 ")
MichelobChampionshipatKingsmill_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Michelob+Championship+at+Kingsmill&player=&tour=PGA&submit=go","MichelobChampionshipatKingsmill_02 ")
JustinTimberlakeShrinersHospitals_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_02 ")
ChildrensMiracleNetworkClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_02 ")
BuickChallenge_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Buick+Challenge&player=&tour=PGA&submit=go","BuickChallenge_02 ")
TourChampionship_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_02 ")
VikingClassic_02 <- golfstat("https://www.golfstats.com/search/?yr=2002&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_02 ")

#2003 Season
MercedesBenzChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_03 ")
SonyOpeninHawaii_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_03 ")
FBROpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_03 ")
BobHopeChryslerClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_03 ")
ATTPebbleBeach_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_03 ")
BuickInvitational_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_03 ")
NorthernTrustOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_03 ")
ChryslerClassicTucson_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_03 ")
WGCAccentureMatchPlayChampionship_03 <- matchplay("https://www.golfstats.com/search/?yr=2003&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_03 ")
FordChampionshipatDoral_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_03 ")
HondaClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_03 ")
ArnoldPalmerInvitational_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_03 ")
ThePlayersChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_03 ")
ATTClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_03 ")
Masters_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Masters&player=&tour=PGA&submit=go","Masters_03 ")
VerizonHeritage_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_03 ")
ShellHoustonOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_03 ")
ZurichClassicofNewOrleans_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_03 ")
WachoviaChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Wachovia+Championship&player=&tour=PGA&submit=go","WachoviaChampionship_03 ")
EDSByronNelsonClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_03 ")
CrownePlazaInvitationalatColonial_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_03 ")
MemorialTournament_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_03 ")
BoozAllenClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_03 ")
USOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_03 ")
BarclaysClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_03 ")
StanfordStJudeClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_03 ")
BMWChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_03 ")
USBankChampionshipinMilwaukee_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_03 ")
BCOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_03 ")
BritishOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_03 ")
TravelersChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_03 ")
BuickOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_03 ")
TheInternational_03 <- international("https://www.golfstats.com/search/?yr=2003&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_03 ")
PGAChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_03 ")
WGCBridgestoneInvitational_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_03 ")
LegendsRenoTahoeOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_03 ")
DeutscheBankChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_03 ")
RBCCanadianOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_03 ")
JohnDeereClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_03 ")
LumberClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_03 ")
ValeroTexasOpen_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_03 ")
WGCCAChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_03 ")
VikingClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_03 ")
JustinTimberlakeShrinersHospitals_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_03 ")
WyndhamChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_03 ")
ChildrensMiracleNetworkClassic_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_03 ")
PODSChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_03 ")
TourChampionship_03 <- golfstat("https://www.golfstats.com/search/?yr=2003&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_03 ")

#2004 Season
MercedesBenzChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_04 ")
SonyOpeninHawaii_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_04 ")
BobHopeChryslerClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_04 ")
FBROpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_04 ")
ATTPebbleBeach_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_04 ")
BuickInvitational_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_04 ")
NorthernTrustOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_04 ")
ChryslerClassicTucson_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_04 ")
WGCAccentureMatchPlayChampionship_04 <- matchplay("https://www.golfstats.com/search/?yr=2004&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_04 ")
FordChampionshipatDoral_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_04 ")
HondaClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_04 ")
ArnoldPalmerInvitational_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_04 ")
ThePlayersChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_04 ")
ATTClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_04 ")
Masters_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Masters&player=&tour=PGA&submit=go","Masters_04 ")
VerizonHeritage_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_04 ")
ShellHoustonOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_04 ")
ZurichClassicofNewOrleans_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_04 ")
WachoviaChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Wachovia+Championship&player=&tour=PGA&submit=go","WachoviaChampionship_04 ")
EDSByronNelsonClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_04 ")
CrownePlazaInvitationalatColonial_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_04 ")
StanfordStJudeClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_04 ")
MemorialTournament_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_04 ")
BarclaysClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_04 ")
USOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_04 ")
BoozAllenClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_04 ")
BMWChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_04 ")
JohnDeereClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_04 ")
BCOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_04 ")
BritishOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_04 ")
USBankChampionshipinMilwaukee_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_04 ")
BuickOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_04 ")
TheInternational_04 <- international("https://www.golfstats.com/search/?yr=2004&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_04 ")
PGAChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_04 ")
WGCBridgestoneInvitational_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_04 ")
LegendsRenoTahoeOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_04 ")
TravelersChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_04 ")
DeutscheBankChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_04 ")
RBCCanadianOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_04 ")
ValeroTexasOpen_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_04 ")
LumberClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_04 ")
WGCCAChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_04 ")
VikingClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_04 ")
JustinTimberlakeShrinersHospitals_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_04 ")
WyndhamChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_04 ")
ChildrensMiracleNetworkClassic_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_04 ")
PODSChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_04 ")
TourChampionship_04 <- golfstat("https://www.golfstats.com/search/?yr=2004&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_04 ")

#2005 Season
MercedesBenzChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_05 ")
SonyOpeninHawaii_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_05 ")
BuickInvitational_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_05 ")
BobHopeChryslerClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_05 ")
FBROpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_05 ")
ATTPebbleBeach_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_05 ")
NorthernTrustOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_05 ")
ChryslerClassicTucson_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_05 ")
WGCAccentureMatchPlayChampionship_05 <- matchplay("https://www.golfstats.com/search/?yr=2005&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_05 ")
FordChampionshipatDoral_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_05 ")
HondaClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_05 ")
ArnoldPalmerInvitational_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_05 ")
ThePlayersChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_05 ")
ATTClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_05 ")
Masters_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Masters&player=&tour=PGA&submit=go","Masters_05 ")
VerizonHeritage_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_05 ")
ShellHoustonOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_05 ")
ZurichClassicofNewOrleans_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_05 ")
WachoviaChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Wachovia+Championship&player=&tour=PGA&submit=go","WachoviaChampionship_05 ")
EDSByronNelsonClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_05 ")
CrownePlazaInvitationalatColonial_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_05 ")
StanfordStJudeClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_05 ")
MemorialTournament_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_05 ")
BoozAllenClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_05 ")
USOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_05 ")
BarclaysClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_05 ")
BMWChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_05 ")
JohnDeereClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_05 ")
BCOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_05 ")
BritishOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_05 ")
USBankChampionshipinMilwaukee_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_05 ")
BuickOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_05 ")
TheInternational_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_05 ")
PGAChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_05 ")
WGCBridgestoneInvitational_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_05 ")
LegendsRenoTahoeOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_05 ")
TravelersChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_05 ")
DeutscheBankChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_05 ")
RBCCanadianOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_05 ")
LumberClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_05 ")
ValeroTexasOpen_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_05 ")
WyndhamChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_05 ")
WGCCAChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_05 ")
JustinTimberlakeShrinersHospitals_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_05 ")
ChildrensMiracleNetworkClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_05 ")
PODSChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_05 ")
VikingClassic_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_05 ")
TourChampionship_05 <- golfstat("https://www.golfstats.com/search/?yr=2005&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_05 ")

#2006 Season
MercedesBenzChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_06 ")
SonyOpeninHawaii_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_06 ")
BobHopeChryslerClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_06 ")
BuickInvitational_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_06 ")
FBROpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_06 ")
ATTPebbleBeach_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_06 ")
NorthernTrustOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_06 ")
ChryslerClassicTucson_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Chrysler+Classic+Tucson&player=&tour=PGA&submit=go","ChryslerClassicTucson_06 ")
WGCAccentureMatchPlayChampionship_06 <- matchplay("https://www.golfstats.com/search/?yr=2006&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_06 ")
FordChampionshipatDoral_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Ford+Championship+at+Doral&player=&tour=PGA&submit=go","FordChampionshipatDoral_06 ")
HondaClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_06 ")
ArnoldPalmerInvitational_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_06 ")
ThePlayersChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_06 ")
ATTClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_06 ")
Masters_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Masters&player=&tour=PGA&submit=go","Masters_06 ")
VerizonHeritage_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_06 ")
ShellHoustonOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_06 ")
ZurichClassicofNewOrleans_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_06 ")
WachoviaChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Wachovia+Championship&player=&tour=PGA&submit=go","WachoviaChampionship_06 ")
EDSByronNelsonClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_06 ")
CrownePlazaInvitationalatColonial_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_06 ")
StanfordStJudeClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_06 ")
MemorialTournament_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_06 ")
BarclaysClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_06 ")
USOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_06 ")
BoozAllenClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Booz+Allen+Classic&player=&tour=PGA&submit=go","BoozAllenClassic_06 ")
TravelersChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_06 ")
BMWChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_06 ")
JohnDeereClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_06 ")
BCOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=B.C.+Open&player=&tour=PGA&submit=go","BCOpen_06 ")
BritishOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_06 ")
USBankChampionshipinMilwaukee_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_06 ")
BuickOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_06 ")
TheInternational_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=The+International&player=&tour=PGA&submit=go","TheInternational_06 ")
PGAChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_06 ")
WGCBridgestoneInvitational_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_06 ")
LegendsRenoTahoeOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_06 ")
DeutscheBankChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_06 ")
RBCCanadianOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_06 ")
LumberClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=84+Lumber+Classic&player=&tour=PGA&submit=go","LumberClassic_06 ")
ValeroTexasOpen_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_06 ")
WGCCAChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_06 ")
VikingClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_06 ")
WyndhamChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_06 ")
JustinTimberlakeShrinersHospitals_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_06 ")
ChildrensMiracleNetworkClassic_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_06 ")
PODSChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_06 ")
TourChampionship_06 <- golfstat("https://www.golfstats.com/search/?yr=2006&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_06 ")

#2007 Season
MercedesBenzChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_07 ")
SonyOpeninHawaii_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_07 ")
BobHopeChryslerClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_07 ")
BuickInvitational_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_07 ")
FBROpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=FBR+Open&player=&tour=PGA&submit=go","FBROpen_07 ")
ATTPebbleBeach_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_07 ")
NorthernTrustOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_07 ")
MayakobaGolfClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_07 ")
WGCAccentureMatchPlayChampionship_07 <- matchplay("https://www.golfstats.com/search/?yr=2007&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_07 ")
HondaClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_07 ")
PODSChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_07 ")
ArnoldPalmerInvitational_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_07 ")
WGCCAChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_07 ")
ShellHoustonOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_07 ")
Masters_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Masters&player=&tour=PGA&submit=go","Masters_07 ")
VerizonHeritage_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_07 ")
ZurichClassicofNewOrleans_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_07 ")
EDSByronNelsonClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=EDS+Byron+Nelson+Classic&player=&tour=PGA&submit=go","EDSByronNelsonClassic_07 ")
WachoviaChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Wachovia+Championship&player=&tour=PGA&submit=go","WachoviaChampionship_07 ")
ThePlayersChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_07 ")
ATTClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_07 ")
CrownePlazaInvitationalatColonial_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_07 ")
MemorialTournament_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_07 ")
StanfordStJudeClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_07 ")
USOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_07 ")
TravelersChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_07 ")
BuickOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_07 ")
ATTNational_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_07 ")
JohnDeereClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_07 ")
USBankChampionshipinMilwaukee_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_07 ")
BritishOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_07 ")
RBCCanadianOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_07 ")
WGCBridgestoneInvitational_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_07 ")
LegendsRenoTahoeOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_07 ")
PGAChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_07 ")
WyndhamChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_07 ")
BarclaysClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_07 ")
DeutscheBankChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_07 ")
BMWChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_07 ")
TourChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_07 ")
TurningStoneResortChampionship_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Turning+Stone+Resort+Championship&player=&tour=PGA&submit=go","TurningStoneResortChampionship_07 ")
VikingClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_07 ")
ValeroTexasOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_07 ")
JustinTimberlakeShrinersHospitals_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_07 ")
FryscomOpen_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_07 ")
GinnsurMerClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Ginn+sur+Mer+Classic&player=&tour=PGA&submit=go","GinnsurMerClassic_07 ")
ChildrensMiracleNetworkClassic_07 <- golfstat("https://www.golfstats.com/search/?yr=2007&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_07 ")

#2008 Season
MercedesBenzChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_08 ")
SonyOpeninHawaii_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_08 ")
BobHopeChryslerClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_08 ")
BuickInvitational_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_08 ")
WasteManagementPhoenixOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_08 ")
ATTPebbleBeach_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_08 ")
NorthernTrustOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_08 ")
MayakobaGolfClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_08 ")
WGCAccentureMatchPlayChampionship_08 <- matchplay("https://www.golfstats.com/search/?yr=2008&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_08 ")
HondaClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_08 ")
PODSChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=PODS+Championship&player=&tour=PGA&submit=go","PODSChampionship_08 ")
ArnoldPalmerInvitational_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_08 ")
WGCCAChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_08 ")
PuertoRicoOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_08 ")
ZurichClassicofNewOrleans_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_08 ")
ShellHoustonOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_08 ")
Masters_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Masters&player=&tour=PGA&submit=go","Masters_08 ")
VerizonHeritage_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_08 ")
HPByronNelsonChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_08 ")
WachoviaChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Wachovia+Championship&player=&tour=PGA&submit=go","WachoviaChampionship_08 ")
ThePlayersChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_08 ")
ATTClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=AT%26T+Classic&player=&tour=PGA&submit=go","ATTClassic_08 ")
CrownePlazaInvitationalatColonial_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_08 ")
MemorialTournament_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_08 ")
StanfordStJudeClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Stanford+St.+Jude+Classic&player=&tour=PGA&submit=go","StanfordStJudeClassic_08 ")
USOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_08 ")
TravelersChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_08 ")
BuickOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_08 ")
ATTNational_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_08 ")
JohnDeereClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_08 ")
USBankChampionshipinMilwaukee_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_08 ")
BritishOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_08 ")
RBCCanadianOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_08 ")
WGCBridgestoneInvitational_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_08 ")
LegendsRenoTahoeOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_08 ")
PGAChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_08 ")
WyndhamChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_08 ")
TheBarclays_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=The+Barclays&player=&tour=PGA&submit=go","TheBarclays_08 ")
DeutscheBankChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_08 ")
BMWChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_08 ")
VikingClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_08 ")
TourChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_08 ")
TurningStoneResortChampionship_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Turning+Stone+Resort+Championship&player=&tour=PGA&submit=go","TurningStoneResortChampionship_08 ")
ValeroTexasOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_08 ")
JustinTimberlakeShrinersHospitals_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_08 ")
FryscomOpen_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_08 ")
GinnsurMerClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Ginn+sur+Mer+Classic&player=&tour=PGA&submit=go","GinnsurMerClassic_08 ")
ChildrensMiracleNetworkClassic_08 <- golfstat("https://www.golfstats.com/search/?yr=2008&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_08 ")

#2009 Season
MercedesBenzChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Mercedes-Benz+Championship&player=&tour=PGA&submit=go","MercedesBenzChampionship_09 ")
SonyOpeninHawaii_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_09 ")
BobHopeChryslerClassic_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Bob+Hope+Chrysler+Classic&player=&tour=PGA&submit=go","BobHopeChryslerClassic_09 ")
WasteManagementPhoenixOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_09 ")
BuickInvitational_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Buick+Invitational&player=&tour=PGA&submit=go","BuickInvitational_09 ")
ATTPebbleBeach_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_09 ")
NorthernTrustOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_09 ")
WGCAccentureMatchPlayChampionship_09 <- matchplay("https://www.golfstats.com/search/?yr=2009&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_09 ")
MayakobaGolfClassic_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_09 ")
HondaClassic_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_09 ")
PuertoRicoOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_09 ")
WGCCAChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_09 ")
TransitionsChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Transitions+Championship&player=&tour=PGA&submit=go","TransitionsChampionship_09 ")
ArnoldPalmerInvitational_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_09 ")
ShellHoustonOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_09 ")
Masters_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Masters&player=&tour=PGA&submit=go","Masters_09 ")
VerizonHeritage_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_09 ")
ZurichClassicofNewOrleans_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_09 ")
QuailHollowChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Quail+Hollow+Championship&player=&tour=PGA&submit=go","QuailHollowChampionship_09 ")
ThePlayersChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_09 ")
ValeroTexasOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_09 ")
HPByronNelsonChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_09 ")
CrownePlazaInvitationalatColonial_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_09 ")
MemorialTournament_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_09 ")
StJudeClassic_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=St.+Jude+Classic&player=&tour=PGA&submit=go","StJudeClassic_09 ")
USOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_09 ")
TravelersChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_09 ")
ATTNational_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_09 ")
JohnDeereClassic_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_09 ")
USBankChampionshipinMilwaukee_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=U.S.+Bank+Championship+in+Milwaukee&player=&tour=PGA&submit=go","USBankChampionshipinMilwaukee_09 ")
BritishOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_09 ")
RBCCanadianOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_09 ")
BuickOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Buick+Open&player=&tour=PGA&submit=go","BuickOpen_09 ")
WGCBridgestoneInvitational_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_09 ")
LegendsRenoTahoeOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Legends+Reno-Tahoe+Open&player=&tour=PGA&submit=go","LegendsRenoTahoeOpen_09 ")
PGAChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_09 ")
WyndhamChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_09 ")
TheBarclays_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=The+Barclays&player=&tour=PGA&submit=go","TheBarclays_09 ")
DeutscheBankChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_09 ")
BMWChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_09 ")
TourChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_09 ")
TurningStoneResortChampionship_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Turning+Stone+Resort+Championship&player=&tour=PGA&submit=go","TurningStoneResortChampionship_09 ")
JustinTimberlakeShrinersHospitals_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_09 ")
FryscomOpen_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_09 ")
ChildrensMiracleNetworkClassic_09 <- golfstat("https://www.golfstats.com/search/?yr=2009&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_09 ")

#2010 Season
SBSChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=SBS+Championship&player=&tour=PGA&submit=go","SBSChampionship_10 ")
SonyOpeninHawaii_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_10 ")
BobHopeClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Bob+Hope+Classic&player=&tour=PGA&submit=go","BobHopeClassic_10 ")
FarmersInsuranceOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_10 ")
NorthernTrustOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_10 ")
ATTPebbleBeach_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_10 ")
MayakobaGolfClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_10 ")
WGCAccentureMatchPlayChampionship_10 <- matchplay("https://www.golfstats.com/search/?yr=2010&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_10 ")
WasteManagementPhoenixOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_10 ")
HondaClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_10 ")
WGCCAChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=WGC-CA+Championship&player=&tour=PGA&submit=go","WGCCAChampionship_10 ")
PuertoRicoOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_10 ")
TransitionsChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Transitions+Championship&player=&tour=PGA&submit=go","TransitionsChampionship_10 ")
ArnoldPalmerInvitational_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_10 ")
ShellHoustonOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_10 ")
Masters_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Masters&player=&tour=PGA&submit=go","Masters_10 ")
VerizonHeritage_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Verizon+Heritage&player=&tour=PGA&submit=go","VerizonHeritage_10 ")
ZurichClassicofNewOrleans_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_10 ")
QuailHollowChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Quail+Hollow+Championship&player=&tour=PGA&submit=go","QuailHollowChampionship_10 ")
ThePlayersChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_10 ")
ValeroTexasOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_10 ")
HPByronNelsonChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_10 ")
CrownePlazaInvitationalatColonial_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_10 ")
MemorialTournament_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_10 ")
StJudeClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=St.+Jude+Classic&player=&tour=PGA&submit=go","StJudeClassic_10 ")
USOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_10 ")
TravelersChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_10 ")
ATTNational_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_10 ")
JohnDeereClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_10 ")
RenoTahoeOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Reno-Tahoe+Open&player=&tour=PGA&submit=go","RenoTahoeOpen_10 ")
BritishOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_10 ")
RBCCanadianOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_10 ")
TheGreenbrierClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_10 ")
WGCBridgestoneInvitational_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_10 ")
TurningStoneResortChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Turning+Stone+Resort+Championship&player=&tour=PGA&submit=go","TurningStoneResortChampionship_10 ")
PGAChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_10 ")
WyndhamChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_10 ")
BarclaysClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_10 ")
DeutscheBankChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_10 ")
BMWChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_10 ")
TourChampionship_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_10 ")
VikingClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_10 ")
McGladreyClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=McGladrey+Classic&player=&tour=PGA&submit=go","McGladreyClassic_10 ")
FryscomOpen_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_10 ")
JustinTimberlakeShrinersHospitals_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_10 ")
ChildrensMiracleNetworkClassic_10 <- golfstat("https://www.golfstats.com/search/?yr=2010&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_10 ")

#2011 Season
HyundaiTournamentofChampions_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Hyundai+Tournament+of+Champions&player=&tour=PGA&submit=go","HyundaiTournamentofChampions_11 ")
SonyOpeninHawaii_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_11 ")
BobHopeClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Bob+Hope+Classic&player=&tour=PGA&submit=go","BobHopeClassic_11 ")
FarmersInsuranceOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_11 ")
WasteManagementPhoenixOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_11 ")
ATTPebbleBeach_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_11 ")
NorthernTrustOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_11 ")
WGCAccentureMatchPlayChampionship_11 <- matchplay("https://www.golfstats.com/search/?yr=2011&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_11 ")
MayakobaGolfClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_11 ")
HondaClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_11 ")
PuertoRicoOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_11 ")
WGCCadillacChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=WGC-Cadillac+Championship&player=&tour=PGA&submit=go","WGCCadillacChampionship_11 ")
TransitionsChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Transitions+Championship&player=&tour=PGA&submit=go","TransitionsChampionship_11 ")
ArnoldPalmerInvitational_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_11 ")
ShellHoustonOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_11 ")
Masters_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Masters&player=&tour=PGA&submit=go","Masters_11 ")
ValeroTexasOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_11 ")
TheHeritage_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=The+Heritage&player=&tour=PGA&submit=go","TheHeritage_11 ")
ZurichClassicofNewOrleans_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_11 ")
WellsFargoChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_11 ")
ThePlayers_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=The+Players&player=&tour=PGA&submit=go","ThePlayers_11 ")
CrownePlazaInvitationalatColonial_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_11 ")
HPByronNelsonChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_11 ")
MemorialTournament_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_11 ")
FedExStJudeClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_11 ")
USOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_11 ")
TravelersChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_11 ")
ATTNational_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_11 ")
JohnDeereClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_11 ")
VikingClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Viking+Classic&player=&tour=PGA&submit=go","VikingClassic_11 ")
BritishOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_11 ")
RBCCanadianOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_11 ")
TheGreenbrierClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_11 ")
WGCBridgestoneInvitational_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_11 ")
RenoTahoeOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Reno-Tahoe+Open&player=&tour=PGA&submit=go","RenoTahoeOpen_11 ")
PGAChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_11 ")
WyndhamChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_11 ")
TheBarclays_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=The+Barclays&player=&tour=PGA&submit=go","TheBarclays_11 ")
DeutscheBankChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_11 ")
BMWChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_11 ")
TourChampionship_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_11 ")
JustinTimberlakeShrinersHospitals_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_11 ")
FryscomOpen_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_11 ")
McGladreyClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=McGladrey+Classic&player=&tour=PGA&submit=go","McGladreyClassic_11 ")
ChildrensMiracleNetworkClassic_11 <- golfstat("https://www.golfstats.com/search/?yr=2011&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_11 ")

#2012 Season
HyundaiTournamentofChampions_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Hyundai+Tournament+of+Champions&player=&tour=PGA&submit=go","HyundaiTournamentofChampions_12 ")
SonyOpeninHawaii_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_12 ")
HumanaChallenge_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Humana+Challenge&player=&tour=PGA&submit=go","HumanaChallenge_12 ")
FarmersInsuranceOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_12 ")
WasteManagementPhoenixOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_12 ")
ATTPebbleBeachNationalProAm_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=AT%26T+Pebble+Beach+National+Pro-Am&player=&tour=PGA&submit=go","ATTPebbleBeachNationalProAm_12 ")
NorthernTrustOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_12 ")
WGCAccentureMatchPlayChampionship_12 <- matchplay("https://www.golfstats.com/search/?yr=2012&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_12 ")
MayakobaGolfClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_12 ")
HondaClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_12 ")
PuertoRicoOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_12 ")
WGCCadillacChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=WGC-Cadillac+Championship&player=&tour=PGA&submit=go","WGCCadillacChampionship_12 ")
TransitionsChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Transitions+Championship&player=&tour=PGA&submit=go","TransitionsChampionship_12 ")
ArnoldPalmerInvitational_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_12 ")
ShellHoustonOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_12 ")
Masters_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Masters&player=&tour=PGA&submit=go","Masters_12 ")
RBCHeritage_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_12 ")
ValeroTexasOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_12 ")
ZurichClassicofNewOrleans_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_12 ")
WellsFargoChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_12 ")
ThePlayersChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_12 ")
HPByronNelsonChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_12 ")
CrownePlazaInvitationalatColonial_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_12 ")
MemorialTournament_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_12 ")
FedExStJudeClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_12 ")
USOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_12 ")
TravelersChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_12 ")
ATTNational_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_12 ")
TheGreenbrierClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_12 ")
JohnDeereClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_12 ")
BritishOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_12 ")
TrueSouthClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=True+South+Classic&player=&tour=PGA&submit=go","TrueSouthClassic_12 ")
RBCCanadianOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_12 ")
WGCBridgestoneInvitational_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_12 ")
RenoTahoeOpen_12 <- barracuda("https://www.golfstats.com/search/?yr=2012&tournament=Reno-Tahoe+Open&player=&tour=PGA&submit=go","RenoTahoeOpen_12 ")
PGAChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_12 ")
WyndhamChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_12 ")
TheBarclays_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=The+Barclays&player=&tour=PGA&submit=go","TheBarclays_12 ")
DeutscheBankChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_12 ")
BMWChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_12 ")
TourChampionship_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_12 ")
JustinTimberlakeShrinersHospitals_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Justin+Timberlake+Shriners+Hospitals&player=&tour=PGA&submit=go","JustinTimberlakeShrinersHospitals_12 ")
FryscomOpen_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_12 ")
McGladreyClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=McGladrey+Classic&player=&tour=PGA&submit=go","McGladreyClassic_12 ")
ChildrensMiracleNetworkClassic_12 <- golfstat("https://www.golfstats.com/search/?yr=2012&tournament=Children%27s+Miracle+Network+Classic&player=&tour=PGA&submit=go","ChildrensMiracleNetworkClassic_12 ")

#2013 Season
HyundaiTournamentofChampions_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Hyundai+Tournament+of+Champions&player=&tour=PGA&submit=go","HyundaiTournamentofChampions_13 ")
SonyOpeninHawaii_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_13 ")
HumanaChallenge_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Humana+Challenge&player=&tour=PGA&submit=go","HumanaChallenge_13 ")
FarmersInsuranceOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_13 ")
WasteManagementPhoenixOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_13 ")
ATTPebbleBeachNationalProAm_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=AT%26T+Pebble+Beach+National+Pro-Am&player=&tour=PGA&submit=go","ATTPebbleBeachNationalProAm_13 ")
NorthernTrustOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_13 ")
WGCAccentureMatchPlayChampionship_13 <- matchplay("https://www.golfstats.com/search/?yr=2013&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_13 ")
HondaClassic_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_13 ")
WGCCadillacChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=WGC-Cadillac+Championship&player=&tour=PGA&submit=go","WGCCadillacChampionship_13 ")
PuertoRicoOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_13 ")
TampaBayChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Tampa+Bay+Championship&player=&tour=PGA&submit=go","TampaBayChampionship_13 ")
ArnoldPalmerInvitational_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_13 ")
ShellHoustonOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_13 ")
ValeroTexasOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_13 ")
Masters_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Masters&player=&tour=PGA&submit=go","Masters_13 ")
RBCHeritage_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_13 ")
ZurichClassicofNewOrleans_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_13 ")
WellsFargoChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_13 ")
ThePlayersChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_13 ")
HPByronNelsonChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_13 ")
CrownePlazaInvitationalatColonial_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_13 ")
MemorialTournament_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_13 ")
FedExStJudeClassic_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_13 ")
USOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_13 ")
TravelersChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_13 ")
ATTNational_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=AT%26T+National&player=&tour=PGA&submit=go","ATTNational_13 ")
TheGreenbrierClassic_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_13 ")
JohnDeereClassic_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_13 ")
BritishOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_13 ")
SandersonFarmsChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Sanderson+Farms+Championship&player=&tour=PGA&submit=go","SandersonFarmsChampionship_13 ")
RBCCanadianOpen_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_13 ")
WGCBridgestoneInvitational_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_13 ")
RenoTahoeOpen_13 <- barracuda("https://www.golfstats.com/search/?yr=2013&tournament=Reno-Tahoe+Open&player=&tour=PGA&submit=go","RenoTahoeOpen_13 ")
PGAChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_13 ")
WyndhamChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_13 ")
BarclaysClassic_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_13 ")
DeutscheBankChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_13 ")
BMWChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_13 ")
TourChampionship_13 <- golfstat("https://www.golfstats.com/search/?yr=2013&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_13 ")

#2014 Season
FryscomOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_14 ")
ShrinersHospitalsforChildrenOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Shriners+Hospitals+for+Children+Open&player=&tour=PGA&submit=go","ShrinersHospitalsforChildrenOpen_14 ")
CIMBClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=CIMB+Classic&player=&tour=PGA&submit=go","CIMBClassic_14 ")
WGCHSBCChampions_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_14 ")
WGCHSBCChampions_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_14 ")
McGladreyClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=McGladrey+Classic&player=&tour=PGA&submit=go","McGladreyClassic_14 ")
MayakobaGolfClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_14 ")
HyundaiTournamentofChampions_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Hyundai+Tournament+of+Champions&player=&tour=PGA&submit=go","HyundaiTournamentofChampions_14 ")
SonyOpeninHawaii_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_14 ")
HumanaChallenge_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Humana+Challenge&player=&tour=PGA&submit=go","HumanaChallenge_14 ")
FarmersInsuranceOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_14 ")
WasteManagementPhoenixOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_14 ")
ATTPebbleBeach_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_14 ")
NorthernTrustOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_14 ")
WGCAccentureMatchPlayChampionship_14 <- matchplay("https://www.golfstats.com/search/?yr=2014&tournament=WGC-Accenture+Match+Play+Championship&player=&tour=PGA&submit=go","WGCAccentureMatchPlayChampionship_14 ")
HondaClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_14 ")
WGCCadillacChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=WGC-Cadillac+Championship&player=&tour=PGA&submit=go","WGCCadillacChampionship_14 ")
PuertoRicoOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_14 ")
ValsparChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Valspar+Championship&player=&tour=PGA&submit=go","ValsparChampionship_14 ")
ArnoldPalmerInvitational_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_14 ")
ValeroTexasOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_14 ")
ShellHoustonOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_14 ")
Masters_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Masters&player=&tour=PGA&submit=go","Masters_14 ")
RBCHeritage_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_14 ")
ZurichClassicofNewOrleans_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_14 ")
WellsFargoChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_14 ")
ThePlayersChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_14 ")
HPByronNelsonChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=HP+Byron+Nelson+Championship&player=&tour=PGA&submit=go","HPByronNelsonChampionship_14 ")
CrownePlazaInvitationalatColonial_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_14 ")
MemorialTournament_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_14 ")
FedExStJudeClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_14 ")
USOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_14 ")
TravelersChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_14 ")
QuickenLoansNational_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Quicken+Loans+National&player=&tour=PGA&submit=go","QuickenLoansNational_14 ")
TheGreenbrierClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_14 ")
JohnDeereClassic_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_14 ")
BritishOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_14 ")
RBCCanadianOpen_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_14 ")
WGCBridgestoneInvitational_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_14 ")
BarracudaChampionship_14 <- barracuda("https://www.golfstats.com/search/?yr=2014&tournament=Barracuda+Championship&player=&tour=PGA&submit=go","BarracudaChampionship_14 ")
PGAChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_14 ")
WyndhamChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_14 ")
TheBarclays_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=The+Barclays&player=&tour=PGA&submit=go","TheBarclays_14 ")
DeutscheBankChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_14 ")
BMWChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_14 ")
TourChampionship_14 <- golfstat("https://www.golfstats.com/search/?yr=2014&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_14 ")

#2015 Season
FryscomOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_15 ")
ShrinersHospitalsforChildrenOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Shriners+Hospitals+for+Children+Open&player=&tour=PGA&submit=go","ShrinersHospitalsforChildrenOpen_15 ")
McGladreyClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=McGladrey+Classic&player=&tour=PGA&submit=go","McGladreyClassic_15 ")
CIMBClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=CIMB+Classic&player=&tour=PGA&submit=go","CIMBClassic_15 ")
SandersonFarmsChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Sanderson+Farms+Championship&player=&tour=PGA&submit=go","SandersonFarmsChampionship_15 ")
WGCHSBCChampions_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_15 ")
MayakobaGolfClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_15 ")
HyundaiTournamentofChampions_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Hyundai+Tournament+of+Champions&player=&tour=PGA&submit=go","HyundaiTournamentofChampions_15 ")
SonyOpeninHawaii_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_15 ")
HumanaChallenge_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Humana+Challenge&player=&tour=PGA&submit=go","HumanaChallenge_15 ")
WasteManagementPhoenixOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_15 ")
FarmersInsuranceOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_15 ")
ATTPebbleBeachNationalProAm_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=AT%26T+Pebble+Beach+National+Pro-Am&player=&tour=PGA&submit=go","ATTPebbleBeachNationalProAm_15 ")
NorthernTrustOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_15 ")
HondaClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_15 ")
PuertoRicoOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_15 ")
WGCCadillacChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=WGC-Cadillac+Championship&player=&tour=PGA&submit=go","WGCCadillacChampionship_15 ")
ValsparChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Valspar+Championship&player=&tour=PGA&submit=go","ValsparChampionship_15 ")
ArnoldPalmerInvitational_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_15 ")
ValeroTexasOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_15 ")
ShellHoustonOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_15 ")
Masters_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Masters&player=&tour=PGA&submit=go","Masters_15 ")
RBCHeritage_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_15 ")
ZurichClassicofNewOrleans_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_15 ")
WGCCadillacMatchPlayChampionship_15 <- matchplay("https://www.golfstats.com/search/?yr=2015&tournament=WGC-Cadillac+Match+Play+Championship&player=&tour=PGA&submit=go","WGCCadillacMatchPlayChampionship_15 ")
ThePlayersChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_15 ")
WellsFargoChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_15 ")
CrownePlazaInvitationalatColonial_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Crowne+Plaza+Invitational+at+Colonial&player=&tour=PGA&submit=go","CrownePlazaInvitationalatColonial_15 ")
ATTByronNelsonChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=AT%26T+Byron+Nelson+Championship&player=&tour=PGA&submit=go","ATTByronNelsonChampionship_15 ")
MemorialTournament_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_15 ")
FedExStJudeClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_15 ")
USOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_15 ")
TravelersChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_15 ")
TheGreenbrierClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_15 ")
JohnDeereClassic_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_15 ")
BarbasolChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Barbasol+Championship&player=&tour=PGA&submit=go","BarbasolChampionship_15 ")
TheOpenChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=The+Open+Championship&player=&tour=PGA&submit=go","TheOpenChampionship_15 ")
RBCCanadianOpen_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_15 ")
QuickenLoansNational_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Quicken+Loans+National&player=&tour=PGA&submit=go","QuickenLoansNational_15 ")
BarracudaChampionship_15 <- barracuda("https://www.golfstats.com/search/?yr=2015&tournament=Barracuda+Championship&player=&tour=PGA&submit=go","BarracudaChampionship_15 ")
WGCBridgestoneInvitational_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_15 ")
PGAChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_15 ")
WyndhamChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_15 ")
TheBarclays_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=The+Barclays&player=&tour=PGA&submit=go","TheBarclays_15 ")
DeutscheBankChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_15 ")
BMWChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_15 ")
TourChampionship_15 <- golfstat("https://www.golfstats.com/search/?yr=2015&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_15 ")

#2016 Season
FryscomOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Frys.com+Open&player=&tour=PGA&submit=go","FryscomOpen_16 ")
ShrinersHospitalsforChildrenOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Shriners+Hospitals+for+Children+Open&player=&tour=PGA&submit=go","ShrinersHospitalsforChildrenOpen_16 ")
CIMBClassic_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=CIMB+Classic&player=&tour=PGA&submit=go","CIMBClassic_16 ")
WGCHSBCChampions_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_16 ")
SandersonFarmsChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Sanderson+Farms+Championship&player=&tour=PGA&submit=go","SandersonFarmsChampionship_16 ")
OHLClassicatMayakoba_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=OHL+Classic+at+Mayakoba&player=&tour=PGA&submit=go","OHLClassicatMayakoba_16 ")
TheRSMClassic_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=The+RSM+Classic&player=&tour=PGA&submit=go","TheRSMClassic_16 ")
HyundaiTournamentofChampions_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Hyundai+Tournament+of+Champions&player=&tour=PGA&submit=go","HyundaiTournamentofChampions_16 ")
SonyOpeninHawaii_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_16 ")
CareerBuilderChallenge_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=CareerBuilder+Challenge&player=&tour=PGA&submit=go","CareerBuilderChallenge_16 ")
FarmersInsuranceOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_16 ")
WasteManagementPhoenixOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_16 ")
ATTPebbleBeachNationalProAm_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=AT%26T+Pebble+Beach+National+Pro-Am&player=&tour=PGA&submit=go","ATTPebbleBeachNationalProAm_16 ")
NorthernTrustOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Northern+Trust+Open&player=&tour=PGA&submit=go","NorthernTrustOpen_16 ")
HondaClassic_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_16 ")
WGCCadillacChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=WGC-Cadillac+Championship&player=&tour=PGA&submit=go","WGCCadillacChampionship_16 ")
ValsparChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Valspar+Championship&player=&tour=PGA&submit=go","ValsparChampionship_16 ")
ArnoldPalmerInvitational_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_16 ")
PuertoRicoOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_16 ")
WGCDellMatchPlayChampionship_16 <- matchplay("https://www.golfstats.com/search/?yr=2016&tournament=WGC-Dell+Match+Play+Championship&player=&tour=PGA&submit=go","WGCDellMatchPlayChampionship_16 ")
ShellHoustonOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_16 ")
Masters_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Masters&player=&tour=PGA&submit=go","Masters_16 ")
RBCHeritage_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_16 ")
ValeroTexasOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_16 ")
ZurichClassicofNewOrleans_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_16 ")
WellsFargoChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_16 ")
ThePlayersChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_16 ")
ATTByronNelson_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=AT%26T+Byron+Nelson&player=&tour=PGA&submit=go","ATTByronNelson_16 ")
DeanDeLucaInvitational_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Dean+%26+DeLuca+Invitational&player=&tour=PGA&submit=go","DeanDeLucaInvitational_16 ")
MemorialTournament_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_16 ")
FedExStJudeClassic_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_16 ")
USOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_16 ")
QuickLoansNational_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Quick+Loans+National&player=&tour=PGA&submit=go","QuickLoansNational_16 ")
WGCBridgestoneInvitational_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=WGC+-+Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_16 ")
BarracudaChampionship_16 <- barracuda("https://www.golfstats.com/search/?yr=2016&tournament=Barracuda+Championship&player=&tour=PGA&submit=go","BarracudaChampionship_16 ")
TheOpenChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=The+Open+Championship&player=&tour=PGA&submit=go","TheOpenChampionship_16 ")
BarbasolChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Barbasol+Championship&player=&tour=PGA&submit=go","BarbasolChampionship_16 ")
RBCCanadianOpen_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_16 ")
PGAChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_16 ")
TravelersChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_16 ")
JohnDeereClassic_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_16 ")
WyndhamChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_16 ")
BarclaysClassic_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Barclays+Classic&player=&tour=PGA&submit=go","BarclaysClassic_16 ")
DeutscheBankChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Deutsche+Bank+Championship&player=&tour=PGA&submit=go","DeutscheBankChampionship_16 ")
BMWChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_16 ")
TourChampionship_16 <- golfstat("https://www.golfstats.com/search/?yr=2016&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_16 ")

#2017 Season
SafewayOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Safeway+Open&player=&tour=PGA&submit=go","SafewayOpen_17 ")
CIMBClassic_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=CIMB+Classic&player=&tour=PGA&submit=go","CIMBClassic_17 ")
SandersonFarmsChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Sanderson+Farms+Championship&player=&tour=PGA&submit=go","SandersonFarmsChampionship_17 ")
WGCHSBCChampions_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_17 ")
ShrinersHospitalsforChildrenOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Shriners+Hospitals+for+Children+Open&player=&tour=PGA&submit=go","ShrinersHospitalsforChildrenOpen_17 ")
OHLClassicatMayakoba_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=OHL+Classic+at+Mayakoba&player=&tour=PGA&submit=go","OHLClassicatMayakoba_17 ")
TheRSMClassic_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=The+RSM+Classic&player=&tour=PGA&submit=go","TheRSMClassic_17 ")
SBSTournamentofChampions_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=SBS+Tournament+of+Champions&player=&tour=PGA&submit=go","SBSTournamentofChampions_17 ")
SonyOpeninHawaii_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_17 ")
CareerBuilderChallenge_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=CareerBuilder+Challenge&player=&tour=PGA&submit=go","CareerBuilderChallenge_17 ")
FarmersInsuranceOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_17 ")
WasteManagementPhoenixOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_17 ")
ATTPebbleBeach_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=AT%26T+Pebble+Beach&player=&tour=PGA&submit=go","ATTPebbleBeach_17 ")
GenesisOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Genesis+Open&player=&tour=PGA&submit=go","GenesisOpen_17 ")
TheHondaClassic_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=The+Honda+Classic&player=&tour=PGA&submit=go","TheHondaClassic_17 ")
WGCMexicoChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=WGC-Mexico+Championship&player=&tour=PGA&submit=go","WGCMexicoChampionship_17 ")
ValsparChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Valspar+Championship&player=&tour=PGA&submit=go","ValsparChampionship_17 ")
ArnoldPalmerInvitational_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_17 ")
WGCDellMatchPlayChampionship_17 <- matchplay("https://www.golfstats.com/search/?yr=2017&tournament=WGC-Dell+Match+Play+Championship&player=&tour=PGA&submit=go","WGCDellMatchPlayChampionship_17 ")
PuertoRicoOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_17 ")
ShellHoustonOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Shell+Houston+Open&player=&tour=PGA&submit=go","ShellHoustonOpen_17 ")
Masters_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Masters&player=&tour=PGA&submit=go","Masters_17 ")
RBCHeritage_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_17 ")
ValeroTexasOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_17 ")
ZurichClassicofNewOrleans_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_17 ")
ZurichClassicofNewOrleans_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_17 ")
WellsFargoChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_17 ")
ThePlayersChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_17 ")
ATTByronNelson_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=AT%26T+Byron+Nelson&player=&tour=PGA&submit=go","ATTByronNelson_17 ")
DeanDeLucaInvitational_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Dean+%26+DeLuca+Invitational&player=&tour=PGA&submit=go","DeanDeLucaInvitational_17 ")
MemorialTournament_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_17 ")
FedExStJudeClassic_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_17 ")
USOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_17 ")
TravelersChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_17 ")
QuickenLoansNational_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Quicken+Loans+National&player=&tour=PGA&submit=go","QuickenLoansNational_17 ")
TheGreenbrierClassic_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=The+Greenbrier+Classic&player=&tour=PGA&submit=go","TheGreenbrierClassic_17 ")
JohnDeereClassic_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_17 ")
BarbasolChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Barbasol+Championship&player=&tour=PGA&submit=go","BarbasolChampionship_17 ")
BritishOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_17 ")
RBCCanadianOpen_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_17 ")
WGCBridgestoneInvitational_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_17 ")
BarracudaChampionship_17 <- barracuda("https://www.golfstats.com/search/?yr=2017&tournament=Barracuda+Championship&player=&tour=PGA&submit=go","BarracudaChampionship_17 ")
PGAChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_17 ")
WyndhamChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_17 ")
TheNorthernTrust_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=The+Northern+Trust&player=&tour=PGA&submit=go","TheNorthernTrust_17 ")
DellTechnologiesChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Dell+Technologies+Championship&player=&tour=PGA&submit=go","DellTechnologiesChampionship_17 ")
BMWChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_17 ")
TourChampionship_17 <- golfstat("https://www.golfstats.com/search/?yr=2017&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_17 ")

#2018 Season
SafewayOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Safeway+Open&player=&tour=PGA&submit=go","SafewayOpen_18 ")
CIMBClassic_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=CIMB+Classic&player=&tour=PGA&submit=go","CIMBClassic_18 ")
TheCJCupatNineBridges_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=The+CJ+Cup+at+Nine+Bridges&player=&tour=PGA&submit=go","TheCJCupatNineBridges_18 ")
WGCHSBCChampions_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_18 ")
SandersonFarmsChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Sanderson+Farms+Championship&player=&tour=PGA&submit=go","SandersonFarmsChampionship_18 ")
ShrinersHospitalsforChildrenOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Shriners+Hospitals+for+Children+Open&player=&tour=PGA&submit=go","ShrinersHospitalsforChildrenOpen_18 ")
OHLClassicatMayakoba_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=OHL+Classic+at+Mayakoba&player=&tour=PGA&submit=go","OHLClassicatMayakoba_18 ")
TheRSMClassic_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=The+RSM+Classic&player=&tour=PGA&submit=go","TheRSMClassic_18 ")
SentryTournamentofChampions_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Sentry+Tournament+of+Champions&player=&tour=PGA&submit=go","SentryTournamentofChampions_18 ")
SonyOpeninHawaii_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_18 ")
CareerBuilderChallenge_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=CareerBuilder+Challenge&player=&tour=PGA&submit=go","CareerBuilderChallenge_18 ")
FarmersInsuranceOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_18 ")
WasteManagementPhoenixOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_18 ")
ATTPebbleBeachProAm_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=AT%26T+Pebble+Beach+Pro-Am&player=&tour=PGA&submit=go","ATTPebbleBeachProAm_18 ")
GenesisOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Genesis+Open&player=&tour=PGA&submit=go","GenesisOpen_18 ")
HondaClassic_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_18 ")
WGCMexicoChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=WGC-Mexico+Championship&player=&tour=PGA&submit=go","WGCMexicoChampionship_18 ")
ValsparChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Valspar+Championship&player=&tour=PGA&submit=go","ValsparChampionship_18 ")
ArnoldPalmerInvitational_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_18 ")
WGCDellMatchPlayChampionship_18 <- matchplay("https://www.golfstats.com/search/?yr=2018&tournament=WGC+-+Dell+Match+Play+Championship&player=&tour=PGA&submit=go","WGCDellMatchPlayChampionship_18 ")
CoralesPuntacanResortChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Corales+Puntacan+Resort+Championship&player=&tour=PGA&submit=go","CoralesPuntacanResortChampionship_18 ")
HoustonOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Houston+Open&player=&tour=PGA&submit=go","HoustonOpen_18 ")
Masters_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Masters&player=&tour=PGA&submit=go","Masters_18 ")
RBCHeritage_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_18 ")
ValeroTexasOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_18 ")
ZurichClassicofNewOrleans_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_18 ")
ZurichClassicofNewOrleans_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_18 ")
WellsFargoChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_18 ")
ThePlayersChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_18 ")
ATTByronNelson_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=AT%26T+Byron+Nelson&player=&tour=PGA&submit=go","ATTByronNelson_18 ")
FortWorthInvitational_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Fort+Worth+Invitational&player=&tour=PGA&submit=go","FortWorthInvitational_18 ")
MemorialTournament_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_18 ")
FedExStJudeClassic_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=FedEx+St.+Jude+Classic&player=&tour=PGA&submit=go","FedExStJudeClassic_18 ")
USOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_18 ")
TravelersChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_18 ")
QuickenLoansNational_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Quicken+Loans+National&player=&tour=PGA&submit=go","QuickenLoansNational_18 ")
AMilitaryTributeatTheGreenbrier_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=A+Military+Tribute+at+The+Greenbrier&player=&tour=PGA&submit=go","AMilitaryTributeatTheGreenbrier_18 ")
JohnDeereClassic_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_18 ")
TheOpenChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=The+Open+Championship&player=&tour=PGA&submit=go","TheOpenChampionship_18 ")
BarbasolChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Barbasol+Championship&player=&tour=PGA&submit=go","BarbasolChampionship_18 ")
RBCCanadianOpen_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_18 ")
BarracudaChampionship_18 <- barracuda("https://www.golfstats.com/search/?yr=2018&tournament=Barracuda+Championship&player=&tour=PGA&submit=go","BarracudaChampionship_18 ")
WGCBridgestoneInvitational_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=WGC-Bridgestone+Invitational&player=&tour=PGA&submit=go","WGCBridgestoneInvitational_18 ")
PGAChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_18 ")
WyndhamChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_18 ")
TheNorthernTrust_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=The+Northern+Trust&player=&tour=PGA&submit=go","TheNorthernTrust_18 ")
DellTechnologiesChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Dell+Technologies+Championship&player=&tour=PGA&submit=go","DellTechnologiesChampionship_18 ")
BMWChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_18 ")
TourChampionship_18 <- golfstat("https://www.golfstats.com/search/?yr=2018&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_18 ")

#2019 Season
SafewayOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Safeway+Open&player=&tour=PGA&submit=go","SafewayOpen_19 ")
CIMBClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=CIMB+Classic&player=&tour=PGA&submit=go","CIMBClassic_19 ")
TheCJCupatNineBridges_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=The+CJ+Cup+at+Nine+Bridges&player=&tour=PGA&submit=go","TheCJCupatNineBridges_19 ")
WGCHSBCChampions_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=WGC-HSBC+Champions&player=&tour=PGA&submit=go","WGCHSBCChampions_19 ")
SandersonFarmsChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Sanderson+Farms+Championship&player=&tour=PGA&submit=go","SandersonFarmsChampionship_19 ")
ShrinersHospitalsforChildrenOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Shriners+Hospitals+for+Children+Open&player=&tour=PGA&submit=go","ShrinersHospitalsforChildrenOpen_19 ")
MayakobaGolfClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Mayakoba+Golf+Classic&player=&tour=PGA&submit=go","MayakobaGolfClassic_19 ")
TheRSMClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=The+RSM+Classic&player=&tour=PGA&submit=go","TheRSMClassic_19 ")
SentryTournamentofChampions_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Sentry+Tournament+of+Champions&player=&tour=PGA&submit=go","SentryTournamentofChampions_19 ")
SonyOpeninHawaii_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Sony+Open+in+Hawaii&player=&tour=PGA&submit=go","SonyOpeninHawaii_19 ")
DesertClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Desert+Classic&player=&tour=PGA&submit=go","DesertClassic_19 ")
FarmersInsuranceOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Farmers+Insurance+Open&player=&tour=PGA&submit=go","FarmersInsuranceOpen_19 ")
WasteManagementPhoenixOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Waste+Management+Phoenix+Open&player=&tour=PGA&submit=go","WasteManagementPhoenixOpen_19 ")
ATTPebbleBeachProAm_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=AT%26T+Pebble+Beach+Pro-Am&player=&tour=PGA&submit=go","ATTPebbleBeachProAm_19 ")
GenesisOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Genesis+Open&player=&tour=PGA&submit=go","GenesisOpen_19 ")
WGCMexicoChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=WGC+-+Mexico+Championship&player=&tour=PGA&submit=go","WGCMexicoChampionship_19 ")
PuertoRicoOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Puerto+Rico+Open&player=&tour=PGA&submit=go","PuertoRicoOpen_19 ")
HondaClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Honda+Classic&player=&tour=PGA&submit=go","HondaClassic_19 ")
ArnoldPalmerInvitational_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Arnold+Palmer+Invitational&player=&tour=PGA&submit=go","ArnoldPalmerInvitational_19 ")
ThePlayersChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=The+Players+Championship&player=&tour=PGA&submit=go","ThePlayersChampionship_19 ")
ValsparChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Valspar+Championship&player=&tour=PGA&submit=go","ValsparChampionship_19 ")
WGCDellMatchPlayChampionship_19 <- matchplay("https://www.golfstats.com/search/?yr=2019&tournament=WGC-Dell+Match+Play+Championship&player=&tour=PGA&submit=go","WGCDellMatchPlayChampionship_19 ")
CoralesPuntacanResortChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Corales+Puntacan+Resort+Championship&player=&tour=PGA&submit=go","CoralesPuntacanResortChampionship_19 ")
ValeroTexasOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Valero+Texas+Open&player=&tour=PGA&submit=go","ValeroTexasOpen_19 ")
Masters_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Masters&player=&tour=PGA&submit=go","Masters_19 ")
RBCHeritage_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=RBC+Heritage&player=&tour=PGA&submit=go","RBCHeritage_19 ")
ZurichClassicofNewOrleans_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_19 ")
ZurichClassicofNewOrleans_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Zurich+Classic+of+New+Orleans&player=&tour=PGA&submit=go","ZurichClassicofNewOrleans_19 ")
WellsFargoChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Wells+Fargo+Championship&player=&tour=PGA&submit=go","WellsFargoChampionship_19 ")
ATTByronNelson_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=AT%26T+Byron+Nelson&player=&tour=PGA&submit=go","ATTByronNelson_19 ")
PGAChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=PGA+Championship&player=&tour=PGA&submit=go","PGAChampionship_19 ")
CharlesSchwabChallenge_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Charles+Schwab+Challenge&player=&tour=PGA&submit=go","CharlesSchwabChallenge_19 ")
MemorialTournament_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Memorial+Tournament&player=&tour=PGA&submit=go","MemorialTournament_19 ")
RBCCanadianOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=RBC+Canadian+Open&player=&tour=PGA&submit=go","RBCCanadianOpen_19 ")
USOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=U.S.+Open&player=&tour=PGA&submit=go","USOpen_19 ")
TravelersChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Travelers+Championship&player=&tour=PGA&submit=go","TravelersChampionship_19 ")
RocketMortgageClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Rocket+Mortgage+Classic&player=&tour=PGA&submit=go","RocketMortgageClassic_19 ")
MOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=3M+Open&player=&tour=PGA&submit=go","MOpen_19 ")
JohnDeereClassic_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=John+Deere+Classic&player=&tour=PGA&submit=go","JohnDeereClassic_19 ")
BritishOpen_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=British+Open&player=&tour=PGA&submit=go","BritishOpen_19 ")
BarbasolChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Barbasol+Championship&player=&tour=PGA&submit=go","BarbasolChampionship_19 ")
WGCFedExStJudeInvitational_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=WGC-FedEx+St.+Jude+Invitational&player=&tour=PGA&submit=go","WGCFedExStJudeInvitational_19 ")
BarracudaChampionship_19 <- barracuda("https://www.golfstats.com/search/?yr=2019&tournament=Barracuda+Championship&player=&tour=PGA&submit=go","BarracudaChampionship_19 ")
WyndhamChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Wyndham+Championship&player=&tour=PGA&submit=go","WyndhamChampionship_19 ")
TheNorthernTrust_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=The+Northern+Trust&player=&tour=PGA&submit=go","TheNorthernTrust_19 ")
BMWChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=BMW+Championship&player=&tour=PGA&submit=go","BMWChampionship_19 ")
TourChampionship_19 <- golfstat("https://www.golfstats.com/search/?yr=2019&tournament=Tour+Championship&player=&tour=PGA&submit=go","TourChampionship_19 ")

#The Tour Championship used a different format that gave golfers a head start. I had to add names back in due to the data cleaning process removing them since the data was not structured like other tournaments.

TourChampionship_19$Player <- c("Rory McIlroy","Xander Schauffele","Brooks Koepka","Justin Thomas","Paul Casey","Adam Scott","Tony Finau","Chez Reavie","Kevin Kisner",
"Hideki Matsuyama","Patrick Reed","Bryson DeChambeau","Jon Rahm","Jason Kokrak","Gary Woodland","Tommy Fleetwood","Matt Kuchar","Webb Simpson","Sungjae Im","Rickie Fowler","Louis Oosthuizen","Abraham Ancer","Patrick Cantlay","Marc Leishman","Brandt Snedeker","Corey Conners","Justin Rose","Charles Howell III","Lucas Glover","Dustin Johnson")

#Combine season results

#1996
pga_1996_results <- do.call("rbind", list(AirCanadaChampionship_96,ArnoldPalmerInvitational_96,ATTClassic_96,BarclaysClassic_96,BCOpen_96,BMWChampionship_96,BobHopeChryslerClassic_96,BoozAllenClassic_96,BritishOpen_96,BuickChallenge_96,BuickInvitational_96,BuickOpen_96,ChildrensMiracleNetworkClassic_96,ChryslerClassicTucson_96,CrownePlazaInvitationalatColonial_96,CVSCharityClassic_96,EDSByronNelsonClassic_96,FBROpen_96,FordChampionshipatDoral_96,HondaClassic_96,JohnDeereClassic_96,JustinTimberlakeShrinersHospitals_96,Masters_96,MemorialTournament_96,MercedesBenzChampionship_96,MichelobChampionshipatKingsmill_96,NECWorldSeriesofGolf_96,NorthernTrustOpen_96,PGAChampionship_96,RBCCanadianOpen_96,ShellHoustonOpen_96,SonyOpeninHawaii_96,StanfordStJudeClassic_96,TheInternational_96,ThePlayersChampionship_96,TourChampionship_96,TravelersChampionship_96,USBankChampionshipinMilwaukee_96,USOpen_96,ValeroTexasOpen_96,VerizonHeritage_96,VikingClassic_96,WyndhamChampionship_96,ZurichClassicofNewOrleans_96))

#1997
pga_1997_results <- do.call("rbind",list(AirCanadaChampionship_97,ArnoldPalmerInvitational_97,ATTClassic_97,ATTPebbleBeach_97,BarclaysClassic_97,BCOpen_97,BMWChampionship_97,BobHopeChryslerClassic_97,BoozAllenClassic_97,BritishOpen_97,BuickChallenge_97,BuickInvitational_97,BuickOpen_97,ChryslerClassicTucson_97,CrownePlazaInvitationalatColonial_97,CVSCharityClassic_97,EDSByronNelsonClassic_97,FBROpen_97,FordChampionshipatDoral_97,HondaClassic_97,JohnDeereClassic_97,JustinTimberlakeShrinersHospitals_97,Masters_97,MemorialTournament_97,MercedesBenzChampionship_97,MichelobChampionshipatKingsmill_97,NECWorldSeriesofGolf_97,NorthernTrustOpen_97,PGAChampionship_97,RBCCanadianOpen_97,ShellHoustonOpen_97,SonyOpeninHawaii_97,StanfordStJudeClassic_97,TheInternational_97,ThePlayersChampionship_97,TourChampionship_97,TravelersChampionship_97,USBankChampionshipinMilwaukee_97,USOpen_97,ValeroTexasOpen_97,VerizonHeritage_97,VikingClassic_97,WyndhamChampionship_97,ZurichClassicofNewOrleans_97))

#1998
pga_1998_results <- do.call('rbind',list(AirCanadaChampionship_98,ArnoldPalmerInvitational_98,ATTClassic_98,ATTPebbleBeach_98,BarclaysClassic_98,BCOpen_98,BMWChampionship_98,BobHopeChryslerClassic_98,BoozAllenClassic_98,BritishOpen_98,BuickChallenge_98,BuickInvitational_98,BuickOpen_98,ChildrensMiracleNetworkClassic_98,ChryslerClassicTucson_98,CrownePlazaInvitationalatColonial_98,CVSCharityClassic_98,EDSByronNelsonClassic_98,FBROpen_98,FordChampionshipatDoral_98,HondaClassic_98,JohnDeereClassic_98,JustinTimberlakeShrinersHospitals_98,Masters_98,MemorialTournament_98,MercedesBenzChampionship_98,MichelobChampionshipatKingsmill_98,NECWorldSeriesofGolf_98,NorthernTrustOpen_98,PGAChampionship_98,RBCCanadianOpen_98,ShellHoustonOpen_98,SonyOpeninHawaii_98,StanfordStJudeClassic_98,TheInternational_98,ThePlayersChampionship_98,TourChampionship_98,TravelersChampionship_98,USBankChampionshipinMilwaukee_98,USOpen_98,ValeroTexasOpen_98,VerizonHeritage_98,VikingClassic_98,WyndhamChampionship_98,ZurichClassicofNewOrleans_98))

#1999
pga_1999_results <- do.call('rbind',list(AirCanadaChampionship_99,ArnoldPalmerInvitational_99,ATTClassic_99,ATTPebbleBeach_99,BarclaysClassic_99,BCOpen_99,BMWChampionship_99,BobHopeChryslerClassic_99,BoozAllenClassic_99,BritishOpen_99,BuickChallenge_99,BuickInvitational_99,BuickOpen_99,ChildrensMiracleNetworkClassic_99,ChryslerClassicTucson_99,CrownePlazaInvitationalatColonial_99,EDSByronNelsonClassic_99,FBROpen_99,FordChampionshipatDoral_99,HondaClassic_99,JohnDeereClassic_99,JustinTimberlakeShrinersHospitals_99,LegendsRenoTahoeOpen_99,Masters_99,MemorialTournament_99,MercedesBenzChampionship_99,MichelobChampionshipatKingsmill_99,NorthernTrustOpen_99,PGAChampionship_99,RBCCanadianOpen_99,ShellHoustonOpen_99,SonyOpeninHawaii_99,StanfordStJudeClassic_99,TheInternational_99,ThePlayersChampionship_99,TourChampionship_99,TravelersChampionship_99,USBankChampionshipinMilwaukee_99,USOpen_99,ValeroTexasOpen_99,VerizonHeritage_99,VikingClassic_99,WGCAccentureMatchPlayChampionship_99,WGCBridgestoneInvitational_99,WGCCAChampionship_99,WyndhamChampionship_99,ZurichClassicofNewOrleans_99))

#2000
pga_2000_results <- do.call('rbind',list(AirCanadaChampionship_00,ArnoldPalmerInvitational_00,ATTClassic_00,ATTPebbleBeach_00,BarclaysClassic_00,BCOpen_00,BMWChampionship_00,BobHopeChryslerClassic_00,BoozAllenClassic_00,BritishOpen_00,BuickChallenge_00,BuickInvitational_00,BuickOpen_00,ChildrensMiracleNetworkClassic_00,ChryslerClassicTucson_00,CrownePlazaInvitationalatColonial_00,EDSByronNelsonClassic_00,FBROpen_00,FordChampionshipatDoral_00,HondaClassic_00,JohnDeereClassic_00,JustinTimberlakeShrinersHospitals_00,LegendsRenoTahoeOpen_00,LumberClassic_00,Masters_00,MemorialTournament_00,MercedesBenzChampionship_00,MichelobChampionshipatKingsmill_00,NorthernTrustOpen_00,PGAChampionship_00,PODSChampionship_00,RBCCanadianOpen_00,ShellHoustonOpen_00,SonyOpeninHawaii_00,StanfordStJudeClassic_00,TheInternational_00,ThePlayersChampionship_00,TourChampionship_00,TravelersChampionship_00,USBankChampionshipinMilwaukee_00,USOpen_00,ValeroTexasOpen_00,VerizonHeritage_00,VikingClassic_00,WGCAccentureMatchPlayChampionship_00,WGCBridgestoneInvitational_00,WGCCAChampionship_00,WyndhamChampionship_00,ZurichClassicofNewOrleans_00))

#2001
pga_2001_results <- do.call('rbind',list(AirCanadaChampionship_01,ArnoldPalmerInvitational_01,ATTClassic_01,ATTPebbleBeach_01,BarclaysClassic_01,BCOpen_01,BMWChampionship_01,BobHopeChryslerClassic_01,BoozAllenClassic_01,BritishOpen_01,BuickChallenge_01,BuickInvitational_01,BuickOpen_01,ChildrensMiracleNetworkClassic_01,ChryslerClassicTucson_01,CrownePlazaInvitationalatColonial_01,EDSByronNelsonClassic_01,FBROpen_01,FordChampionshipatDoral_01,HondaClassic_01,JohnDeereClassic_01,JustinTimberlakeShrinersHospitals_01,LegendsRenoTahoeOpen_01,LumberClassic_01,Masters_01,MemorialTournament_01,MercedesBenzChampionship_01,MichelobChampionshipatKingsmill_01,NorthernTrustOpen_01,PGAChampionship_01,RBCCanadianOpen_01,ShellHoustonOpen_01,SonyOpeninHawaii_01,StanfordStJudeClassic_01,TheInternational_01,ThePlayersChampionship_01,TourChampionship_01,TravelersChampionship_01,USBankChampionshipinMilwaukee_01,USOpen_01,ValeroTexasOpen_01,VerizonHeritage_01,VikingClassic_01,WGCAccentureMatchPlayChampionship_01,WGCBridgestoneInvitational_01,WyndhamChampionship_01,ZurichClassicofNewOrleans_01))

#2002
pga_2002_results <- do.call('rbind',list(AirCanadaChampionship_02,ArnoldPalmerInvitational_02,ATTClassic_02,ATTPebbleBeach_02,BarclaysClassic_02,BCOpen_02,BMWChampionship_02,BobHopeChryslerClassic_02,BoozAllenClassic_02,BritishOpen_02,BuickChallenge_02,BuickInvitational_02,BuickOpen_02,ChildrensMiracleNetworkClassic_02,ChryslerClassicTucson_02,CrownePlazaInvitationalatColonial_02,EDSByronNelsonClassic_02,FBROpen_02,FordChampionshipatDoral_02,HondaClassic_02,JohnDeereClassic_02,JustinTimberlakeShrinersHospitals_02,LegendsRenoTahoeOpen_02,LumberClassic_02,Masters_02,MemorialTournament_02,MercedesBenzChampionship_02,MichelobChampionshipatKingsmill_02,NorthernTrustOpen_02,PGAChampionship_02,PODSChampionship_02,RBCCanadianOpen_02,ShellHoustonOpen_02,SonyOpeninHawaii_02,StanfordStJudeClassic_02,TheInternational_02,ThePlayersChampionship_02,TourChampionship_02,TravelersChampionship_02,USBankChampionshipinMilwaukee_02,USOpen_02,ValeroTexasOpen_02,VerizonHeritage_02,VikingClassic_02,WGCAccentureMatchPlayChampionship_02,WGCBridgestoneInvitational_02,WGCCAChampionship_02,WyndhamChampionship_02,ZurichClassicofNewOrleans_02))

#2003
pga_2003_results <- do.call('rbind',list(ArnoldPalmerInvitational_03,ATTClassic_03,ATTPebbleBeach_03,BarclaysClassic_03,BCOpen_03,BMWChampionship_03,BobHopeChryslerClassic_03,BoozAllenClassic_03,BritishOpen_03,BuickInvitational_03,BuickOpen_03,ChildrensMiracleNetworkClassic_03,ChryslerClassicTucson_03,CrownePlazaInvitationalatColonial_03,DeutscheBankChampionship_03,EDSByronNelsonClassic_03,FBROpen_03,FordChampionshipatDoral_03,HondaClassic_03,JohnDeereClassic_03,JustinTimberlakeShrinersHospitals_03,LegendsRenoTahoeOpen_03,LumberClassic_03,Masters_03,MemorialTournament_03,MercedesBenzChampionship_03,NorthernTrustOpen_03,PGAChampionship_03,PODSChampionship_03,RBCCanadianOpen_03,ShellHoustonOpen_03,SonyOpeninHawaii_03,StanfordStJudeClassic_03,TheInternational_03,ThePlayersChampionship_03,TourChampionship_03,TravelersChampionship_03,USBankChampionshipinMilwaukee_03,USOpen_03,ValeroTexasOpen_03,VerizonHeritage_03,VikingClassic_03,WachoviaChampionship_03,WGCAccentureMatchPlayChampionship_03,WGCBridgestoneInvitational_03,WGCCAChampionship_03,WyndhamChampionship_03,ZurichClassicofNewOrleans_03))

#2004
pga_2004_results <- do.call('rbind',list(ArnoldPalmerInvitational_04,ATTClassic_04,ATTPebbleBeach_04,BarclaysClassic_04,BCOpen_04,BMWChampionship_04,BobHopeChryslerClassic_04,BoozAllenClassic_04,BritishOpen_04,BuickInvitational_04,BuickOpen_04,ChildrensMiracleNetworkClassic_04,ChryslerClassicTucson_04,CrownePlazaInvitationalatColonial_04,DeutscheBankChampionship_04,EDSByronNelsonClassic_04,FBROpen_04,FordChampionshipatDoral_04,HondaClassic_04,JohnDeereClassic_04,JustinTimberlakeShrinersHospitals_04,LegendsRenoTahoeOpen_04,LumberClassic_04,Masters_04,MemorialTournament_04,MercedesBenzChampionship_04,NorthernTrustOpen_04,PGAChampionship_04,PODSChampionship_04,RBCCanadianOpen_04,ShellHoustonOpen_04,SonyOpeninHawaii_04,StanfordStJudeClassic_04,TheInternational_04,ThePlayersChampionship_04,TourChampionship_04,TravelersChampionship_04,USBankChampionshipinMilwaukee_04,USOpen_04,ValeroTexasOpen_04,VerizonHeritage_04,VikingClassic_04,WachoviaChampionship_04,WGCAccentureMatchPlayChampionship_04,WGCBridgestoneInvitational_04,WGCCAChampionship_04,WyndhamChampionship_04,ZurichClassicofNewOrleans_04))

#2005
pga_2005_results <- do.call('rbind',list(ArnoldPalmerInvitational_05,ATTClassic_05,ATTPebbleBeach_05,BarclaysClassic_05,BCOpen_05,BMWChampionship_05,BobHopeChryslerClassic_05,BoozAllenClassic_05,BritishOpen_05,BuickInvitational_05,BuickOpen_05,ChildrensMiracleNetworkClassic_05,ChryslerClassicTucson_05,CrownePlazaInvitationalatColonial_05,DeutscheBankChampionship_05,EDSByronNelsonClassic_05,FBROpen_05,FordChampionshipatDoral_05,HondaClassic_05,JohnDeereClassic_05,JustinTimberlakeShrinersHospitals_05,LegendsRenoTahoeOpen_05,LumberClassic_05,Masters_05,MemorialTournament_05,MercedesBenzChampionship_05,NorthernTrustOpen_05,PGAChampionship_05,PODSChampionship_05,RBCCanadianOpen_05,ShellHoustonOpen_05,SonyOpeninHawaii_05,StanfordStJudeClassic_05,TheInternational_05,ThePlayersChampionship_05,TourChampionship_05,TravelersChampionship_05,USBankChampionshipinMilwaukee_05,USOpen_05,ValeroTexasOpen_05,VerizonHeritage_05,VikingClassic_05,WachoviaChampionship_05,WGCAccentureMatchPlayChampionship_05,WGCBridgestoneInvitational_05,WGCCAChampionship_05,WyndhamChampionship_05,ZurichClassicofNewOrleans_05))

#2006
pga_2006_results <- do.call('rbind',list(ArnoldPalmerInvitational_06,ATTClassic_06,ATTPebbleBeach_06,BarclaysClassic_06,BCOpen_06,BMWChampionship_06,BobHopeChryslerClassic_06,BoozAllenClassic_06,BritishOpen_06,BuickInvitational_06,BuickOpen_06,ChildrensMiracleNetworkClassic_06,ChryslerClassicTucson_06,CrownePlazaInvitationalatColonial_06,DeutscheBankChampionship_06,EDSByronNelsonClassic_06,FBROpen_06,FordChampionshipatDoral_06,HondaClassic_06,JohnDeereClassic_06,JustinTimberlakeShrinersHospitals_06,LegendsRenoTahoeOpen_06,LumberClassic_06,Masters_06,MemorialTournament_06,MercedesBenzChampionship_06,NorthernTrustOpen_06,PGAChampionship_06,PODSChampionship_06,RBCCanadianOpen_06,ShellHoustonOpen_06,SonyOpeninHawaii_06,StanfordStJudeClassic_06,TheInternational_06,ThePlayersChampionship_06,TourChampionship_06,TravelersChampionship_06,USBankChampionshipinMilwaukee_06,USOpen_06,ValeroTexasOpen_06,VerizonHeritage_06,VikingClassic_06,WachoviaChampionship_06,WGCAccentureMatchPlayChampionship_06,WGCBridgestoneInvitational_06,WGCCAChampionship_06,WyndhamChampionship_06,ZurichClassicofNewOrleans_06))

#2007
pga_2007_results <- do.call('rbind',list(ArnoldPalmerInvitational_07,ATTClassic_07,ATTNational_07,ATTPebbleBeach_07,BarclaysClassic_07,BMWChampionship_07,BobHopeChryslerClassic_07,BritishOpen_07,BuickInvitational_07,BuickOpen_07,ChildrensMiracleNetworkClassic_07,CrownePlazaInvitationalatColonial_07,DeutscheBankChampionship_07,EDSByronNelsonClassic_07,FBROpen_07,FryscomOpen_07,GinnsurMerClassic_07,HondaClassic_07,JohnDeereClassic_07,JustinTimberlakeShrinersHospitals_07,LegendsRenoTahoeOpen_07,Masters_07,MayakobaGolfClassic_07,MemorialTournament_07,MercedesBenzChampionship_07,NorthernTrustOpen_07,PGAChampionship_07,PODSChampionship_07,RBCCanadianOpen_07,ShellHoustonOpen_07,SonyOpeninHawaii_07,StanfordStJudeClassic_07,ThePlayersChampionship_07,TourChampionship_07,TravelersChampionship_07,TurningStoneResortChampionship_07,USBankChampionshipinMilwaukee_07,USOpen_07,ValeroTexasOpen_07,VerizonHeritage_07,VikingClassic_07,WachoviaChampionship_07,WGCAccentureMatchPlayChampionship_07,WGCBridgestoneInvitational_07,WGCCAChampionship_07,WyndhamChampionship_07,ZurichClassicofNewOrleans_07))

#2008
pga_2008_results <- do.call('rbind',list(ArnoldPalmerInvitational_08,ATTClassic_08,ATTNational_08,ATTPebbleBeach_08,BMWChampionship_08,BobHopeChryslerClassic_08,BritishOpen_08,BuickInvitational_08,BuickOpen_08,ChildrensMiracleNetworkClassic_08,CrownePlazaInvitationalatColonial_08,DeutscheBankChampionship_08,FryscomOpen_08,GinnsurMerClassic_08,HondaClassic_08,HPByronNelsonChampionship_08,JohnDeereClassic_08,JustinTimberlakeShrinersHospitals_08,LegendsRenoTahoeOpen_08,Masters_08,MayakobaGolfClassic_08,MemorialTournament_08,MercedesBenzChampionship_08,NorthernTrustOpen_08,PGAChampionship_08,PODSChampionship_08,PuertoRicoOpen_08,RBCCanadianOpen_08,ShellHoustonOpen_08,SonyOpeninHawaii_08,StanfordStJudeClassic_08,TheBarclays_08,ThePlayersChampionship_08,TourChampionship_08,TravelersChampionship_08,TurningStoneResortChampionship_08,USBankChampionshipinMilwaukee_08,USOpen_08,ValeroTexasOpen_08,VerizonHeritage_08,VikingClassic_08,WachoviaChampionship_08,WasteManagementPhoenixOpen_08,WGCAccentureMatchPlayChampionship_08,WGCBridgestoneInvitational_08,WGCCAChampionship_08,WyndhamChampionship_08,ZurichClassicofNewOrleans_08))

#2009
pga_2009_results <- do.call('rbind',list(ArnoldPalmerInvitational_09,ATTNational_09,ATTPebbleBeach_09,BMWChampionship_09,BobHopeChryslerClassic_09,BritishOpen_09,BuickInvitational_09,BuickOpen_09,ChildrensMiracleNetworkClassic_09,CrownePlazaInvitationalatColonial_09,DeutscheBankChampionship_09,FryscomOpen_09,HondaClassic_09,HPByronNelsonChampionship_09,JohnDeereClassic_09,JustinTimberlakeShrinersHospitals_09,LegendsRenoTahoeOpen_09,Masters_09,MayakobaGolfClassic_09,MemorialTournament_09,MercedesBenzChampionship_09,NorthernTrustOpen_09,PGAChampionship_09,PuertoRicoOpen_09,QuailHollowChampionship_09,RBCCanadianOpen_09,ShellHoustonOpen_09,SonyOpeninHawaii_09,StJudeClassic_09,TheBarclays_09,ThePlayersChampionship_09,TourChampionship_09,TransitionsChampionship_09,TravelersChampionship_09,TurningStoneResortChampionship_09,USBankChampionshipinMilwaukee_09,USOpen_09,ValeroTexasOpen_09,VerizonHeritage_09,WasteManagementPhoenixOpen_09,WGCAccentureMatchPlayChampionship_09,WGCBridgestoneInvitational_09,WGCCAChampionship_09,WyndhamChampionship_09,ZurichClassicofNewOrleans_09))

#2010
pga_2010_results <- do.call('rbind',list(ArnoldPalmerInvitational_10,ATTNational_10,ATTPebbleBeach_10,BarclaysClassic_10,BMWChampionship_10,BobHopeClassic_10,BritishOpen_10,ChildrensMiracleNetworkClassic_10,CrownePlazaInvitationalatColonial_10,DeutscheBankChampionship_10,FarmersInsuranceOpen_10,FryscomOpen_10,HondaClassic_10,HPByronNelsonChampionship_10,JohnDeereClassic_10,JustinTimberlakeShrinersHospitals_10,Masters_10,MayakobaGolfClassic_10,McGladreyClassic_10,MemorialTournament_10,NorthernTrustOpen_10,PGAChampionship_10,PuertoRicoOpen_10,QuailHollowChampionship_10,RBCCanadianOpen_10,RenoTahoeOpen_10,SBSChampionship_10,ShellHoustonOpen_10,SonyOpeninHawaii_10,StJudeClassic_10,TheGreenbrierClassic_10,ThePlayersChampionship_10,TourChampionship_10,TransitionsChampionship_10,TravelersChampionship_10,TurningStoneResortChampionship_10,USOpen_10,ValeroTexasOpen_10,VerizonHeritage_10,VikingClassic_10,WasteManagementPhoenixOpen_10,WGCAccentureMatchPlayChampionship_10,WGCBridgestoneInvitational_10,WGCCAChampionship_10,WyndhamChampionship_10,ZurichClassicofNewOrleans_10))

#2011
pga_2011_results <- do.call('rbind',list(ArnoldPalmerInvitational_11,ATTNational_11,ATTPebbleBeach_11,BMWChampionship_11,BobHopeClassic_11,BritishOpen_11,ChildrensMiracleNetworkClassic_11,CrownePlazaInvitationalatColonial_11,DeutscheBankChampionship_11,FarmersInsuranceOpen_11,FedExStJudeClassic_11,FryscomOpen_11,HondaClassic_11,HPByronNelsonChampionship_11,HyundaiTournamentofChampions_11,JohnDeereClassic_11,JustinTimberlakeShrinersHospitals_11,Masters_11,MayakobaGolfClassic_11,McGladreyClassic_11,MemorialTournament_11,NorthernTrustOpen_11,PGAChampionship_11,PuertoRicoOpen_11,RBCCanadianOpen_11,RenoTahoeOpen_11,ShellHoustonOpen_11,SonyOpeninHawaii_11,TheBarclays_11,TheGreenbrierClassic_11,TheHeritage_11,ThePlayers_11,TourChampionship_11,TransitionsChampionship_11,TravelersChampionship_11,USOpen_11,ValeroTexasOpen_11,VikingClassic_11,WasteManagementPhoenixOpen_11,WellsFargoChampionship_11,WGCAccentureMatchPlayChampionship_11,WGCBridgestoneInvitational_11,WGCCadillacChampionship_11,WyndhamChampionship_11,ZurichClassicofNewOrleans_11))

#2012
pga_2012_results <- do.call('rbind',list(ArnoldPalmerInvitational_12,ATTNational_12,ATTPebbleBeachNationalProAm_12,BMWChampionship_12,BritishOpen_12,ChildrensMiracleNetworkClassic_12,CrownePlazaInvitationalatColonial_12,DeutscheBankChampionship_12,FarmersInsuranceOpen_12,FedExStJudeClassic_12,FryscomOpen_12,HondaClassic_12,HPByronNelsonChampionship_12,HumanaChallenge_12,HyundaiTournamentofChampions_12,JohnDeereClassic_12,JustinTimberlakeShrinersHospitals_12,Masters_12,MayakobaGolfClassic_12,McGladreyClassic_12,MemorialTournament_12,NorthernTrustOpen_12,PGAChampionship_12,PuertoRicoOpen_12,RBCCanadianOpen_12,RBCHeritage_12,RenoTahoeOpen_12,ShellHoustonOpen_12,SonyOpeninHawaii_12,TheBarclays_12,TheGreenbrierClassic_12,ThePlayersChampionship_12,TourChampionship_12,TransitionsChampionship_12,TravelersChampionship_12,TrueSouthClassic_12,USOpen_12,ValeroTexasOpen_12,WasteManagementPhoenixOpen_12,WellsFargoChampionship_12,WGCAccentureMatchPlayChampionship_12,WGCBridgestoneInvitational_12,WGCCadillacChampionship_12,WyndhamChampionship_12,ZurichClassicofNewOrleans_12))

#2013
pga_2013_results <- do.call('rbind',list(ArnoldPalmerInvitational_13,ATTNational_13,ATTPebbleBeachNationalProAm_13,BarclaysClassic_13,BMWChampionship_13,BritishOpen_13,CrownePlazaInvitationalatColonial_13,DeutscheBankChampionship_13,FarmersInsuranceOpen_13,FedExStJudeClassic_13,HondaClassic_13,HPByronNelsonChampionship_13,HumanaChallenge_13,HyundaiTournamentofChampions_13,JohnDeereClassic_13,Masters_13,MemorialTournament_13,NorthernTrustOpen_13,PGAChampionship_13,PuertoRicoOpen_13,RBCCanadianOpen_13,RBCHeritage_13,RenoTahoeOpen_13,SandersonFarmsChampionship_13,ShellHoustonOpen_13,SonyOpeninHawaii_13,TampaBayChampionship_13,TheGreenbrierClassic_13,ThePlayersChampionship_13,TourChampionship_13,TravelersChampionship_13,USOpen_13,ValeroTexasOpen_13,WasteManagementPhoenixOpen_13,WellsFargoChampionship_13,WGCAccentureMatchPlayChampionship_13,WGCBridgestoneInvitational_13,WGCCadillacChampionship_13,WyndhamChampionship_13,ZurichClassicofNewOrleans_13))

#2014
pga_2014_results <- do.call('rbind',list(ArnoldPalmerInvitational_14,ATTPebbleBeach_14,BarracudaChampionship_14,BMWChampionship_14,BritishOpen_14,CIMBClassic_14,CrownePlazaInvitationalatColonial_14,DeutscheBankChampionship_14,FarmersInsuranceOpen_14,FedExStJudeClassic_14,FryscomOpen_14,HondaClassic_14,HPByronNelsonChampionship_14,HumanaChallenge_14,HyundaiTournamentofChampions_14,JohnDeereClassic_14,Masters_14,MayakobaGolfClassic_14,McGladreyClassic_14,MemorialTournament_14,NorthernTrustOpen_14,PGAChampionship_14,PuertoRicoOpen_14,QuickenLoansNational_14,RBCCanadianOpen_14,RBCHeritage_14,ShellHoustonOpen_14,ShrinersHospitalsforChildrenOpen_14,SonyOpeninHawaii_14,TheBarclays_14,TheGreenbrierClassic_14,ThePlayersChampionship_14,TourChampionship_14,TravelersChampionship_14,USOpen_14,ValeroTexasOpen_14,ValsparChampionship_14,WasteManagementPhoenixOpen_14,WellsFargoChampionship_14,WGCAccentureMatchPlayChampionship_14,WGCBridgestoneInvitational_14,WGCCadillacChampionship_14,WGCHSBCChampions_14,WyndhamChampionship_14,ZurichClassicofNewOrleans_14))

#2015
pga_2015_results <- do.call('rbind',list(ArnoldPalmerInvitational_15,ATTByronNelsonChampionship_15,ATTPebbleBeachNationalProAm_15,BarbasolChampionship_15,BarracudaChampionship_15,BMWChampionship_15,CIMBClassic_15,CrownePlazaInvitationalatColonial_15,DeutscheBankChampionship_15,FarmersInsuranceOpen_15,FedExStJudeClassic_15,FryscomOpen_15,HondaClassic_15,HumanaChallenge_15,HyundaiTournamentofChampions_15,JohnDeereClassic_15,Masters_15,MayakobaGolfClassic_15,McGladreyClassic_15,MemorialTournament_15,NorthernTrustOpen_15,PGAChampionship_15,PuertoRicoOpen_15,QuickenLoansNational_15,RBCCanadianOpen_15,RBCHeritage_15,SandersonFarmsChampionship_15,ShellHoustonOpen_15,ShrinersHospitalsforChildrenOpen_15,SonyOpeninHawaii_15,TheBarclays_15,TheGreenbrierClassic_15,TheOpenChampionship_15,ThePlayersChampionship_15,TourChampionship_15,TravelersChampionship_15,USOpen_15,ValeroTexasOpen_15,ValsparChampionship_15,WasteManagementPhoenixOpen_15,WellsFargoChampionship_15,WGCBridgestoneInvitational_15,WGCCadillacChampionship_15,WGCCadillacMatchPlayChampionship_15,WGCHSBCChampions_15,WyndhamChampionship_15,ZurichClassicofNewOrleans_15))

#2016
pga_2016_results <- do.call('rbind',list(ArnoldPalmerInvitational_16,ATTByronNelson_16,ATTPebbleBeachNationalProAm_16,BarbasolChampionship_16,BarclaysClassic_16,BarracudaChampionship_16,BMWChampionship_16,CareerBuilderChallenge_16,CIMBClassic_16,DeanDeLucaInvitational_16,DeutscheBankChampionship_16,FarmersInsuranceOpen_16,FedExStJudeClassic_16,FryscomOpen_16,HondaClassic_16,HyundaiTournamentofChampions_16,JohnDeereClassic_16,Masters_16,MemorialTournament_16,NorthernTrustOpen_16,OHLClassicatMayakoba_16,PGAChampionship_16,PuertoRicoOpen_16,QuickLoansNational_16,RBCCanadianOpen_16,RBCHeritage_16,SandersonFarmsChampionship_16,ShellHoustonOpen_16,ShrinersHospitalsforChildrenOpen_16,SonyOpeninHawaii_16,TheOpenChampionship_16,ThePlayersChampionship_16,TheRSMClassic_16,TourChampionship_16,TravelersChampionship_16,USOpen_16,ValeroTexasOpen_16,ValsparChampionship_16,WasteManagementPhoenixOpen_16,WellsFargoChampionship_16,WGCBridgestoneInvitational_16,WGCCadillacChampionship_16,WGCDellMatchPlayChampionship_16,WGCHSBCChampions_16,WyndhamChampionship_16,ZurichClassicofNewOrleans_16))

#2017
pga_2017_results <- do.call('rbind',list(ArnoldPalmerInvitational_17,ATTByronNelson_17,ATTPebbleBeach_17,BarbasolChampionship_17,BarracudaChampionship_17,BMWChampionship_17,BritishOpen_17,CareerBuilderChallenge_17,CIMBClassic_17,DeanDeLucaInvitational_17,DellTechnologiesChampionship_17,FarmersInsuranceOpen_17,FedExStJudeClassic_17,GenesisOpen_17,JohnDeereClassic_17,Masters_17,MemorialTournament_17,OHLClassicatMayakoba_17,PGAChampionship_17,PuertoRicoOpen_17,QuickenLoansNational_17,RBCCanadianOpen_17,RBCHeritage_17,SafewayOpen_17,SandersonFarmsChampionship_17,SBSTournamentofChampions_17,ShellHoustonOpen_17,ShrinersHospitalsforChildrenOpen_17,SonyOpeninHawaii_17,TheGreenbrierClassic_17,TheHondaClassic_17,TheNorthernTrust_17,ThePlayersChampionship_17,TheRSMClassic_17,TourChampionship_17,TravelersChampionship_17,USOpen_17,ValeroTexasOpen_17,ValsparChampionship_17,WasteManagementPhoenixOpen_17,WellsFargoChampionship_17,WGCBridgestoneInvitational_17,WGCDellMatchPlayChampionship_17,WGCHSBCChampions_17,WGCMexicoChampionship_17,WyndhamChampionship_17,ZurichClassicofNewOrleans_17))

#2018
pga_2018_results <- do.call('rbind',list(AMilitaryTributeatTheGreenbrier_18,ArnoldPalmerInvitational_18,ATTByronNelson_18,ATTPebbleBeachProAm_18,BarbasolChampionship_18,BarracudaChampionship_18,BMWChampionship_18,CareerBuilderChallenge_18,CIMBClassic_18,CoralesPuntacanResortChampionship_18,DellTechnologiesChampionship_18,FarmersInsuranceOpen_18,FedExStJudeClassic_18,FortWorthInvitational_18,GenesisOpen_18,HondaClassic_18,HoustonOpen_18,JohnDeereClassic_18,Masters_18,MemorialTournament_18,OHLClassicatMayakoba_18,PGAChampionship_18,QuickenLoansNational_18,RBCCanadianOpen_18,RBCHeritage_18,SafewayOpen_18,SandersonFarmsChampionship_18,SentryTournamentofChampions_18,ShrinersHospitalsforChildrenOpen_18,SonyOpeninHawaii_18,TheCJCupatNineBridges_18,TheNorthernTrust_18,TheOpenChampionship_18,ThePlayersChampionship_18,TheRSMClassic_18,TourChampionship_18,TravelersChampionship_18,USOpen_18,ValeroTexasOpen_18,ValsparChampionship_18,WasteManagementPhoenixOpen_18,WellsFargoChampionship_18,WGCBridgestoneInvitational_18,WGCDellMatchPlayChampionship_18,WGCHSBCChampions_18,WGCMexicoChampionship_18,WyndhamChampionship_18,ZurichClassicofNewOrleans_18))

#2019
pga_2019_results <- do.call('rbind',list(ArnoldPalmerInvitational_19,ATTByronNelson_19,ATTPebbleBeachProAm_19,BarbasolChampionship_19,BarracudaChampionship_19,BMWChampionship_19,BritishOpen_19,CharlesSchwabChallenge_19,CIMBClassic_19,CoralesPuntacanResortChampionship_19,DesertClassic_19,FarmersInsuranceOpen_19,GenesisOpen_19,HondaClassic_19,JohnDeereClassic_19,Masters_19,MayakobaGolfClassic_19,MemorialTournament_19,MOpen_19,PGAChampionship_19,PuertoRicoOpen_19,RBCCanadianOpen_19,RBCHeritage_19,RocketMortgageClassic_19,SafewayOpen_19,SandersonFarmsChampionship_19,SentryTournamentofChampions_19,ShrinersHospitalsforChildrenOpen_19,SonyOpeninHawaii_19,TheCJCupatNineBridges_19,TheNorthernTrust_19,ThePlayersChampionship_19,TheRSMClassic_19,TourChampionship_19,TravelersChampionship_19,USOpen_19,ValeroTexasOpen_19,ValsparChampionship_19,WasteManagementPhoenixOpen_19,WellsFargoChampionship_19,WGCDellMatchPlayChampionship_19,WGCFedExStJudeInvitational_19,WGCHSBCChampions_19,WGCMexicoChampionship_19,WyndhamChampionship_19,ZurichClassicofNewOrleans_19))

All_Results <- do.call("rbind", list(pga_1996_results,pga_1997_results,pga_1998_results,pga_1999_results,pga_2000_results,pga_2001_results,pga_2002_results,pga_2003_results,pga_2004_results,pga_2005_results,pga_2006_results,pga_2007_results,pga_2008_results,pga_2009_results,pga_2010_results,pga_2011_results,pga_2012_results,pga_2013_results,pga_2014_results,pga_2015_results,pga_2016_results,pga_2017_results,pga_2018_results,pga_2019_results))

#Create a data frame based on the number of wins, Top 10s and Majors won

#Drop team results
All_Results <- All_Results[!grepl("with", All_Results$Player),]

#Create a table for the number of wins by player
All_Results_Wins <- All_Results[which(All_Results$Finish == 1),]
All_Results_Wins <- aggregate(All_Results_Wins$Finish, list(All_Results_Wins$Player), sum)
colnames(All_Results_Wins) <- c("Player", "Wins")
All_Results_Wins <- All_Results_Wins[order(-All_Results_Wins$Wins),]

#Create a table for the number of Top 10s by player
All_Results_Top10 <- All_Results[which(All_Results$Finish <= 10),]
All_Results_Top10 <- as.data.frame(table(All_Results_Top10$Player))
colnames(All_Results_Top10) <- c("Player", "Top10")
All_Results_Top10 <- All_Results_Top10[order(-All_Results_Top10$Top10),]

#Create a table for the number of majors by player
All_Results_Majors <- All_Results[which(All_Results$Finish == 1),]
All_Results_Majors <- All_Results_Majors[grepl("Masters", All_Results_Majors[["Tournament"]]) |grepl("British", All_Results_Majors[["Tournament"]]) | grepl("TheOpen", All_Results_Majors[["Tournament"]]) | grepl("PGAChamp", All_Results_Majors[["Tournament"]]) | grepl("USOpen", All_Results_Majors[["Tournament"]]),]

All_Results_Majors <- as.data.frame(table(All_Results_Majors$Player))
colnames(All_Results_Majors) <- c("Player", "Majors")
All_Results_Majors <- All_Results_Majors[order(-All_Results_Majors$Majors),]

#Combine all results
pga <- merge(All_Results_Wins, All_Results_Top10)
pga <- merge(pga, All_Results_Majors)
pga <- pga[order(-pga$Wins),]


#Viz 1
ggplot(data=pga, aes(x= reorder(Player, -Wins), y=Wins)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes(label = Wins), size = 2.5, vjust = -0.5)


#Create dataframe to determine the reocrds of golfers against Tiger Woods
#Wins
Tiger_wins <- aggregate(All_Results$TWwin, list(All_Results$Player), sum)
colnames(Tiger_wins) <- c("Player","Wins")
#Losses
Tiger_losses <- aggregate(All_Results$TWLoss, list(All_Results$Player), sum)
colnames(Tiger_losses) <- c("Player","Losses")
#Ties
Tiger_ties <- aggregate(All_Results$TWTie, list(All_Results$Player), sum)
colnames(Tiger_ties) <- c("Player","Ties")
Tiger_head <- merge(Tiger_wins, Tiger_losses)
Tiger_head <- merge(Tiger_head, Tiger_ties)
#Remove top row as it is blank
Tiger_head <- Tiger_head[-1,]
#Add column for win percentage
Tiger_head$WinPercentage <- round((Tiger_head$Wins+(Tiger_head$Ties/2))/
  (Tiger_head$Wins + Tiger_head$Losses + Tiger_head$Ties),3)
Tiger_head$Total <- Tiger_head$Wins + Tiger_head$Losses + Tiger_head$Ties
Tiger_head <- Tiger_head[which(Tiger_head$Total >= 30),]
Tiger_head <- Tiger_head[order(-Tiger_head$WinPercentage),]

#Viz 2
ggplot(Tiger_head, aes(x=WinPercentage)) +
  geom_area(stat = "bin") +
  geom_area(aes(y = ..density..), stat = "bin") +
  geom_vline(xintercept = 0.5)

#Function to identify player(s) with the most wins in a season
tour_summary <- function(df,yyyy,major1,major2,major3,major4){
  wins <- df[which(df$Finish == 1),]
  wins <- as.data.frame(table(wins$Player))
  colnames(wins) <- c("Player","Wins")
  wins <- wins[order(-wins$Wins),]  
  top10 <- df[which(df$Finish <=10),]
  top10 <- as.data.frame(table(top10$Player))
  colnames(top10) <- c("Player","Top10")
  top10 <- top10[order(-top10$Top10),]  
  h2h <-  aggregate(df$Wins, by = list(df$Player),sum) 
  colnames(h2h) <- c("Player","GolfersDefeated")
  h2h <- h2h[order(-h2h$GolfersDefeated),]
  eventsPlayed <- as.data.frame(table(df$Player))
  colnames(eventsPlayed) <- c("Player","EventsPlayed")
  eventsPlayed <- eventsPlayed[order(-eventsPlayed$EventsPlayed),]
  majors <- do.call("rbind",list(major1, major2, major3, major4))
  majors <- majors[which(majors$Finish == 1),]
  majors <- as.data.frame(table(majors$Player))
  colnames(majors) <- c("Player","Majors")
  summary <- merge(wins,h2h) %>%
    merge(eventsPlayed) %>%
    merge(top10)
  summary <- summary[!grepl("with", summary$Player),]
  summary <- summary[order(-summary$Wins),]
  summary$GolfersDefeatedPerEvent <- summary$GolfersDefeated/summary$EventsPlayed
  summary$WinPercentage <- round(summary$Wins/summary$EventsPlayed,3)
  summary$Rank <- rank(-summary$Wins, ties.method = "min")
  final <- summary[which(summary$Rank <=1),] 
  final$Year <- yyyy
  final <- merge(final,majors, all.x = TRUE)
  final$Majors[is.na(final$Majors)] <- 0
  final <- final[,c(9,1:2,5,10,7,3,4,6,8)]
  final <- final[order(-final$Wins),]
}

#Populating data frames to identify player(s) with the most wins
pga_1996_final <- tour_summary(pga_1996_results,1996,Masters_96,USOpen_96,
                               BritishOpen_96,PGAChampionship_96)
pga_1997_final <- tour_summary(pga_1997_results,1997,Masters_97,USOpen_97,
                               BritishOpen_97,PGAChampionship_97)
pga_1998_final <- tour_summary(pga_1998_results,1998,Masters_98,USOpen_98,
                               BritishOpen_98,PGAChampionship_98)
pga_1999_final <- tour_summary(pga_1999_results,1999,Masters_99,USOpen_99,
                               BritishOpen_99,PGAChampionship_99)
pga_2000_final <- tour_summary(pga_2000_results,2000,Masters_00,USOpen_00,
                               BritishOpen_00,PGAChampionship_00)
pga_2001_final <- tour_summary(pga_2001_results,2001,Masters_01,USOpen_01,
                               BritishOpen_01,PGAChampionship_01)
pga_2002_final <- tour_summary(pga_2002_results,2002,Masters_02,USOpen_02,
                               BritishOpen_02,PGAChampionship_02)
pga_2003_final <- tour_summary(pga_2003_results,2003,Masters_03,USOpen_03,
                               BritishOpen_03,PGAChampionship_03)
pga_2004_final <- tour_summary(pga_2004_results,2004,Masters_04,USOpen_04,
                               BritishOpen_04,PGAChampionship_04)
pga_2005_final <- tour_summary(pga_2005_results,2005,Masters_05,USOpen_05,
                               BritishOpen_05,PGAChampionship_05)
pga_2006_final <- tour_summary(pga_2006_results,2006,Masters_06,USOpen_06,
                               BritishOpen_06,PGAChampionship_06)
pga_2007_final <- tour_summary(pga_2007_results,2007,Masters_07,USOpen_07,
                               BritishOpen_07,PGAChampionship_07)
pga_2008_final <- tour_summary(pga_2008_results,2008,Masters_08,USOpen_08,
                               BritishOpen_08,PGAChampionship_08)
pga_2009_final <- tour_summary(pga_2009_results,2009,Masters_09,USOpen_09,
                               BritishOpen_09,PGAChampionship_09)
pga_2010_final <- tour_summary(pga_2010_results,2010,Masters_10,USOpen_10,
                               BritishOpen_10,PGAChampionship_10)
pga_2011_final <- tour_summary(pga_2011_results,2011,Masters_11,USOpen_11,
                               BritishOpen_11,PGAChampionship_11)
pga_2012_final <- tour_summary(pga_2012_results,2012,Masters_12,USOpen_12,
                               BritishOpen_12,PGAChampionship_12)
pga_2013_final <- tour_summary(pga_2013_results,2013,Masters_13,USOpen_13,
                               BritishOpen_13,PGAChampionship_13)
pga_2014_final <- tour_summary(pga_2014_results,2014,Masters_14,USOpen_14,
                               BritishOpen_14,PGAChampionship_14)
pga_2015_final <- tour_summary(pga_2015_results,2015,Masters_15,USOpen_15,
                               TheOpenChampionship_15,PGAChampionship_15)
pga_2016_final <- tour_summary(pga_2016_results,2016,Masters_16,USOpen_16,
                               TheOpenChampionship_16,PGAChampionship_16)
pga_2017_final <- tour_summary(pga_2017_results,2017,Masters_17,USOpen_17,
                               BritishOpen_17,PGAChampionship_17)
pga_2018_final <- tour_summary(pga_2018_results,2018,Masters_18,USOpen_18,
                               TheOpenChampionship_18,PGAChampionship_18)
pga_2019_final <- tour_summary(pga_2019_results,2019,Masters_19,USOpen_19,
                               BritishOpen_19,PGAChampionship_19)

#Combine previous output so all players who led the Tour in wins are in the same dataset
pga_leaders <- do.call("rbind",list(pga_1996_final,pga_1997_final,pga_1998_final,pga_1999_final,pga_2000_final,pga_2001_final,pga_2002_final,pga_2003_final,pga_2004_final,pga_2005_final,pga_2006_final,pga_2007_final,pga_2008_final,pga_2009_final,pga_2010_final,pga_2011_final,pga_2012_final,pga_2013_final,pga_2014_final,pga_2015_final,pga_2016_final,pga_2017_final,pga_2018_final,pga_2019_final))

#Add a column called Tiger so that color can be added to points related to Tiger
pga_leaders$Tiger <- ifelse(pga_leaders$Player == "Tiger Woods", "Tiger", "Field")

#Breaks ties in a season for players with the most wins. First tie breaker is the number of majors and then number of Top 10s and then Win Percentage as needed. Due to the smaller nature of this subset ties were explored manually and then code was used to remove

pga_leaders <- pga_leaders[-c(which(pga_leaders$Player == "Bubba Watson" & pga_leaders$Year == 2011), which(pga_leaders$Player == "Luke Donald" & pga_leaders$Year == 2011), which(pga_leaders$Player == "Steve Stricker" & pga_leaders$Year == 2011), which(pga_leaders$Player == "Mark Wilson" & pga_leaders$Year == 2011), which(pga_leaders$Player == "Nick Watney" & pga_leaders$Year == 2011), which(pga_leaders$Player == "Webb Simpson" & pga_leaders$Year == 2011), which(pga_leaders$Player == "Jimmy Walker" & pga_leaders$Year == 2014), which(pga_leaders$Player == "Bubba Watson" & pga_leaders$Year == 2014), which(pga_leaders$Player == "Jason Day" & pga_leaders$Year == 2015), which(pga_leaders$Player == "Jason Day" & pga_leaders$Year == 2016), which(pga_leaders$Player == "Bryson DeChambeau" & pga_leaders$Year == 2018), which(pga_leaders$Player == "Bubba Watson" & pga_leaders$Year == 2018), which(pga_leaders$Player == "Justin Thomas" & pga_leaders$Year == 2018), which(pga_leaders$Player == "Rory McIlroy" & pga_leaders$Year == 2019)),]

#Viz 3 
#Show Tiger's Wins, Win Percentage and Majors win relative to the field
ggplot(pga_leaders, aes(x=WinPercentage, y=Wins, col = Tiger, size = Majors)) +
  geom_point()+
  scale_color_manual(values = c("Tiger" = "red", "Field" = "grey")) +
  geom_text(aes(label = Year), hjust = -0.3, vjust = -0.1, size = 3) +
  theme_bw() +
  theme( panel.grid.minor = element_blank(),
         panel.background = element_blank())

#2000 season stats

#Function to load data from pga tour
pga_stats_2000 <- function(site){
  x <- read_html(site)
  y <- x %>% html_table(fill = TRUE)
  y <- as.data.frame(y)
  y <- y[,-c(1:5)]
}

#Driving Distance
drivedistance <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.101.y2000.html")
drivedistance <- drivedistance[,c(1,3)]
colnames(drivedistance) <- c("Player","DriveDistAvg")
#Driving Accuracy
driveaccuracy <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.102.y2000.html")
driveaccuracy <- driveaccuracy[,c(1,3)]
colnames(driveaccuracy) <- c("Player","DriveAcc")
x <- merge(drivedistance,driveaccuracy)
#GIR
gir <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.103.y2000.html")
gir <- gir[,c(1,3)]
colnames(gir) <- c("Player","GIR")
x <- merge(x, gir)
#Sand Save
sand <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.111.y2000.html")
sand <- sand[,c(1,3)]
colnames(sand) <- c("Player","SandSave")
x <- merge(x, sand)
#Scrambling
scramble <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.130.y2000.html") 
scramble <- scramble[,c(1,3)]
colnames(scramble) <- c("Player","Scramble")
x <- merge(x, scramble)
#One putt Percentage
oneputt <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.413.y2000.html")
oneputt <- oneputt[,c(1,3)]
colnames(oneputt) <- c("Player","OnePutt")
x <- merge(x, oneputt)
#Three-putt Avoidance
threeputt <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.426.y2000.html") 
threeputt <- threeputt[,c(1,3)]
colnames(threeputt) <- c("Player","ThreePutt")
x <- merge(x, threeputt)
#Putts Per Round
putts <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.119.y2000.html")
putts <- putts[,c(1,3)]
colnames(putts) <- c("Player","Putts")
x <- merge(x, putts)
#Birdie Conversion
str(All_Results)
birdieconversion <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.115.y2000.html") 
birdieconversion <- birdieconversion[,c(1,3)]
colnames(birdieconversion) <- c("Player","BirdConv")
x <- merge(x, birdieconversion)
#Scoring Average
scoringavg <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.120.y2000.html")
scoringavg <- scoringavg[,c(1,3)]
colnames(scoringavg) <- c("Player","ScoreAvg")
x <- merge(x, scoringavg)
#Par 3
par3 <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.142.y2000.html")
par3 <- par3[,c(1,3)]
colnames(par3) <- c("Player","Par3")
x <- merge(x, par3)
#Par 4
par4 <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.143.y2000.html")
par4 <- par4[,c(1,3)]
colnames(par4) <- c("Player","Par4")
x <- merge(x, par4)
#Par 5
par5 <- pga_stats_2000("https://www.pgatour.com/stats/stat.144.y2000.html")
par5 <- par5[,c(1,3)]
colnames(par5) <- c("Player","Par5")
x <- merge(x, par5)
#Money
money <- pga_stats_2000("https://www.pgatour.com/content/pgatour/stats/stat.109.y2000.html")
money <- money[,c(1,3)]
colnames(money) <- c("Player","Money")
x <- merge(x, money)
#Convert dollars to numeric
x$Money <- as.numeric(gsub('[$,]', '', x$Money))
#Remove first column, which contains player names. It is not needed for multiple regression 
pga_mr <- x[,-1]

#Build models 
#EVentually Average Score and Par3, Par4, and Par5 scores were not considered as it is obvious the player who has the lowest score does well. I wanted to identify what aspect of the player's game was needed to perform well on the PGA Tour money list. In 2000 this was a quality metric to determine if a player was successful. Unfortunately, the amount of data was limited in 2000.
model<- lm(Money ~ DriveDistAvg + DriveAcc + GIR + SandSave + Scramble +
             OnePutt + ThreePutt + Putts + BirdConv + Par3 + Par4 + Par5, 
           data = pga_mr)
model2 <- lm(Money ~ DriveDistAvg + DriveAcc + GIR + Scramble +
               OnePutt + ThreePutt + Putts + BirdConv + Par3 + Par4 + Par5, 
             data = pga_mr)
summary(model2)
model3 <- lm(Money ~ DriveDistAvg + GIR + Scramble + OnePutt + ThreePutt 
             + Putts + BirdConv + Par3 + Par4 + Par5, data = pga_mr)
summary(model3)

model4 <- lm(Money ~ DriveDistAvg + GIR + Scramble + OnePutt + ThreePutt 
             + BirdConv + Par3 + Par4 + Par5, data = pga_mr)
summary(model4)
model5 <- lm(Money ~ DriveDistAvg + GIR + Scramble + OnePutt + ThreePutt 
             + Par3 + Par4 + Par5, data = pga_mr)
summary(model5)
model6 <- lm(Money ~  GIR + Scramble + OnePutt + ThreePutt 
             + Par3 + Par4 + Par5, data = pga_mr)
summary(model6)
plot(model6)
model7 <- lm(Money ~ DriveDistAvg + DriveAcc + GIR + SandSave + Scramble +
             OnePutt + ThreePutt + Putts + BirdConv, data = pga_mr)
summary(model7)
model8 <- lm(Money ~ DriveDistAvg + DriveAcc + GIR + Scramble +
               OnePutt + ThreePutt + Putts + BirdConv, data = pga_mr)
summary(model8)
model9 <- lm(Money ~ DriveDistAvg + DriveAcc + GIR + Scramble +
               OnePutt + Putts + BirdConv, data = pga_mr)
summary(model9)
model10 <- lm(Money ~ DriveDistAvg + GIR + Scramble +
               OnePutt + Putts + BirdConv, data = pga_mr)
summary(model10)

#Create visual to show how Tiger compared in the key stats against the field
#Field Stats
DriveAvg <- round(mean(drivedistance$DriveDistAvg),1)
GIRAvg <- round(mean(gir$GIR),1)
ScrambleAvg <- round(mean(scramble$Scramble),1)
OnePuttAvg <- round(mean(oneputt$OnePutt),1)
PuttsAvg <- round(mean(putts$Putts),1)
BirdieConvAvg <- round(mean(birdieconversion$BirdConv),1)
#Tiger's stats
TigerDrive <- round(drivedistance[which(drivedistance$Player == "Tiger Woods"),2],1)
TigerGIR <- round(gir[which(gir$Player == "Tiger Woods"),2],1)
TigerScamble <- round(scramble[which(scramble$Player == "Tiger Woods"),2],1)
TigerOnePuttAvg <- round(oneputt[which(oneputt$Player == "Tiger Woods"),2],1)
TigerPuttsAvg <- round(putts[which(putts$Player == "Tiger Woods"),2],1)
TigerBirdieAvg <- round(birdieconversion[which(birdieconversion$Player == "Tiger Woods"),2],1)

#Plot each stat to compare
#Viz 5
#Driving Distance
Driving <- melt(data.frame(DriveAvg,TigerDrive))
ggplot(data=Driving, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
#GIR
GIRviz <- melt(data.frame(GIRAvg,TigerGIR))
ggplot(data=GIRviz, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
#Scrambling
ScrambleViz <- melt(data.frame(ScrambleAvg, TigerScramble))
ggplot(data=ScrambleViz, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
#One Putt Percentage
OnePuttViz <- melt(data.frame(OnePuttAvg, TigerOnePuttAvg))
ggplot(data=OnePuttViz, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
#Putts
PuttViz <- melt(data.frame(PuttsAvg, TigerPuttsAvg))
ggplot(data=PuttViz, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
#Birdie Conversion
BirdieViz <- melt(data.frame(BirdieConvAvg, TigerBirdieAvg))
ggplot(data=BirdieViz, aes(x=variable, y=value)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

#Different function to examine Tiger's performance in the majors during 2000
golfstat2 <- function(link) {
  x <- read_html(link)
  y <- x %>% html_table(fill = TRUE)
  y <- as.data.frame(y)
  colnames(y) <- y[2,]
  y <- y[-c(1:2),-c(2,8:10,13:16)]
  colnames(y)[2] <- "Place"
  y$Player <- gsub("Playoff:.*","",y$Player)
  made_cut <- y[which(y$Place !="CUT" & y$Place != "WD"),]
  cut <- y[which(y$Place =="CUT"),]
  made_cut$Finish <- rank(made_cut$`Total Score`, ties.method = "min")
  max_rank <- max(made_cut$Finish)
  cut$Finish <- rank(cut$`Total Score`, ties.method = "min")+max_rank
  y_final <- rbind(made_cut,cut)
  colnames(y_final)[8] <- "TotalScore"
  cols <- names(y_final)[3:9]
  y_final[cols] <- lapply(y_final[cols], as.numeric)
  y_final <- as.data.frame(y_final)  
}

#Using the function to create a dataset for the Masters, US Open, British Open, and PGA

#Masters
masters_cut <- Masters_00[which(Masters_00$Place != "CUT"),]
us_open_cut <- USOpen_00[which(USOpen_00$Place != "CUT"),]
british_cut <- BritishOpen_00[which(BritishOpen_00$Place != "CUT"),]
pga_cut <- PGAChampionship_00[which(PGAChampionship_00$Place != "CUT"),]

#Add a column to indicate if the Player is Tiger Woods. This will be used to color his dot in boxplot
masters_cut$Tiger <- ifelse((masters_cut$Player == "Tiger Woods") == "TRUE",1,0)
us_open_cut$Tiger <- ifelse((us_open_cut$Player == "Tiger Woods") == "TRUE",1,0)
british_cut$Tiger <- ifelse((british_cut$Player == "Tiger Woods") == "TRUE",1,0)
pga_cut$Tiger <- ifelse((pga_cut$Player == "Tiger Woods") == "TRUE",1,0)

#Combine outputs for majors
majors_2000 <- do.call("rbind", list(masters_cut,us_open_cut,british_cut,pga_cut))

#Plot Major Results for those who made the cut and identify where Tiger is located
#Visual reveals that Tiger is below the first quartile, which is good because you want the lowest score in golf. He appears as an outlier for the US Open and is close to an outlier for the British and PGA. This begins to tell the story of his dominance
#Masters
#Viz 4
masters_box <- ggplot(masters_cut, aes(x="", y=TotalScore, col=Tiger)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("2000 Masters Results") +
  xlab("") + ylab("Score") +
  theme_light() +
  theme(legend.position = "none", plot.title = element_text(color = "#006747"))
#US Open
us_open_box <- ggplot(us_open_cut, aes(x="", y=TotalScore, col=Tiger)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("2000 US Open Results")+
  xlab("") + ylab("Score") +
  theme_light() +
  theme(plot.title = element_text(color = "#E31936"))
#British
british_box <- ggplot(british_cut, aes(x="", y=TotalScore, col=Tiger)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("2000 British Open Results")+
  xlab("") + ylab("Score") +
  theme_light() +
  theme(legend.position = "none", plot.title = element_text(color = "#FFB500"))
#PGA
pga_box <- ggplot(pga_cut, aes(x="", y=TotalScore, col=Tiger)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("2000 PGA Championship Results")+
  xlab("") + ylab("Score") +
  theme_light() +  
  theme(legend.position = "none", plot.title = element_text(color = "#222943"))
#Plot all boxplots together
library(cowplot)
plot_grid(masters_box, us_open_box, british_box, pga_box)

#Identify Player's reocrds in majors

#Examine the number of golfers each golfer beat in majors and visualize
wins <- function(df){
  a <- as.data.frame(table(df$Finish))
  colnames(a) <- c("Finish", "Count")
  b <- merge(df, a, by.x = "Finish", by.y = "Finish")
  b$Count <- b$Count - 1
  b$Wins <- nrow(b) - b$Finish - b$Count
  b$Ties <- b$Count
  b$Losses <- (nrow(b)-1) - b$Wins - b$Ties
  b <- b[,c(2:12,1,17)]
  b <- as.data.frame(b)
}

#Masters
masters_wins <- wins(Masters_00)
#US Open
us_open_wins <- wins(USOpen_00)
#British
british_wins <- wins(BritishOpen_00)
#PGA
pga_wins <- wins(PGAChampionship_00)

#Combine all majors 
major_wins <- merge(merge(merge(masters_wins, us_open_wins, by = "Player",all = TRUE),british_wins, by = "Player", all = TRUE),pga_wins, by = "Player", all = TRUE)
View(major_wins)

major_wins <- major_wins[,c(1,9:11,21:23,33:35,45:47)]
colnames(major_wins) <- c("Player","MastersWins","MastersLosses","MastersTies","USOpenWins","USOpenLosses","USOpenTies","BritishWins","BritishLosses","BritishTies","PGAWins","PGALosses","PGATies") View(major_wins)
major_wins$TotalWins <- rowSums(major_wins[,c("MastersWins", "USOpenWins",
                                "BritishWins","PGAWins")], na.rm = TRUE)
major_wins$TotalLosses <- rowSums(major_wins[,c("MastersLosses", "USOpenLosses",
                            "BritishLosses","PGALosses")], na.rm = TRUE)
major_wins$TotalTies <- rowSums(major_wins[,c("MastersTies", "USOpenTies", 
                               "BritishTies","PGATies")], na.rm = TRUE)

major_wins_final <- major_wins[,c(1,14:16)]
major_wins_final <- major_wins_final[order(-major_wins_final$TotalWins),]
View(major_wins_final)