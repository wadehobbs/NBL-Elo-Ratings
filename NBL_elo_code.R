#### Web Scraping NBL data ####
library(XML)
library(stringr)

#Set URL
url <- read_html("https://www.nbl.com.au/results")
url2 <- read_html('https://www.flashscore.com/basketball/australia/nbl/results/')
#Find the selector with SelectorGadget in browser - for this site it is as follows:
selector <- '#g_3_M5R4rYF7 > td:nth-child(5)'
xpath <- '/html/body/div[5]/div[1]/div/div[2]/div[1]/div[7]/div[3]/table[1]'
#Pipe
test <- url %>%
        html_nodes(selector) %>%
        html_text() %>%
        as.numeric()

url2 %>%
        html_nodes(selector) %>%
        html_text() %>%
        as.numeric()

test <- url2 %>%
        html_node(xpath=xpath) %>%
        html_table()

#No luck with the web scraping
#Ended up copy/pasting into excel from
#https://www.flashscore.com/basketball/australia/nbl/results/

#### Calculating Elo Ratings ####
#Import data
data_2017 <- read_excel("~/Dropbox/Personal/Jobs/NBL Elo Project/data_2017.xlsx", trim_ws = TRUE)
data_2016 <- read_excel("~/Dropbox/Personal/Jobs/NBL Elo Project/data_2017.xlsx", sheet = "Sheet1", trim_ws = TRUE)
#Clean: separate the score and date columns 
data_2016 <- separate(data_2016, Score, into = c("Home_Score", "Away_Score"), sep = "\\:", remove = TRUE)
data_2016 <- separate(data_2016, Date, into = c("Date", "Time"), sep = "\\. ", remove = TRUE)
nbl_db <- rbind(data_2016, data_2017)
#Delete incomplete rows
nbl_db <- nbl_db[complete.cases(nbl_db),]
#Trim empty space on the away_score col
nbl_db$Away_Score <- str_trim(nbl_db$Away_Score, "left")
nbl_db$Home <- str_trim(nbl_db$Home, "right")
nbl_db$Away <- str_trim(nbl_db$Away, "right")

#Make sure scores are numeric data
nbl_db$Home_Score <- as.numeric(nbl_db$Home_Score)
nbl_db$Away_Score <- as.numeric(nbl_db$Away_Score)
#Fix inconsistency in names
nbl_db$Home[nbl_db$Home == "Adelaide"] <- "Adelaide 36ers"
nbl_db$Home[nbl_db$Home == "Sydney"] <- "Sydney Kings"
nbl_db$Home[nbl_db$Home == "Perth"] <- "Perth Wildcats"
nbl_db$Home[nbl_db$Home == "New Zealand Breakers"] <- "NZ Breakers"
nbl_db$Home[nbl_db$Home == "Cairns"] <- "Cairns Taipans"
nbl_db$Away[nbl_db$Away == "Adelaide"] <- "Adelaide 36ers"
nbl_db$Away[nbl_db$Away == "Sydney"] <- "Sydney Kings"
nbl_db$Away[nbl_db$Away == "Perth"] <- "Perth Wildcats"
nbl_db$Away[nbl_db$Away == "New Zealand Breakers"] <- "NZ Breakers"
nbl_db$Away[nbl_db$Away == "Cairns"] <- "Cairns Taipans"

#Massive pain to get the date column right
#Needs to be done manually because it had no year when copy pasted. Had to maually add year
#Then arrange by date
date <- nbl_db$Date
date[1:47] <- sub("$", ".17", date[1:47] )
date[48:120] <- sub("$", ".16", date[48:120] )
date[121:171] <- sub("$", ".18", date[121:171] )
date[172:241] <- sub("$", ".17", date[172:241] )
nbl_db$Date <- date
nbl_db$Date <- dmy(nbl_db$Date) #Function converts to date
arrange(nbl_db, Date)

#Elo
#Set up the initial team elo values data frame
teams <- data.frame(team = unique(c(nbl_db$Home, nbl_db$Away)))
#Set intial values to 1500 (Standard in elo ratings)
teams <- teams %>%
        mutate(elo = 1500)

#Set up elo score col (1 for a win, 0.5 for draw, 0 for loss)
nbl_db <- nbl_db %>%
        mutate(result = if_else(Home_Score > Away_Score, 1,
                                if_else(Home_Score == Away_Score, 0.5, 0)))

#Elo
#Set up the initial team elo values data frame
teams <- data.frame(team = unique(c(nbl_db$Home, nbl_db$Away)))
#Set intial values to 1500 (Standard in elo ratings)
teams <- teams %>%
        mutate(elo = 1500)

#Set up elo score col (1 for a win, 0.5 for draw, 0 for loss)
nbl_db <- nbl_db %>%
        mutate(result = if_else(Home_Score > Away_Score, 1,
                                if_else(Home_Score == Away_Score, 0.5, 0)))

#Implement the elo rating algorithm over the data set
#Each game is evaluated and elo scores given, then summed and stored in the 'teams' df
for (i in seq_len(nrow(nbl_db))) {
        game <- nbl_db[i, ]
        
        # Pre-match ratings
        teamA_elo <- subset(teams, team == game$Home)$elo
        teamB_elo <- subset(teams, team == game$Away)$elo
        
        # Let's update our ratings
        new_elo <- elo.calc(wins.A = game$result,
                            elo.A = teamA_elo,
                            elo.B = teamB_elo,
                            k = 20)
        
        # The results come back as a data.frame
        # with team A's new rating in row 1 / column 1
        # and team B's new rating in row 1 / column 2
        teamA_new_elo <- new_elo[1, 1]
        teamB_new_elo <- new_elo[1, 2]
        
        # We then update the ratings for teams A and B
        # and leave the other teams as they were
        teams <- teams %>%
                mutate(elo = if_else(team == game$Home, teamA_new_elo,
                                     if_else(team == game$Away, teamB_new_elo, elo)))
}

#This could be more accurate by taking into account the score differential. 


