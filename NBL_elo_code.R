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
data_2017 <- read_excel("~/Dropbox/Personal/Jobs/NBL Elo Project/data_2017.xlsx")
#Clean: separate the score and date columns 
data_2017 <- separate(data_2017, Score, into = c("Home_Score", "Away_Score"), sep = "\\:", remove = TRUE)
data_2017 <- separate(data_2017, Date_Time, into = c("Date", "Time"), sep = "\\ ", remove = TRUE)
#Delete incomplete rows
data_2017 <- data_2017[complete.cases(data_2017),]


