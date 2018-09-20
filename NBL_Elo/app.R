#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(elo)

nbl_db <- read_csv("cleaned_nbl_db.csv")
teams <- data.frame(team = unique(c(nbl_db$Home, nbl_db$Away)))
teams <- teams %>%
        mutate(elo = 1500)
# Define UI for application that draws a histogram
ui <- fluidPage(
        titlePanel(title=div(img(src="nbl_logo.jpg", height = 50, width = 50), "Elo Ratings")),
        sidebarLayout(
                sidebarPanel(
                        sliderInput("k", label = "k", min = 0, max = 40, value = 20, step = 5)#,
                        #selectInput("Season", label = "Season", choices =  list("All", "2016/17", "2017/18")),
                        #checkboxGroupInput("Elo Calc Type", "Elo Calculation Method", choices = list("Standard", "Include Home Team Advantage", "Include Margin of Victory"))
                        
                ),
                mainPanel(
                        #tableOutput(outputId = "Eloratings"),
                        plotOutput(outputId = "Eloplot")
                )
        )
)

# Define server logic ----
server <- function(input, output) {
       
        # output$Eloratings <- renderTable({
        #         
        #         k <- input$k
        #         #Standard elo rating with no extensions
        #         nbl_elo_calcs <- elo.run(nbl_db$result ~ nbl_db$Home + nbl_db$Away, data = nbl_db, k = k)
        #         nbl_elo_ratings <- data.frame(Elo.Ratings = final.elos(nbl_elo_calcs))
        #         nbl_elo_ratings <- rownames_to_column(nbl_elo_ratings, "Teams")
        #         nbl_elo_ratings <- arrange(nbl_elo_ratings, desc(Elo.Ratings))

                
        output$Eloplot <- renderPlot({
                k <- input$k
                nbl_db$G <- abs(nbl_db$Home_Score - nbl_db$Away_Score)
        
                ##--Standard Elo ratings--##
                nbl_elo_calcs <- elo.run(nbl_db$result ~ nbl_db$Home + nbl_db$Away, data = nbl_db, k = k)
                nbl_elo_ratings <- data.frame(Elo.Ratings = final.elos(nbl_elo_calcs))
                nbl_elo_ratings <- rownames_to_column(nbl_elo_ratings, "Teams")
                nbl_elo_ratings <- arrange(nbl_elo_ratings, desc(Elo.Ratings))
   
                ##--Elo rating with home team advantage added--##
                nbl_elo_calcs_adj4hc <- elo.run(nbl_db$result ~ adjust(nbl_db$Home, 100) + nbl_db$Away, data = nbl_db, k = k)
                nbl_elo_ratings_adj4hc <- data.frame(Elo.Ratings = final.elos(nbl_elo_calcs_adj4hc))
                nbl_elo_ratings_adj4hc <- rownames_to_column(nbl_elo_ratings_adj4hc, "Teams")
                nbl_elo_ratings_adj4hc <- arrange(nbl_elo_ratings_adj4hc, desc(Elo.Ratings))
                
                ##--Elo ratings with home team adv and margin of victory--##
                #nbl_elo_calcs <- elo.run(nbl_db$result ~ adjust(nbl_db$Home, 100)+ nbl_db$Away + k(20*log(G) + 1), data = nbl_db)
                #Set up the initial team elo values data frame
                teams <- data.frame(team = unique(c(nbl_db$Home, nbl_db$Away)))
                
                #FiveThirtyEight Method using elo.calc and a for loop
                #Set intial values to 1500 (Standard in elo ratings)
                teams <- teams %>%
                        mutate(elo = 1500)
                
                nbl_db$G <- abs(nbl_db$Home_Score - nbl_db$Away_Score)
                
                for (i in seq_len(nrow(nbl_db))) {
                        game <- nbl_db[i, ]
                        
                        # Pre-match ratings
                        teamA_elo <- subset(teams, team == game$Home)$elo
                        teamB_elo <- subset(teams, team == game$Away)$elo
                        
                        # Let's update our ratings
                        new_elo <- elo.calc(wins.A = game$result,
                                            elo.A = teamA_elo,
                                            elo.B = teamB_elo,
                                            k = k*(((game$G + 3)^0.8) / (7.5 + 0.006*(
                                                    ifelse(game$Home_Score > game$Away_Score, teamA_elo, teamB_elo) -
                                                            ifelse(game$Home_Score > game$Away_Score, teamB_elo, teamA_elo) +
                                                            ifelse(game$Home_Score > game$Away_Score, 100, -100)))))
                        
                        teamA_new_elo <- new_elo[1, 1]
                        teamB_new_elo <- new_elo[1, 2]
                        
                        teams <- teams %>%
                                mutate(elo = if_else(team == game$Home, teamA_new_elo,
                                                     if_else(team == game$Away, teamB_new_elo, elo)))
                        teams <- arrange(teams, desc(elo))
                }
                #Taking the 3 elo calcs and combining into a dataframe for plotting
                all_elo_ratings <- data.frame(std = nbl_elo_ratings, hca = nbl_elo_ratings_adj4hc$Elo.Ratings, margin = teams$elo)
                all_elo_ratings2 <- data.frame(std = nbl_elo_ratings, hca = nbl_elo_ratings_adj4hc$Elo.Ratings, margin = teams$elo)
                colnames(all_elo_ratings) <- c("Teams", "Standard", "Home Court", "Margin")
                colnames(all_elo_ratings2) <- c("Teams", "Standard", "Home Court", "Margin")
                all_elo_ratings <- gather(all_elo_ratings, key = "Elo_Type", value = "Elo_Rating", Standard:Margin)
                all_elo_ratings2$mean <- rowMeans(all_elo_ratings2[,2:4])
                all_elo_ratings2 <- arrange(all_elo_ratings2, desc(mean))
                team_order <- all_elo_ratings2$Teams
                
                #plot
                ggplot(data = all_elo_ratings, aes(x = factor(Teams, level = team_order), y = Elo_Rating, col = Elo_Type, group = Elo_Type)) +
                        geom_jitter(width = 0.15, size = 5) +
                        theme(axis.title.x=element_blank(),
                              axis.title.y=element_blank())

        
        })
}


# Run the app ----
shinyApp(ui = ui, server = server)
