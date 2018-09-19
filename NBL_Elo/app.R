#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

nbl_db <- read_csv("nbl_db.csv")
teams <- data.frame(team = unique(c(nbl_db$Home, nbl_db$Away)))
teams <- teams %>%
        mutate(elo = 1500)
# Define UI for application that draws a histogram
ui <- fluidPage(
        titlePanel(title=div(img(src="nbl_logo.jpg", height = 50, width = 50), "Elo Ratings")),
        sidebarLayout(
                sidebarPanel(
                        sliderInput("k", label = "k", min = 0, max = 40, value = 20, step = 5)
                ),
                mainPanel(
                        tableOutput(outputId = "Eloratings")
                )
        )
)

# Define server logic ----
server <- function(input, output) {
        
        output$Eloratings <- renderTable({
                
                k <- input$k
                for (i in seq_len(nrow(nbl_db))) {
                        game <- nbl_db[i, ]
                        
                        # Pre-match ratings
                        teamA_elo <- subset(teams, team == game$Home)$elo
                        teamB_elo <- subset(teams, team == game$Away)$elo
                        
                        # Let's update our ratings
                        new_elo <- elo.calc(wins.A = game$result,
                                            elo.A = teamA_elo,
                                            elo.B = teamB_elo,
                                            k = k, adjust.A = 100)
                        
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
                arrange(teams, desc(elo))
        })
}


# Run the app ----
shinyApp(ui = ui, server = server)
