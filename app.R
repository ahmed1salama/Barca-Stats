library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(fmsb)
library(dashboardthemes)
source("main.R")
source("Draw_Pitch.R")

ui <- dashboardPage(skin = "purple", title = "Barca-Stats dashboard",
  dashboardHeader(title = shinyDashboardLogo(
    theme = "grey_dark",
    boldText = "Barca",
    mainText = "Stats"
  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home", lib = "glyphicon")),
      menuItem("Players Stats", tabName = "players_stats", icon = icon("stats", lib = "glyphicon")),
      menuItem("El Clasico", tabName = "el_classico", icon = icon("fire", lib = "glyphicon")),
      menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(
                infoBox(
                  color  = "purple",
                  "Team",
                  "Barcelona",
                  width = 4,
                  fill = T,
                  icon = icon("user", lib = "glyphicon")
                ),
                infoBox(
                  color  = "navy",
                  "Season",
                  "2005/2006",
                  width = 4,
                  fill = T,
                  icon = icon("calendar", lib = "glyphicon")
                ),
                infoBox(
                  color  = "black",
                  "League",
                  "La Liga",
                  width = 4,
                  fill = T,
                  icon = icon("home", lib = "glyphicon")
              )),

              fluidRow(
                infoBoxOutput("matches_num", width = 2),
                infoBoxOutput("wins", width = 2),
                infoBoxOutput("losses", width = 2),
                infoBoxOutput("draws", width = 2),
                infoBoxOutput("goals_for", width = 2),
                infoBoxOutput("goals_against", width = 2)
              ),

              fluidRow(
                box(
                  h2("Matches Info Table"),
                    hr(),
                  dataTableOutput("matches_table"),
                  width = 12
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "players_stats",
              fluidRow(
                column(width = 9,
                  box(width = NULL,
                    h2("Players Info Table"),
                    hr(),
                    dataTableOutput("player_table")
                  )
                ),
                column(width = 3,
                  box(width = NULL,
                    selectInput(inputId = "player_menu", label = "Select Player", choices = players_stats$player_name),
                    imageOutput(outputId = "player_image")
                  ),
                  box(width = NULL,
                      h2("Player Skills"),
                      hr(),
                      plotOutput(outputId = "radar")
                  )
                )
              )
      ),
      # Third tab content
      tabItem(tabName = "el_classico",
              fluidRow(
                column(width = 2, offset = 2,
                         box(width = NULL, align = "center",
                             
                             imageOutput(outputId = "home_team_photo", height = 250),
                             textOutput("home_team_name")
                             
                         )
                       ),
                column(width = 2, offset = 0,
                         box(width = NULL, align = "center",
                             
                             imageOutput(outputId = "away_team_photo", height = 250),
                             textOutput("away_team_name")
                             
                         )
                       ),
                column(width = 2,
                  box(width = NULL, align = "left",
                      h2("Match Info"),
                      hr(),
                      htmlOutput("match_result"),
                      htmlOutput("match_date"),
                      htmlOutput("match_week"),
                      htmlOutput("match_stadium"),
                      htmlOutput("match_referee")
                    
                  )
                )
              ),
              fluidRow(
                       box(height = 600,
                           h2("Shots, goals and expected goals based on statsbomb expected goals model"),
                           hr(),
                           plotOutput(outputId = "shots_xg_plot", height = 450)
                        ),
                       column(width = 6,   
                         box(width = NULL,
                             h2("Discover Player Passes"),
                             hr(),
                             selectInput("player_name", label = "Select Player", choices = classico_events[classico_events$team.name == "Barcelona", "player.name"] %>% unique()),
                             plotOutput("player_passes")  
                         ),
                         
                          
                         )
                       
                
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                box(
                  h2("About"),
                  h4("This dashboard demonstrates some statistics about Barcelona FC, season 2005/2006 like matches statistcs and 
                    players performance."),
                  br(),
                  h4("Total Available matches 17 matches from statsbomb free data."),
                  a("Data", href = "https://github.com/statsbomb/open-data", target = "_blank"),
                  br(),
                  h4("Choice of Barcelona team was based on that all the 17 matches played by Barcelona vs other spanish teams."),
                  br(),
                  h4("This app is for educational purposes only !!")
                )
              ),
              fluidRow(
                box(
                  h2("Creator"),
                  h4("Ahmed Salama Hamed"),
                  h5(a("Github", href = "https://github.com/ahmed1salama", target = "_blank"), "|",
                  a("Linkedin", href = "https://www.linkedin.com/in/ahmed-salama-272a68130/", target = "_blank"))
                )
              )
              
            )
    
    )
  )
)


server <- function(input, output) {
  
  # home tab summary part 
  output$matches_num = renderInfoBox({
    infoBox(
      color  = "yellow",
      title = "Available 
      Matches",
      value = matches_num,
      icon = icon("plus", lib = "glyphicon"))
  })
  
  output$wins = renderInfoBox({
    infoBox(
      color  = "green",
      title = "Wins",
      value = wins,
      icon = icon("hand-up", lib = "glyphicon")
    )
  })
  
  output$losses = renderInfoBox({
    infoBox(
      color  = "red",
      title = "Losses",
      value = losses,
      icon = icon("hand-down", lib = "glyphicon")
    )
  })
  
  output$draws = renderInfoBox({
    infoBox(
      color  = "blue",
      title = "Draws",
      value = draws,
      icon = icon("resize-horizontal", lib = "glyphicon")
    )
  })
  
  output$goals_for = renderInfoBox({
    infoBox(
      color  = "green",
      title = "Goals For",
      value = goals_for,
      icon = icon("record", lib = "glyphicon")
    )
  })
  
  output$goals_against = renderInfoBox({
    infoBox(
      color  = "red",
      title = "Goals Against",
      value = goals_against,
      icon = icon("remove", lib = "glyphicon")
    )
  })
  
  
  # home tab macthes table part 
  output$matches_table = renderDataTable({
    datatable(matches[,c(1, 6, 8, 2, 3, 4, 10, 12)], colnames = c("Date",
                                                                  "Home Team",
                                                                  "Away Team",
                                                                  "Home Score",
                                                                  "Away Score",
                                                                  "Match Week",
                                                                  "Stadium",
                                                                  "Referee"),
              options = list(pageLength = matches_num,
                             columnDefs = list(list(className = 'dt-left', targets = "_all")),
                             scrollX = TRUE, autoWidth = FALSE
              ))
  })
  
  # reactive value for image and radar part
  selected_player = reactive({
    input$player_menu
  })
  
  # players stats tab players part
  output$player_image = renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path('www',
                                        paste(players_stats[players_stats$player_name == selected_player(), "player_id"],'.jpg', sep = '')))
    
    # Return a list containing the filename
    list(src = filename,
         height = "100%",
         width = "100%"
         )
  }, deleteFile = FALSE)
  
  # players stats radar part
  output$radar = renderPlot({
    colnames(fifa_stats) <- c("player_id", "Passing" , "Shooting" , "Physical" , "Defence", "Speed", "Dribble")
    radar_df = players_stats %>%
                  filter(player_name == selected_player()) %>%
                  inner_join(fifa_stats, by = c("player_id")) %>%
                  select(Passing:Dribble)

    radar_df <- rbind(rep(100,ncol(radar_df)) , rep(0,ncol(radar_df)) , radar_df)

    radarchart(radar_df,axistype=1 , 
               #custom polygon
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , palcex = 5,
               
               #custom the grid
               cglcol="grey", cglty=6, axislabcol="grey", cglwd=1,plty = 1,
               
               #custom labels
               vlcex=1 )
  })
  
  # players stats tab players table 
  output$player_table = renderDataTable({
    datatable(players_stats[,c(2, 9, 10, 8, 11, 3, 4, 5, 6, 7, 12)], colnames = c(
                                               "Player Name",
                                               "Age",
                                               "Height",
                                               "Jersey",
                                               "Position",
                                               "Played Minutes",
                                               "Matches",
                                               "Shots",
                                               "Goals",
                                               "Shots Per Match",
                                               "Over All Rating"),
              options = list(pageLength = nrow(players_stats),
                             columnDefs = list(list(className = 'dt-left', targets = "_all")),
                             scrollX = TRUE, autoWidth = FALSE))
  })
  
  
  # El classico tab teams part
  
  match_info = matches %>% filter(home_team.home_team_name == "Real Madrid" | away_team.away_team_name == "Real Madrid") %>%
    as.data.frame()
    
  
  
  output$home_team_photo = renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg

     
    filename <- normalizePath(file.path('www',
                                        paste(match_info[, "home_team.home_team_id"] ,'.png', sep = '')))
    
    # Return a list containing the filename
    list(src = filename,
         height = 250,
         width = "100%"
         # height = "100%",
         # width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$home_team_name = renderText({
    
    as.character(match_info[,"home_team.home_team_name"])
  }) 
  
  output$away_team_photo = renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    
    
    filename <- normalizePath(file.path('www',
                                        paste(match_info[, "away_team.away_team_id"] ,'.png', sep = '')))
    
    # Return a list containing the filename
    list(src = filename,
         height = 250,
         width = "100%"
         # height = "100%",
         # width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$away_team_name = renderText({
    as.character(match_info[, "away_team.away_team_name"])
  }) 
  
  # El Classico tab match info part 
  
  output$match_result = renderText(paste(strong("Result:"), as.character(match_info[,"home_team.home_team_name"]), 
                                         match_info[, "home_score"], "-",
                                         match_info[,"away_score"],
                                         as.character(match_info[,"away_team.away_team_name"])
                                         ))
  output$match_date = renderText(paste(strong("Date:"), match_info[, "match_date"]))
 
  output$match_week = renderText(paste(strong("Week:"), match_info[, "match_week"]))
  
  output$match_stadium = renderText(paste(strong("Stadium:"), match_info[, "stadium.name"]))
  
  output$match_referee = renderText(paste(strong("Referee:"), match_info[, "referee.name"]))
  
  # El classico tab shots summary plot
  
  output$shots_xg_plot = renderPlot({ 
    
    hori5 + geom_point(data = classico_events %>% filter(type.name == "Shot", team.name == match_info[,"away_team.away_team_name"]), aes(x, y, color = team.name, alpha = shot.statsbomb_xg , shape = shot.outcome.name == "Goal"), size = 5) + scale_color_manual(values=c("#00008B", "#FFFAFA"))
    
  })
  
  # El classico tab player passes plot
  # reactive value for player name select menu
  selected_player_name = reactive({
    input$player_name
  })
  
  output$player_passes = renderPlot({ 
    
    hori5 + geom_segment(data=classico_events %>% filter(type.name == "Pass", player.name == selected_player_name()), aes(x=x,xend=pass_end_x,
                                                y=y,yend=pass_end_y),size=1.5,color = "#00008B", arrow=arrow(length = unit(0.03, "npc")))
    
  })
}

shinyApp(ui, server)

