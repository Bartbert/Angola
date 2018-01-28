library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Anglola Battle Calculator"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("attackStrength", "Attacking Strength:", min = 1, max = 18, value = 1),
      sliderInput("attackBonus", "Attacking Dice Bonus:", min = 0, max = 5, value = 0),
      br(),
      sliderInput("defenseStrength", "Defending Strength:", min = 1, max = 18, value = 1),
      sliderInput("defenseBonus", "Defending Dice Bonus:", min = 0, max = 5, value = 0),
      checkboxInput("defendTownCity", label = "Defending Town/City", value = FALSE),
      checkboxInput("defendEscarpment", label = "Defending Upside Escarpment", value = FALSE),
      checkboxInput("defendMinefield", label = "Defending Minefield", value = FALSE),
      br(),
      selectInput("terrainType", "Defending Terrain:", choices = c("Clear", "Savannah", "Hills or Jungle")),
      br(),
      actionButton("btnRecalc", "Recalculate")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(fluidRow(column(width = 12, dataTableOutput("tblBattleResults"))))
    )
  )
))
