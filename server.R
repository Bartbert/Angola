library(shiny)

shinyServer(function(input, output) {

  output$tblBattleResults <- DT::renderDataTable({
    
    column_names <- c("Adjustment Level",
                      "Expected Occurrences",
                      "Expected Probability (%)",
                      "Probability at or Below (%)",
                      "Probability at or Above (%)")
    
    datatable(data = battle_summary(),
              colnames = column_names, 
              rownames = FALSE, 
              filter = "none", 
              autoHideNavigation = TRUE, 
              options = list(
                autoWidth = TRUE,
                scrollX = TRUE,
                dom = "tB")) %>%
      formatPercentage(c("result_percent", "cum_percent_lower", "cum_percent_higher"), digits = 2)
    
  })
  
  battle_summary <- reactive({
    
    dice <- determineDiceCounts(terrain = input$terrainType, 
                                defending_town_or_city = input$defendTownCity, 
                                defending_escarpment = input$defendEscarpment, 
                                defending_minefield = input$defendMinefield)
    
    attacker_dice <- dice$attacker_dice
    attacker_bonus <- input$attackBonus
    
    defender_dice <- dice$defender_dice
    defender_bonus <- input$defenseBonus
    
    result <- angolaBattleAnalysis(attacker_dice, attacker_bonus, defender_dice, defender_bonus) %>%
      angolaSummarizedBattleAnalysis()
    
  })
  
})
