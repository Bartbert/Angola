library(shiny)

shinyServer(function(input, output) {

  output$tblBattleResults <- renderDataTable({
    
    dice <- determineDiceCounts(terrain = input$terrainType, 
                                defending_town_or_city = input$defendTownCity, 
                                defending_escarpment = input$defendEscarpment, 
                                defending_minefield = input$defendMinefield)
    
    attacker_dice <- dice$attacker_dice
    attacker_bonus <- input$attackBonus
    
    defender_dice <- dice$defender_dice
    defender_bonus <- input$defenseBonus

    battle_analysis <- angolaBattleAnalysis(attacker_dice, attacker_bonus, defender_dice, defender_bonus)
    battle_summary <- angolaSummarizedBattleAnalysis(battle_analysis)
    
  })
  
})
