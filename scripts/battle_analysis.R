angolaBattleAnalysis <- function(attacker_dice, attacker_bonus, defender_dice, defender_bonus)
{
  attacker_info <-array(rep(1:6, attacker_dice), c(6, attacker_dice))
  attacker_info <- split(attacker_info, rep(1:ncol(attacker_info), each = nrow(attacker_info)))
  
  defender_info <-array(rep(1:6, defender_dice), c(6, defender_dice))
  defender_info <- split(defender_info, rep(1:ncol(defender_info), each = nrow(defender_info)))
  
  attacker_rolls <- expand.grid(attacker_info)
  defender_rolls <- expand.grid(defender_info)
  
  results <- data.frame()
  
  for (i in 1:nrow(attacker_rolls))
  {
    attacker_value <- max(attacker_rolls[i, ]) + attacker_bonus
    
    for (j in 1:nrow(defender_rolls))
    {
      defender_value <- max(defender_rolls[j, ]) + defender_bonus
      
      net_value <- attacker_value - defender_value
      
      results <- results %>%
        bind_rows(data.frame(attacker_value = attacker_value, defender_value = defender_value, net_value = net_value))
    }
  }
  
  results
}

angolaSummarizedBattleAnalysis <- function(battle_analysis)
{
  results <- battle_analysis %>%
    mutate(adjustment = cut(net_value, 
                            breaks = c(-99, -5, -3, -1, 2, 4, 6, 7), 
                            labels = c("-6 or worse down 3 levels",
                                       "-4,-5 down 2 levels",
                                       "-2, -3 down 1 level",
                                       "+1, 0, -1 no adjustment",
                                       "+3, +2 up 1 level",
                                       "+5, +4 up 2 levels",
                                       "+6 or better up 3 levels"),
                            right = FALSE)) %>%
    group_by(adjustment) %>%
    summarise(result_count = n()) %>%
    mutate(result_percent = result_count / sum(result_count)) %>%
    mutate(cum_percent_lower = cumsum(result_percent),
           cum_percent_higher = 1 - cum_percent_lower + result_percent)
}

testBattleResults <- function()
{
  attacker_dice <- 1
  attacker_bonus <- 0
  
  defender_dice <- 3
  defender_bonus <- 0
  
  battle_analysis <- angolaBattleAnalysis(attacker_dice, attacker_bonus, defender_dice, defender_bonus)
  battle_summary <- angolaSummarizedBattleAnalysis(battle_analysis)
}

