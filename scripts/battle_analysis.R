angolaBattleAnalysis <- function(attacker_dice, attacker_bonus, defender_dice, defender_bonus)
{
  attacker_info <-array(rep(1:6, attacker_dice), c(6, attacker_dice))
  attacker_info <- split(attacker_info, rep(1:ncol(attacker_info), each = nrow(attacker_info)))
  
  defender_info <-array(rep(1:6, defender_dice), c(6, defender_dice))
  defender_info <- split(defender_info, rep(1:ncol(defender_info), each = nrow(defender_info)))
  
  attacker_rolls <- expand.grid(attacker_info) %>%
    mutate(max_value = do.call(pmax, (.))) %>%
    group_by(max_value) %>%
    summarise(result_count = n())
  
  defender_rolls <- expand.grid(defender_info) %>%
    mutate(max_value = do.call(pmax, (.))) %>%
    group_by(max_value) %>%
    summarise(result_count = n())
  
  results <- data.frame()
  
  for (i in 1:nrow(attacker_rolls))
  {
    attacker_value <- attacker_rolls$max_value[i] + attacker_bonus
    
    for (j in 1:nrow(defender_rolls))
    {
      defender_value <- defender_rolls$max_value[j] + defender_bonus
      
      net_value <- attacker_value - defender_value
      
      results <- results %>%
        bind_rows(data.frame(attacker_value = attacker_value, 
                             defender_value = defender_value, 
                             net_value = net_value,
                             result_count = attacker_rolls$result_count[i] * defender_rolls$result_count[j] ))
    }
  }
  
  results
}

angolaSummarizedBattleAnalysis <- function(battle_analysis)
{
  results <- battle_analysis %>%
    mutate(adjustment = cut(net_value, 
                            breaks = c(-99, -5, -3, -1, 2, 4, 6, 7), 
                            labels = c("Down 3 levels (-6 or worse)",
                                       "Down 2 levels (-4,-5)",
                                       "Down 1 level  (-2, -3)",
                                       "No Adjustment (+1, 0, -1)",
                                       "Up 1 level    (+3, +2)",
                                       "Up 2 levels   (+5, +4)",
                                       "Up 3 levels   (+6 or better)"),
                            right = FALSE)) %>%
    group_by(adjustment) %>%
    summarise(result_count = sum(result_count)) %>%
    mutate(result_percent = result_count / sum(result_count)) %>%
    mutate(cum_percent_lower = cumsum(result_percent),
           cum_percent_higher = 1 - cum_percent_lower + result_percent)
}

agolaCombatResults <- function(battle_summary, attack_strength, defense_strength)
{
  result_table <- data.frame(id = 9:1,
                             attack_strength = c(6, 5, 4, 3, 2, 1, 1, 1, 1),
                             defense_strength = c(1, 1, 1, 1, 1, 1, 2, 3, 4),
                             combat_odds = c("6:1", "5:1", "4:1", "3:1", "2:1", "1:1", "1:2", "1:3", "1:4"),
                             result_descr = c("Defenders Eliminated", 
                                              "2/3rds Defenders Eliminated; Defenders Retreat", 
                                              "1/2 Defenders Eliminated; Defenders Retreat", 
                                              "1/3rd Defenders Eliminated; Defenders Retreat",
                                              "Defenders Retreat",
                                              "Combat Continues",
                                              "Attackers Retreat",
                                              "1/3rd Attackers Eliminated; Attackers Retreat",
                                              "1/2 Attackers Eliminated; Attackers Retreat")) %>%
    mutate(combat_ratio = attack_strength / defense_strength)
  
  combat_ratio <- attack_strength / defense_strength
  
  initial_id <- 0
  
  for (i in 1:nrow(result_table))
  {
    if (i == 1 && combat_ratio >= result_table$combat_ratio[i])
    {
      initial_id <- result_table$id[i]
      break()
    }
    
    if (i != nrow(result_table) && combat_ratio >= result_table$combat_ratio[i + 1] && combat_ratio < result_table$combat_ratio[i])
    {
      initial_id <- result_table$id[i + 1]
      break()
    }
    
    if (i == nrow(result_table) && combat_ratio <= result_table$combat_ratio[i])
    {
      initial_id <- result_table$id[i]
      break()
    }
    
  }
  
  adjustments <- battle_summary %>%
    mutate(id = case_when(adjustment == "Down 3 levels (-6 or worse)" ~ as.integer(max(initial_id - 3, 1)),
                          adjustment == "Down 2 levels (-4,-5)" ~ as.integer(max(initial_id - 2, 1)),
                          adjustment == "Down 1 level  (-2, -3)" ~ as.integer(max(initial_id - 1, 1)),
                          adjustment == "No Adjustment (+1, 0, -1)" ~ initial_id,
                          adjustment == "Up 1 level    (+3, +2)" ~ as.integer(min(initial_id + 1, 9)),
                          adjustment == "Up 2 levels   (+5, +4)" ~ as.integer(min(initial_id + 2, 9)),
                          adjustment == "Up 3 levels   (+6 or better)" ~ as.integer(min(initial_id + 3, 9))))
  
  result <- result_table %>%
    left_join(select(adjustments, id, result_percent), "id") %>%
    mutate(result_percent = if_else(is.na(result_percent), 0.0, result_percent))
  
}

determineDiceCounts <- function(terrain, defending_town_or_city, defending_escarpment, defending_minefield)
{
  attacker_dice <- 1
  defender_dice <- 1
  
  if (terrain == "Clear") {
    attacker_dice <- 2
  } else if (terrain == "Hills or Jungle") {
    defender_dice <- 2
  }
  
  defender_dice <- ifelse(defending_town_or_city, defender_dice + 1, defender_dice)
  defender_dice <- ifelse(defending_escarpment, defender_dice + 1, defender_dice)
  defender_dice <- ifelse(defending_minefield, defender_dice + 1, defender_dice)
  
  list(attacker_dice = attacker_dice, defender_dice = defender_dice)
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

