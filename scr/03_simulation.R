
# Settings and running the simulation
num_games <- 10000
num_decks <- 6
num_players <- 2
basic_strategy <- load_basic_strategy("./data/basic_strategy.csv")
count_values <- load_card_count_values("./data/count_system.csv")
reshuffle_threshold <- 0.25

# Game rules
can_split <- TRUE
can_double_down <- TRUE
can_double_after_split <- TRUE
stand_soft_17 <- FALSE
can_insurance <- TRUE
can_surrender <- TRUE
blackjack_pays <- 1.5

max_splits <- 2

# Bet sizing
min_bet <- 10  # Minimum bet size
max_bet <- 100  # Maximum bet size
bet_spread <- 10  # Factor to adjust bet size based on true count

results <- simulate_blackjack(num_games, num_decks, basic_strategy, count_values, reshuffle_threshold, can_split, can_double_down, max_splits, num_players, stand_soft_17)

# Print or analyze results
tmpresults <- vector("list", num_games)
for(i in 1:num_games){
  tmpresults[[i]] <- calculate_winners(results[[i]])
}

results_df <- as.data.frame(do.call(rbind,tmpresults))

results_df %>%
  mutate(p1 = cumsum(V1), p2 = cumsum(V2)) %>%
  mutate(gameid=row_number()) %>% 
  select(p1, p2, gameid) %>%
  pivot_longer(cols = c('p1','p2')) %>% 
  ggplot(aes(x = gameid, y=value, colour = name)) +
  geom_step() + theme_minimal()


results_df %>%
  mutate(p1 = cumsum(V1)) %>%
  mutate(gameid=row_number()) %>% 
  select(p1, gameid) %>%
  #pivot_longer(cols = c('p1','p2')) %>% 
  ggplot(aes(x = gameid, y=p1)) +
  geom_step() + theme_minimal()
