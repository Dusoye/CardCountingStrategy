
# Settings and running the simulation
num_games <- 1000
num_decks <- 4
num_players <- 1
basic_strategy <- load_basic_strategy("./data/basic_strategy.csv")
count_values <- load_card_count_values("./data/count_system.csv")
reshuffle_threshold <- 0.25

# Game rules
can_split <- TRUE
can_double_down <- TRUE
can_double_after_split <- TRUE
stand_soft_17 <- TRUE
can_insurance <- FALSE
can_surrender <- FALSE

max_splits <- 3

# Bet sizing
min_bet <- 10  # Minimum bet size
max_bet <- 100  # Maximum bet size
bet_spread <- 10  # Factor to adjust bet size based on true count

results <- simulate_blackjack(num_games, num_decks, basic_strategy, count_values, reshuffle_threshold, can_split, can_double_down, max_splits)

# Print or analyze results
