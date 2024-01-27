
# Settings and running the simulation
num_games <- 1000
num_decks <- 4
basic_strategy <- load_basic_strategy("./data/basic_strategy.csv")
count_values <- load_card_count_values("./data/count_values.csv")
reshuffle_threshold <- 0.25
can_split <- TRUE
can_double_down <- TRUE
max_splits <- 3

min_bet <- 10  # Minimum bet size
max_bet <- 100  # Maximum bet size
bet_spread <- 10  # Factor to adjust bet size based on true count

results <- simulate_blackjack(num_games, num_decks, basic_strategy, count_values, reshuffle_threshold, can_split, can_double_down, max_splits)

# Print or analyze results
