# Initialize the deck
initialize_deck <- function(num_decks) {
  #suits <- c("♣", "♠", "♦", "♥")
  values <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
  deck <- rep(values, 4) # For each suit
  #deck <- expand.grid(value = values, suit = suits)
  deck <- data.table(Value = rep(deck, num_decks)) # For each deck
  return(deck)
}

# Shuffle the deck
shuffle_deck <- function(deck) {
  return(deck[sample(nrow(deck)),])
}


check_reshuffle <- function(deck, num_decks, reshuffle_threshold) {
  num_cards_remaining <- nrow(deck)
  total_cards <- num_decks * 52  
  if ((num_cards_remaining / total_cards) <= reshuffle_threshold) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Deal a card
deal_card <- function(deck, count, count_values) {
  card <- deck[1]
  remaining_deck <- deck[-1]
  num_cards_remaining <- nrow(remaining_deck)
  num_decks_remaining <- num_cards_remaining / 52  
  
  updated_count <- 0#update_count(count, card, count_values, num_decks_remaining)
  return(list("card" = card, "deck" = remaining_deck, "count" = updated_count))
}


# Evaluate hand value
evaluate_hand <- function(hand) {
  values <- hand$Value
  values[values == "J" | values == "Q" | values == "K"] <- 10
  values[values == "A"] <- 11
  
  hand_value <- sum(as.integer(values))
  num_aces <- sum(hand$Value == "A")
  while (hand_value > 21 && num_aces > 0) {
    hand_value <- hand_value - 10
    num_aces <- num_aces - 1
  }
  
  return(hand_value)
}

load_card_count_values <- function(file_path) {
  count_values <- read.csv(file_path, header = TRUE)
  return(count_values)
}

# Calculate size of the bet given the bet spread and true count
calculate_bet_size <- function(true_count, min_bet, max_bet, bet_spread) {
  if (true_count <= 1) {
    return(min_bet)
  } else {
    bet_size <- min_bet * bet_spread * (true_count - 1)
    return(min(bet_size, max_bet))
  }
}

# Updates the running count of the current shoe
update_count <- function(count, card, count_values, num_decks_remaining) {
  card_value <- as.character(card$Value)
  count_change <- count_values[count_values$Card == card_value, "Count"]
  if (length(count_change) > 0) {
    count$running <- count$running + count_change
  }
  count$true <- calculate_true_count(count$running, num_decks_remaining)
  return(count)
}

# Calculate the true count
calculate_true_count <- function(running_count, num_decks_remaining) {
  if (num_decks_remaining > 0) {
    return(running_count / num_decks_remaining)
  } else {
    return(running_count)
  }
}

load_basic_strategy <- function(file_path) {
  strategy <- read.csv(file_path, header = TRUE)
  return(strategy)
}
# S = Stand
# H = Hit
# Dh = Double (if not allowed, then hit)
# Ds = Double (if not allowed, then stand)
# SP = Split
# Uh = Surrender (if not allowed, then hit)
# Us = Surrender (if not allowed, then stand)
# Usp = Surrender (if not allowed, then split)

# Determine action based on strategy
determine_action <- function(player_hand, dealer_card, basic_strategy) {
  dealer <- dealer_hand[1]
  player_value <- evaluate_hand(player_hand)
}

# Player's turn
player_turn <- function(deck, player_hand, dealer_card, basic_strategy, can_double_down, max_splits, splits_done = 0, is_split_hand = FALSE) {
  action <- determine_action(player_hand, dealer_card, basic_strategy, is_split_hand)
  
  while (action != "stand") {
    if (action == "hit") {
      result <- deal_card(deck)
      player_hand <- rbind(player_hand, result$card)
      deck <- result$deck
    } else if (action == "double") {
      return(double_down(deck, player_hand, can_double_down, is_split_hand))
    } else if (action == "split") {
      return(split_hand(deck, player_hand, max_splits, splits_done, basic_strategy, can_double_down))
    }
    
    action <- determine_action(player_hand, dealer_card, basic_strategy, is_split_hand)
  }
  
  return(list("hand" = player_hand, "deck" = deck))
}

# Logic for splitting hands
split_hand <- function(deck, player_hand, max_splits, splits_done, basic_strategy, can_double_down) {
  if (nrow(player_hand) != 2 || player_hand[1, "Value"] != player_hand[2, "Value"] || splits_done >= max_splits) {
    return(list("hand" = player_hand, "deck" = deck))
  }
  
  split_hands <- list()
  for (i in 1:2) {
    new_hand <- player_hand[i, , drop = FALSE]
    result <- deal_card(deck)
    new_hand <- rbind(new_hand, result$card)
    deck <- result$deck
    split_hands[[i]] <- new_hand
  }
  
  final_hands <- list()
  for (split_hand in split_hands) {
    if (nrow(split_hand) == 2 && split_hand[1, "Value"] == "A") {
      # Some casinos only allow one card on each Ace when splitting
      result <- deal_card(deck)
      final_hands <- c(final_hands, list(rbind(split_hand, result$card)))
      deck <- result$deck
    } else {
      result <- player_turn(deck, split_hand, basic_strategy, can_double_down, max_splits, splits_done + 1)
      final_hands <- c(final_hands, list(result$hand))
      deck <- result$deck
    }
  }
  
  return(list("hands" = final_hands, "deck" = deck))
}

# Logic for doubling down
double_down <- function(deck, player_hand, can_double_after_split, is_split_hand) {
  if (nrow(player_hand) != 2 || (is_split_hand && !can_double_after_split)) {
    return(list("hand" = player_hand, "deck" = deck))
  }
  
  # Player can only double down on the first two cards
  result <- deal_card(deck)
  player_hand <- rbind(player_hand, result$card)
  deck <- result$deck
  
  return(list("hand" = player_hand, "deck" = deck, "doubled" = TRUE))
}


# Main game simulation
simulate_blackjack <- function(num_games, num_decks, basic_strategy, count_values, reshuffle_threshold, can_split, can_double_down, max_splits) {
  basic_strategy <- load_basic_strategy(basic_strategy)
  results <- vector("list", num_games)
  for (i in 1:num_games) {
    deck <- shuffle_deck(initialize_deck(num_decks))
    count <- list("running" = 0, "true" = 0)
    player_hand <- data.frame()
    dealer_hand <- data.frame()
    
    # Game loop
    while (TRUE) {
      # Check if reshuffling is needed
      if (check_reshuffle(deck, num_decks, reshuffle_threshold)) {
        deck <- shuffle_deck(initialize_deck(num_decks))
        # Optionally reset the count here if playing with card counting
        count <- list("running" = 0, "true" = 0)
      }
      
      # ... existing code for dealing cards and playing the game ...
      
      # Break the loop or continue to the next game based on game logic
    }
  }
  
  # ... return results ...
}
  }
  return(results)
}
