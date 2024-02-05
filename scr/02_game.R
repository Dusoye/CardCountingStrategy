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

# Check whether a reshuffle is required after the current hand
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

# Determine action based on strategy
determine_action <- function(player_hand, dealer_upcard, basic_strategy_df) {
  player_total <- evaluate_hand(player_hand)  # Evaluate player's hand total
  dealer_upcard_value <- as.character(dealer_upcard$Value)  # Ensure dealer upcard is character for comparison
  
  # Handle face cards and Aces for the dealer
  if (dealer_upcard_value %in% c("J", "Q", "K")) {
    dealer_upcard_value <- "10"
  } else if (dealer_upcard_value == "A") {
    dealer_upcard_value <- "11"
  }
  
  # Determine if the hand is soft, hard, or a pair
  hand_type <- ifelse(nrow(player_hand) == 2 && player_hand$Value[1] == player_hand$Value[2], "Pair",
                      ifelse(any(player_hand$Value == "A") && player_total <= 21, "Soft", "Hard"))
  
  # Adjust for Aces in pairs
  if(hand_type == "Pair" && player_hand$Value[1] == "A") {
    #hand_type <- "Soft"  # Treat pairs of Aces as soft hands
    recommended_action_row <- data.table(Action = 'SP') # Set to always split aces, dealing with soft value
  } else {
  # Look up the recommended action from the strategy dataframe
  recommended_action_row <- basic_strategy_df[basic_strategy_df$Player == player_total & 
                                                basic_strategy_df$Dealer == dealer_upcard_value & 
                                                basic_strategy_df$Type == hand_type,]
  }
  
  if (nrow(recommended_action_row) > 0) {
    return(recommended_action_row$Action[1])  # Return the first matching action
  } else {
    return("H")  # Default to "Hit" if no matching rule is found
  }
}

# Player's turn
player_turn <- function(deck, player_hand, dealer_card, basic_strategy_df, can_double_down, max_splits, splits_done = 0, is_split_hand = FALSE) {
  action <- determine_action(player_hand, dealer_card, basic_strategy_df, is_split_hand)
  
  while (action != "S") {
    if (action == "H") {
      result <- hit_hand(deck, player_hand)  # Use hit_hand for hitting
      player_hand <- result$hand
      deck <- result$deck
    } else if (action == "D" && can_double_down && nrow(player_hand) == 2) {
      # Perform double down if allowed and it's the first two cards
      result <- double_down(deck, player_hand, can_double_down, is_split_hand)
      player_hand <- result$hand
      deck <- result$deck
      break  # End the player's turn after doubling down
    } else if ((action == "Dh" && !can_double_down) || action == "Dh" && nrow(player_hand) != 2) {
      # Hit if doubling down isn't possible due to rules or hand size
      result <- hit_hand(deck, player_hand)
      player_hand <- result$hand
      deck <- result$deck
    } else if (action == "Ds" && !can_double_down || action == "Ds" && nrow(player_hand) != 2) {
      # Stand if you can't double down due to rules or hand size
      break
    } else if (action == "SP") {
      # Assuming split_hand function correctly handles splitting logic
      result <- split_hand(deck, player_hand, max_splits, splits_done, basic_strategy_df, can_double_down)
      player_hand <- result$hand
      deck <- result$deck
      break  # Typically, the player's turn ends after splitting
    }
    
    # Recalculate the action based on the updated hand, unless it was a final action like "Ds"
    if (!(action == "Ds" && !can_double_down || action == "Ds" && nrow(player_hand) != 2)) {
      action <- determine_action(player_hand, dealer_card, basic_strategy_df, is_split_hand)
    } else {
      break  # Exit the loop if action is "Ds" and doubling down isn't an option
    }
  }
  
  return(list("hand" = player_hand, "deck" = deck))
}

# Logic for hit
hit_hand <- function(deck, player_hand){
  result <- deal_card(deck)
  player_hand <- rbind(player_hand, result$card)
  deck <- result$deck
  
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
  results <- vector("list", num_games)  # Initialize a list to store results of each game
  
  for (i in 1:num_games) {  # Loop over the number of games to simulate
    deck <- shuffle_deck(initialize_deck(num_decks))  # Initialize and shuffle the deck(s)
    count <- list("running" = 0, "true" = 0)  # Initialize card count
    
    while (TRUE) {  # Main loop for a single game
      if (check_reshuffle(deck, num_decks, reshuffle_threshold)) {  # Check if reshuffling is needed
        deck <- shuffle_deck(initialize_deck(num_decks))  # Reshuffle the decks
        count <- list("running" = 0, "true" = 0)  # Reset count after reshuffling
      }
      
      bet_size <- calculate_bet_size(count$true, min_bet, max_bet, bet_spread)  # Determine bet size based on true count
      
      # Initial dealing: Deal two cards each to the player and the dealer
      player_hand <- data.frame()
      dealer_hand <- data.frame()
      for (j in 1:2) {
        deal_result <- deal_card(deck)
        player_hand <- rbind(player_hand, deal_result$card)
        deck <- deal_result$deck
        
        deal_result <- deal_card(deck)
        dealer_hand <- rbind(dealer_hand, deal_result$card)
        deck <- deal_result$deck
      }
      
      # Player's turn: Make decisions based on the basic strategy and card counting
      player_result <- player_turn(deck, player_hand, dealer_hand[1,], basic_strategy, can_double_down, max_splits)
      player_hand <- player_result$hand  # Update player's hand after their turn
      deck <- player_result$deck  # Update the deck after player's turn
      
      # Dealer's turn: Follow standard rules for the dealer's play
      dealer_result <- dealer_turn(deck, dealer_hand)
      dealer_hand <- dealer_result$hand  # Update dealer's hand after their turn
      deck <- dealer_result$deck  # Update the deck after dealer's turn
      
      # Determine and record the outcome of the game
      outcome <- determine_winner(player_hand, dealer_hand)
      results[[i]] <- list("outcome" = outcome, "player_hand" = player_hand, "dealer_hand" = dealer_hand, "bet_size" = bet_size)
      
      # Logic to decide whether to continue the loop for a new game or break
    }
  }
  
  return(results)  # Return the results of all simulated games
}
