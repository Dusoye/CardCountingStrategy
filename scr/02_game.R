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
  deck <- deck[sample(nrow(deck)),]
  count <- data.table(system = unique(count_values$system), 'running' = 0, 'true' = 0)
  return(list("deck" = deck, "count" = count))
}

# Check whether a reshuffle is required after the current hand
check_reshuffle <- function(deck, num_decks, reshuffle_threshold) {
  num_cards_remaining <- ifelse(is.null(nrow(deck)), 0, nrow(deck))
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
  
  return(list("hand_value" = hand_value, "num_aces" = num_aces))
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
  card_value <- card$Value
  card_value[card_value == "J" | card_value == "Q" | card_value == "K"] <- 10
  
  count_change <- count_values[count_values$name == card_value, c("system","value")]
  count[count_change, on = .(system), running := running + value]
  
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
determine_action <- function(player_hand, dealer_upcard, basic_strategy_df, can_double_down, can_surrender, can_split) {
  eval <- evaluate_hand(player_hand)  # Evaluate player's hand total
  player_total <- eval$hand_value
  num_aces <- eval$num_aces
  dealer_upcard_value <- as.character(dealer_upcard$Value)  # Ensure dealer upcard is character for comparison
  
  # Handle face cards and Aces for the dealer
  if (dealer_upcard_value %in% c("J", "Q", "K")) {
    dealer_upcard_value <- "10"
  } #else if (dealer_upcard_value == "A") {
    #dealer_upcard_value <- "11"
  #}
  
  # Determine if the hand is soft, hard, or a pair
  hand_type <- ifelse(nrow(player_hand) == 2 && player_hand$Value[1] == player_hand$Value[2], "Pair",
                      ifelse(num_aces > 0 && player_total <= 21, "Soft", "Hard"))
  
  # Adjust for format of basic strategy data
  player_hand_notation <- if(hand_type == "Soft") {
    paste0("A:", player_total - 11)  # Subtract 11 for the Ace counted as 11
  } else if (hand_type == "Pair" && player_hand$Value[1] %in% c("J", "Q", "K")) {
    "10:10"
  } else if (hand_type == "Pair") {
    paste0(player_hand$Value[1], ":", player_hand$Value[2])
  } else {
    as.character(player_total)
  }
  
  # Adjust for Aces in pairs
  if(hand_type == "Pair" && player_hand$Value[1] == "A") {
    recommended_action_row <- basic_strategy_df[basic_strategy_df$Player == "A:A" & 
                                                  basic_strategy_df$Dealer == dealer_upcard_value & 
                                                  basic_strategy_df$Type == hand_type,]
  } else if(player_total >= 21) {
    recommended_action_row <- data.table('Action' = 'S')
  } else {
  # Look up the recommended action from the strategy dataframe
  recommended_action_row <- basic_strategy_df[basic_strategy_df$Player == player_hand_notation & 
                                                basic_strategy_df$Dealer == dealer_upcard_value & 
                                                basic_strategy_df$Type == hand_type,]
  }
  
  if (nrow(recommended_action_row) > 0) {
    action = recommended_action_row$Action[1]  # Return the first matching action
  } else {
    action = "H"  # Default to "Hit" if no matching rule is found
  }
  
  action = switch(action,
                  Dh = ifelse(can_double_down && nrow(player_hand) == 2, "D", "H"),  # Double if possible, else hit
                  Ds = ifelse(can_double_down && nrow(player_hand) == 2, "D", "S"),  # Double if possible, else stand
                  Uh = ifelse(can_surrender && nrow(player_hand) == 2, "U", "H"),  # Surrender if possible, else split
                  Us = ifelse(can_surrender && nrow(player_hand) == 2, "U", "S"),  # Surrender if possible, else split
                  Usp = ifelse(can_surrender && nrow(player_hand) == 2, "U", ifelse(can_split, "SP", "H")),  # Surrender if possible, else split
                  action  # Keep original action if not matching above cases
  )
  
  return(action)
}

# Player's turn
player_turn <- function(deck, player_hand, dealer_card, basic_strategy_df, can_double_down, can_split, max_splits, stand_soft_17, can_surrender, splits_done = 0) {
  # Check for Blackjack 
  if (nrow(player_hand) == 2 && evaluate_hand(player_hand)$hand_value == 21) {
    return(list("hand" = player_hand, "deck" = deck, "outcome" = "Blackjack"))
  }
  
  is_split_hand = if_else(splits_done > 0, TRUE, FALSE)
  action <- determine_action(player_hand, dealer_card, basic_strategy_df, can_double_down, can_surrender, can_split)  # Determine initial action
  
  # Proceed with the gameplay logic based on the final determined action
  while (action != "S") {
    if (action == "H") {
      result <- hit_hand(deck, player_hand)
      #print(paste0('Hit ', result$hand[nrow(result$hand)]))
      player_hand <- result$hand
      deck <- result$deck
    } else if (action == "SP" && can_split) {
      #print('Split')
      result <- split_hand(deck, player_hand, dealer_card, max_splits, splits_done, basic_strategy_df, can_double_down)
      player_hand <- result$hand
      deck <- result$deck
      outcome <- result$outcome
      return(list("hand" = player_hand, "deck" = deck, "outcome" = outcome))
    } else if (action == "D") {
      result <- double_down(deck, player_hand, can_double_after_split, is_split_hand)
      #print(paste0('Double ', result$hand[nrow(result$hand)]))
      player_hand <- result$hand
      deck <- result$deck
      return(list("hand" = player_hand, "deck" = deck, "outcome" = "Double"))
      # End player's turn after doubling down
    } else if (action == "U") {
      #print('Surrender')
      return(list("hand" = player_hand, "deck" = deck, "outcome" = "Surrender"))
      # End player's turn after surrender
    } 
    
    player_total <- evaluate_hand(player_hand)$hand_value
    #print(paste0('player total: ',player_total))
    # Re-determine action for the next round if necessary
    action <- determine_action(player_hand, dealer_card, basic_strategy_df, can_double_down, can_surrender, can_split)
    #print(paste0('final action: ', action))
  }
  return(list("hand" = player_hand, "deck" = deck, "outcome" = "Stand"))
}


# Logic for hit
hit_hand <- function(deck, player_hand){
  result <- deal_card(deck)
  player_hand <- rbind(player_hand, result$card)
  deck <- result$deck
  
  return(list("hand" = player_hand, "deck" = deck))
}
# Logic for splitting hands
split_hand <- function(deck, player_hand, dealer_card, max_splits, splits_done, basic_strategy, can_double_down, can_surrender = TRUE) {
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
  outcome <- list()
  for (split_hand in split_hands) {
      result <- player_turn(deck, split_hand, dealer_card, basic_strategy, can_double_down, can_split, max_splits, stand_soft_17, can_surrender, splits_done + 1)
      final_hands <- c(final_hands, list(result$hand))
      outcome <- c(outcome, result$outcome)
      deck <- result$deck
  }
  
  return(list("hand" = final_hands, "deck" = deck, "outcome" = outcome))
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
  
  return(list("hand" = player_hand, "deck" = deck, "outcome" = "Doubled"))
}

# Dealer's turn
dealer_turn <- function(deck, dealer_hand, stand_soft_17) {
  is_soft_17 <- function(hand_value, num_aces) {
    return(hand_value == 17 && num_aces > 0)
  }
  
  if (nrow(dealer_hand) == 2 && evaluate_hand(dealer_hand)$hand_value == 21) {
    return(list("hand" = dealer_hand, "deck" = deck, "dealer_outcome" = "Blackjack"))
  }

  #print(paste0('dealer hand: ', dealer_hand))
  while(TRUE) {
    eval <- evaluate_hand(dealer_hand)
    hand_value <- eval$hand_value
    num_aces <- eval$num_aces
    if (hand_value < 17 || (!stand_soft_17 && is_soft_17(hand_value, num_aces))) {
      # Hit if hand is below 17 or if it's a soft 17 and rule is to hit on soft 17
      result <- deal_card(deck)
      dealer_hand <- rbind(dealer_hand, result$card)
      deck <- result$deck
    } else {
      # Stand if hand is 17 or above, or if it's a soft 17 and rule is to stand
      break
    }
  }

  return(list("hand" = dealer_hand, "deck" = deck, "dealer_outcome" = "Stand"))
}


# Main game simulation
simulate_blackjack <- function(num_games, num_decks, basic_strategy, count_values, reshuffle_threshold, can_split, can_double_down, max_splits, num_players, stand_soft_17) {
  
  results <- vector("list", num_games)  # Initialize a list to store results of each game
  deck <- shuffle_deck(initialize_deck(num_decks))  # Initialize and shuffle the deck(s)
  count <- deck$count # Reset card counts
  deck <- deck$deck
  
  for (game_idx in 1:num_games) {  # Loop over the number of games to simulate
    if (check_reshuffle(deck, num_decks, reshuffle_threshold)) {
      deck <- shuffle_deck(initialize_deck(num_decks))
      count <- deck$count
      deck <- deck$deck
    } else {
      deck <- deck
    }
    
    # Initialize hands for multiple players
    players_hands <- vector("list", num_players)
    bet_sizes <- numeric(num_players)  # Track bet sizes for each player
    
    # Initial dealing for each player
    for (player_idx in 1:num_players) {
      player_hand <- data.frame()
      for (j in 1:2) {
        deal_result <- deal_card(deck, count, count_values)
        player_hand <- rbind(player_hand, deal_result$card)
        deck <- deal_result$deck
        
      }
      players_hands[[player_idx]] <- player_hand
      #bet_sizes[player_idx] <- calculate_bet_size(count$true, min_bet, max_bet, bet_spread)  # Determine bet size based on true count
    }
    # Initial dealing for the dealer
    dealer_hand <- data.frame()
    for (j in 1:2) {
      deal_result <- deal_card(deck, count, count_values)
      dealer_hand <- rbind(dealer_hand, deal_result$card)
      deck <- deal_result$deck
    }
    # Player turns
    players_outcome <- vector("list", num_players)
    players_value <- vector("list", num_players)
    for (player_idx in 1:num_players) {
      player_result <-player_turn(deck, players_hands[[player_idx]], dealer_hand[1,], basic_strategy, can_double_down, can_split, max_splits, stand_soft_17, can_surrender)
      players_hands[[player_idx]] <- player_result$hand
      
      # Work out value of players hands adjusting for splits
      if(length(players_hands[[player_idx]]) == 1){
        players_value[[player_idx]] <- evaluate_hand(players_hands[[player_idx]])$hand_value
      } else {
        tmp <- vector("list", 2)
        for(hands in 1:length(players_hands[[player_idx]])){
          tmp[[hands]] <- evaluate_hand(players_hands[[player_idx]][[hands]])$hand_value
        }
        players_value[[player_idx]] <- tmp
      }
      
      players_outcome[[player_idx]] <- player_result$outcome
      deck <- player_result$deck
      # Optionally, update count here if necessary
      #player_outcomes[[player_idx]] <- determine_winner(players_hands[[player_idx]], dealer_hand)  # Determine outcome for each player
    }
    
    # Dealer's turn
    dealer_result <- dealer_turn(deck, dealer_hand, stand_soft_17)
    dealer_hand <- dealer_result$hand
    dealer_value <- evaluate_hand(dealer_hand)$hand_value
    deck <- dealer_result$deck
    dealer_outcome <- dealer_result$dealer_outcome
    
    # Compile results for the game
    results[[game_idx]] <- list(player = players_hands, 
                                player_value = players_value,
                                outcome = players_outcome, 
                                dealer = dealer_hand,
                                dealer_value = dealer_value,
                                dealer_outcome = dealer_outcome)
  }
  
  return(results)  # Return the results of all simulated games
}

calculate_winners <- function(game_result) {
  dealer_value <- game_result$dealer_value
  dealer_outcome <- game_result$dealer_outcome
  
  player_results <- lapply(seq_along(game_result$player_value), function(player_idx) {
    player_hands <- game_result$player_value[[player_idx]]
    hand_outcomes <- game_result$outcome[[player_idx]]
    
    # Initialize sum for the player in this round
    player_sum <- 0
    
    # Handle both single and split hands
    if (is.list(player_hands)) {  # Player has split hands
      hand_results <- sapply(seq_along(player_hands), function(hand_idx) {
        player_value <- player_hands[[hand_idx]]
        outcome <- hand_outcomes[[hand_idx]]
        
        # Calculate numerical result for each hand and sum them
        calculate_hand_result(player_value, outcome, dealer_value, dealer_outcome)
      })
      
      # Sum results for split hands
      player_sum <- sum(hand_results)
    } else {  # Single hand
      player_sum <- calculate_hand_result(player_hands, hand_outcomes, dealer_value, dealer_outcome)
    }
    
    return(player_sum)
  })
  
  return(player_results)
}

calculate_hand_result <- function(player_value, outcome, dealer_value, dealer_outcome) {
  # Blackjack push
  if (outcome == "Blackjack" & dealer_outcome == "Blackjack") {
    return(0)
  } 
  # Blackjack win
  else if (outcome == "Blackjack") {
    return(1.5)
  } 
  # Surrender
  else if (outcome == "Surrender") {
    return(-0.5)
  }
  
  # Player busts or dealer wins
  if (player_value > 21) {
    return(ifelse(outcome == "Double", -2, -1))
  } 
  # Player wins
  else if (dealer_value > 21 || player_value > dealer_value) {
    return(ifelse(outcome == "Double", 2, 1))
  } 
  # Push
  else if (player_value == dealer_value) {
    return(0)
  } 
  # Dealer wins
  else {
    return(ifelse(outcome == "Double", -2, -1))
  }
}

calculate_running_sum <- function(results, num_players) {
  # Initialize a vector to store the running sum for each player
  running_sums <- numeric(num_players)
  
  # Iterate through each game's results
  for (game in results) {
    # Iterate through each player in the game
    for (player_idx in seq_along(game)) {
      player_result <- game[[player_idx]]
      
      # Check if the player has multiple hands (result of split)
      if (is.list(player_result)) {
        # Sum the results for split hands and add to the running sum for the player
        running_sums[player_idx] <- running_sums[player_idx] + sum(unlist(player_result))
      } else {
        # Add the result to the running sum for the player
        running_sums[player_idx] <- running_sums[player_idx] + player_result
      }
    }
  }
  
  return(running_sums)
}

