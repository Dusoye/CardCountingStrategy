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
  geom_step() + theme_minimal() +
  ggtitle(paste0('Bet Strategy - P1: ', count_system, ' P2: Flat'))


multistrat <- calculate_winners_strategies(results, 1)

multistrat %>%
  group_by(system) %>%
  mutate(profit = cumsum(hand_result)) %>%
  ggplot(aes(x = gameidx, y = profit, colour = system)) +
  geom_step() +
  theme_minimal() +
  ggtitle('Different count strategies')
