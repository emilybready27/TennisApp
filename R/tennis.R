# Sorts the unique input elements
clean = function(input) {
  return (sort(unique(input)))
}

# Checks if choice is in choices
# If not, returns arbitrary choice
choose = function(choices, choice) {
  if (choice %in% choices) {
    return (choice)
  } else {
    return (choices[1])
  }
}

# Computes points for plotting
compute_points = function(data, player) {
  all_players = data |> 
    group_by(player_id) |>
    summarize(
      player_name = player_name,
      avg_match_minutes = ceiling(mean(avg_match_minutes, na.rm=TRUE)),
      avg_match_aces = mean(avg_match_aces, na.rm=TRUE),
      avg_match_double_faults = mean(avg_match_double_faults, na.rm=TRUE),
      avg_match_first_serves_in = mean(avg_match_first_serves_in, na.rm=TRUE),
      avg_match_service_wins = mean(avg_match_service_wins, na.rm=TRUE),
      avg_match_return_wins = mean(avg_match_return_wins, na.rm=TRUE),
      avg_match_points_won = mean(avg_match_points_won, na.rm=TRUE),
      avg_match_points_played = mean(avg_match_points_played, na.rm=TRUE),
      avg_tournament_wins = mean(avg_tournament_wins, na.rm=TRUE)
    ) |> 
    distinct() |> 
    ungroup() |> 
    arrange(player_name)
  
  avg_player = all_players |>
    summarize(across(avg_match_minutes:avg_tournament_wins,
                     mean, na.rm=TRUE)) |>
    pivot_longer(
      cols = avg_match_minutes:avg_tournament_wins,
      names_to = "Stat",
      values_to = "Average"
    )

  my_player = all_players |>
    filter(player_name == player) |> 
    select(-c(player_name, player_id)) |>
    pivot_longer(
      cols = avg_match_minutes:avg_tournament_wins,
      names_to = "Stat",
      values_to = "Player"
    )

  points = my_player |>
    inner_join(avg_player, by="Stat") |>
    pivot_longer(
      cols = Player:Average,
      names_to = "Type",
      values_to = "Count"
    )

  return (points)
}


# Testing
# tennis2 = read_csv(file = "data/tennis.csv")
# comp = tennis2 |> 
  # filter(tourney_year == 2010) |> 
  # filter(tourney_name == "Roland Garros") |>
  # compute_points("Rafael Nadal")
# View(comp)
# comp |>
#   ggplot() +
#   aes(x=Stat, y=Value, fill=Type) +
#   geom_bar(stat="identity", position="identity", color = "black", alpha=0.6) +
#   coord_flip() +
#   theme_bw() +
#   scale_fill_manual(values=c("#33CCFF", "#FFFF33", "#000000"))
