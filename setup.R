library(tidyverse)

# Read in the data
tournaments = read_csv("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/tournaments1877-2017_unindexed.csv")
matches = read_csv("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/match_scores_1991-2016_unindexed.csv")
stats = read_csv("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/match_stats1991-2016_unindexed.csv")
players = read_csv("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/player_overviews_unindexed.csv")

# Filter columns, rows
tournaments = tournaments |> 
  select(tourney_year_id, tourney_year, tourney_name,
         singles_winner_name, singles_winner_player_id) |> 
  filter(tourney_year >= 1991, tourney_year <= 2016)
matches = matches |> 
  select(match_id, tourney_year_id, tourney_round_name,
         winner_name, winner_player_id, winner_seed,
         loser_name, loser_player_id, loser_seed,
         match_score_tiebreaks:loser_tiebreaks_won)
stats = stats |> 
  select(-c(tourney_order, match_statsurl_suffix, match_time))
players = players |> 
  select(-c(player_slug, player_url, birthdate, birth_month, birth_day,
            weight_kg, height_ft, height_cm))

# Separate stats by winner / loser
winners = matches |> 
  inner_join(stats, by="match_id") |> 
  mutate(opponent_name = loser_name, opponent_player_id = loser_player_id) |> 
  select(match_id, match_duration,
         starts_with("opponent"), starts_with("winner")) |> 
  rename_with(str_replace, -match_id, "winner_", "") |> 
  mutate(result = "win", .after = player_id)
losers = matches |> 
  inner_join(stats, by="match_id") |> 
  mutate(opponent_name = winner_name, opponent_player_id = winner_player_id) |> 
  select(match_id, match_duration,
         starts_with("opponent"), starts_with("loser")) |> 
  rename_with(str_replace, -match_id, "loser_", "") |> 
  mutate(result = "loss", .after = player_id)

# Combine tables together
combined = winners |> 
  bind_rows(losers) |> 
  group_by(match_id) |> 
  arrange(match_id, desc(result), by.group=TRUE)
all_matches = matches |> 
  select(match_id:tourney_round_name, match_score_tiebreaks) |> 
  left_join(combined, by="match_id") |> 
  left_join(tournaments, by="tourney_year_id") |> 
  relocate(tourney_year:singles_winner_player_id,
           .before=tourney_round_name) |> 
  left_join(players, by="player_id") |> 
  mutate(player_name = name) |> 
  select(-c(name, first_name, last_name)) |> 
  relocate(player_name, player_id, .before=opponent_name)

# Abridged
tennis = all_matches |> 
  group_by(player_id, tourney_year, tourney_name) |> 
  mutate(
    avg_match_minutes = ceiling(mean(match_duration, na.rm=TRUE)),
    avg_match_aces = mean(aces, na.rm=TRUE),
    avg_match_double_faults = mean(double_faults, na.rm=TRUE),
    avg_match_first_serves_in = mean(first_serves_in, na.rm=TRUE),
    avg_match_service_wins = mean(service_points_won, na.rm=TRUE),
    avg_match_return_wins = mean(return_points_won, na.rm=TRUE),
    avg_match_points_won = mean(total_points_won, na.rm=TRUE),
    avg_match_points_played = mean(total_points_total, na.rm=TRUE),
    tournament_wins = sum(result == "win", na.rm=TRUE)
  ) |> 
  ungroup() |> 
  group_by(player_id, tourney_year) |> 
  mutate(avg_tournament_wins = mean(tournament_wins, na.rm=TRUE)) |> 
  ungroup() |> 
  distinct()

View(tennis)

# Save data
write_csv(x = tournaments, file = "data/tournaments.csv")
write_csv(x = matches, file = "data/matches.csv")
write_csv(x = stats, file = "data/stats.csv")
write_csv(x = players, file = "data/players.csv")
write_csv(x = tennis, file = "data/tennis.csv")

