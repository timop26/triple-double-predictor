source("~/triple-double-predictor/R/pbp_data_prep.R")
source("~/triple-double-predictor/R/add_features.R")


# Generating base data
for (i in 2025:2022) {
  start <- Sys.time()
  in_game_player_stats <- create_in_game_player_stats(season=i)
  write.csv(in_game_player_stats, file=paste0("~/triple-double-predictor/data/in_game_player_stats_", i, ".csv"), row.names=FALSE)
  end <- Sys.time()
  print(paste("Took", round(difftime(end, start, units="mins")), "minutes"))
}

# Adding game and score margin features
for (i in 2025:2022) {
  in_game_player_stats <- read.csv(paste0("triple-double-predictor/data/in_game_player_stats_", i, ".csv"))
  in_game_player_stats <- join_game_and_score(in_game_player_stats, season=i)
  in_game_player_stats <- in_game_player_stats %>%
    filter(game_play_number %% 4 == 1, tri_dbl == 0)
  write.csv(in_game_player_stats, file=paste0("~/triple-double-predictor/data/modeling_data_", i, ".csv"), row.names=FALSE)
}


