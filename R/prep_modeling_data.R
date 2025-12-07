source("~/triple-double-predictor/R/pbp_data_prep.R")
source("~/triple-double-predictor/R/add_features.R")

player_df <- read.csv("~/triple-double-predictor/data/player_data.csv")
team_df <- read.csv("~/triple-double-predictor/data/team_data.csv")

generate_base_data <- function(season) {
  # Generate in-game player stats
  in_game_player_stats <- create_in_game_player_stats(season=season)
  # Write player stats to csv
  write.csv(
    in_game_player_stats, 
    file=paste0("~/triple-double-predictor/data/in_game_player_stats_", season, ".csv"), 
    row.names=FALSE
  )
}

create_modeling_data <- function(season) {
  filepath <- paste0("~/triple-double-predictor/data/in_game_player_stats_", season, ".csv")
  in_game_player_stats <- read.csv(filepath)
  # Joining game and score features
  in_game_player_stats <- join_game_and_score(in_game_player_stats, season=season)
  # Keeping every 6th play
  in_game_player_stats <- in_game_player_stats %>%
    filter(
      game_play_number %% 6 == 1, 
      tri_dbl == 0, 
      start_game_seconds_remaining >= 30, 
      type_abbreviation == "STD"
    ) %>%
    mutate(
      season=season, 
      prior_season=(season - 1),
      pts_needed=ifelse(pts >= 10, 0, 10 - pts),
      treb_needed=ifelse(treb >= 10, 0, 10 - treb),
      ast_needed=ifelse(ast >= 10, 0, 10 - ast),
      stl_needed=ifelse(stl >= 10, 0, 10 - stl),
      blk_needed=ifelse(blk >= 10, 0, 10 - blk),
      minutes_remaining=start_game_seconds_remaining / 60
    )
  # Adding player info
  in_game_player_stats <- merge(
    in_game_player_stats, 
    select(player_df, athlete_id, season, full_name, pos, weight, height, years),
    by.x=c("player_id", "season"), 
    by.y=c("athlete_id", "season")
  )
  
  # Adding prior season stats
  in_game_player_stats <- merge(
    in_game_player_stats, 
    select(
      player_df,
      athlete_id, 
      season, 
      gp_prior_season=gp, 
      ppg_prior_season=ppg, 
      apg_prior_season=apg, 
      rpg_prior_season=rpg, 
      spg_prior_season=spg, 
      bpg_prior_season=bpg, 
      tdbl_prior_season=tdbl, 
      min_prior_season=min
    ),
    by.x=c("player_id", "prior_season"), 
    by.y=c("athlete_id", "season"),
    all.x=TRUE
  )
  
  # Adding team data season
  in_game_player_stats <- merge(
    in_game_player_stats, 
    select(team_df, team_id, season, team_abbreviation, team_color),
    by=c("team_id", "season"),
    all.x=TRUE
  )
  
  # Keeping only most relevant columns
  in_game_player_stats <- in_game_player_stats %>%
    select(
      game_id,
      game_play_number,
      season,
      player_id,
      full_name,
      team_id,
      team_abbreviation,
      team_color,
      start_game_seconds_remaining,
      minutes_remaining,
      pts,
      ast,
      treb,
      stl,
      blk,
      twoa,
      twom,
      threea,
      threem,
      fta,
      ftm,
      to,
      fls,
      minutes,
      home_away,
      year,
      month,
      venue_full_name,
      type_abbreviation,
      score_margin,
      pos,
      weight,
      height,
      years,
      gp_prior_season,
      ppg_prior_season,
      apg_prior_season,
      rpg_prior_season,
      spg_prior_season,
      bpg_prior_season,
      tdbl_prior_season,
      min_prior_season,
      triple_double
    ) %>%
    arrange(game_id, player_id, game_play_number)
  
  write.csv(
    in_game_player_stats, 
    file=paste0("~/triple-double-predictor/data/modeling_data_", season, ".csv"), 
    row.names=FALSE
  )
}

