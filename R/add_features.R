library(hoopR)
library(tidyverse)
library(lubridate)

create_score_margins <- function(season=most_recent_nba_season()) {
  # Load play-by-play data
  pbp <- load_nba_pbp(season=season) %>%
    select(game_id, game_play_number, home_team_id, away_team_id, period_number, away_score, home_score)
  
  # Calculate score margin for home and away team
  score_margins <- bind_rows(
    pbp %>%
      rename(team_id=home_team_id) %>%
      mutate(score_margin=home_score - away_score) %>%
      select(game_id, game_play_number, team_id, score_margin),
    pbp %>%
      rename(team_id=away_team_id) %>%
      mutate(score_margin=away_score - home_score) %>%
      select(game_id, game_play_number, team_id, score_margin)
  )
  return(score_margins)
}

get_game_features <- function(season=most_recent_nba_season()) {
  # Getting month, year, broadcast, venue, and game type for each game
  game_df <- load_nba_schedule(season=season) %>%
    mutate(month=month(start_date), year=year(start_date), neutral_site=neutral_site * 1) %>%
    select(game_id, month, year, broadcast_market, venue_full_name, neutral_site, type_abbreviation)
  return(game_df)
}

get_players_team <- function(season=most_recent_nba_season()) {
  # Getting the game ID, player ID, team ID and home/away status for each game
  game_logs <- load_nba_player_box(season=season) %>%
    rename(player_id=athlete_id) %>%
    select(game_id, player_id, team_id, home_away)  
}

join_game_and_score <- function(in_game_player_stats, season=most_recent_nba_season()) {
  in_game_player_stats <- in_game_player_stats %>%
    left_join(get_players_team(season=season), by=c("game_id", "player_id")) %>%
    left_join(get_game_features(season=season), by="game_id") %>%
    left_join(create_score_margins(season=season), by=c("game_id", "game_play_number", "team_id"))
  return(in_game_player_stats)
}



