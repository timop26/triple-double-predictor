library(hoopR)
library(purrr)
library(tidyverse)

# Function to calculate mode
stat_mode <- function(x) {
  uniq <- unique(x)
  return(uniq[which.max(tabulate(match(x, uniq)))])
}

# Function to pull relevant stats for a given player and season
player_season <- function(player_id, season=most_recent_nba_season()) {
  espn_nba_player_stats(player_id, season) %>%
    select(
      athlete_id,
      pos=position_abbreviation,
      headshot_href,
      weight,
      height,
      age,
      years,
      gp=general_games_played,
      ppg=offensive_avg_points,
      apg=offensive_avg_assists,
      rpg=general_avg_rebounds,
      spg=defensive_avg_steals,
      bpg=defensive_avg_blocks,
      pp48=offensive_avg48points,
      ap48=offensive_avg48assists,
      rp48=general_avg48rebounds,
      sp48=defensive_avg48steals,
      bp48=defensive_avg48blocks,
      tdbl=general_triple_double,
      ddbl=general_double_double,
      min=general_avg_minutes
    ) %>%
    mutate(season=season)
}

# Safe function for grabbing player stats
safe_player_season <- possibly(player_season, otherwise=NULL)

# Function to grab all players with minutes in a given season
load_active_players <- function(seasons=most_recent_nba_season()) {
  player_df <- load_nba_player_box(season=seasons) %>%
    filter(!is.na(minutes)) %>%
    group_by(athlete_id, season) %>%
    summarize(full_name=stat_mode(athlete_display_name), shortened_name=stat_mode(athlete_short_name))
  return(player_df)
}

# Pulling player stats for all players with minutes and stacking them into one data frame
player_stats <- function(seasons=most_recent_nba_season()) {
  player_df <- load_active_players(season=seasons)
  player_stats <- map2_dfr(player_df$athlete_id, player_df$season, safe_player_season)
  # Merging all players with minutes and their retrieved stats. Also correcting year and age calculation
  player_df <- merge(as.data.frame(player_df), player_stats, by=c("athlete_id", "season"), all.x=TRUE) %>%
    group_by(athlete_id) %>%
    mutate(years=years - (max(season) - season), age=age - (most_recent_nba_season() - season))
  # Keeping only players with known stats
  player_df <- filter(player_df, !is.na(gp)) %>%
    mutate(years=ifelse(years >= 0, years, 0))
}

update_player_data <- function(seasons=most_recent_nba_season(), method="append") {
  player_data <- player_stats(seasons=seasons)
  if (method == "append") {
    old_data <- read.csv("data/player_data.csv")
    player_data <- bind_rows(old_data, player_data) %>%
      arrange(player_id, season)
  }
  write.csv(player_data, "data/player_data.csv", row.names=FALSE)
}

update_player_data()