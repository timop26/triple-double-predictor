library(hoopR)
library(stringr)
library(tidyverse)

create_player_dim <- function(season=most_recent_nba_season()) {
  box_scores <- hoopR::load_nba_player_box(season)
  box_scores$athlete_display_name[box_scores$athlete_display_name == "Brandon Boston"] <- "Brandon Boston|Brandon Boston Jr."
  box_scores$athlete_display_name[box_scores$athlete_display_name == "Jimmy Butler III"] <- "Jimmy Butler III|Jimmy Butler"
  box_scores$athlete_display_name[box_scores$athlete_display_name == "Bub Carrington"] <- "Bub Carrington|Carlton Carrington"
  box_scores$athlete_display_name[box_scores$athlete_display_name == "Alex Sarr"] <- "Alex Sarr|Alexandre Sarr"
  return(unique(box_scores[, c("athlete_id", "athlete_display_name", "season")]))
}

# Function to update the lineup after each play
update_lineup <- function(current_lineup, play) {
  if (play$type_text == "Substitution") {
    replace(current_lineup, current_lineup == play$athlete_id_2, play$athlete_id_1)
  } else {
    current_lineup
  }
}

calc_lineups <- function(season=most_recent_nba_season()) {
  # Get play by play data
  pbp <- hoopR::load_nba_pbp(season=season) %>%
    mutate(seconds_elapsed=start_game_seconds_remaining - end_game_seconds_remaining) %>%
    select(game_id, game_play_number, start_game_seconds_remaining, seconds_elapsed, type_text, athlete_id_1, athlete_id_2)
  
  # Get initial lineups
  lineups <- hoopR::load_nba_player_box(season=season) %>%
    filter(starter) %>%
    group_by(game_id) %>%
    summarise(lineup=list(athlete_id), .groups="drop")
  
  pbp <- left_join(pbp, lineups, by="game_id") %>%
    arrange(game_id, game_play_number)
  
  # Apply accumulate by game_id
  pbp <- pbp %>%
    group_by(game_id) %>%
    mutate(lineup=accumulate(
      .x=row_number(),
      .init=lineup[[1]],
      .f=function(lineup_prev, idx) update_lineup(lineup_prev, slice(pick(everything()), idx))
      )[-1]
    ) %>%
    ungroup()
  
  # Fix seconds_elapsed for first play
  pbp <- pbp %>%
    group_by(game_id) %>%
    mutate(
      seconds_elapsed=if_else(
        game_play_number == 1,
        start_game_seconds_remaining - lead(start_game_seconds_remaining, default = 0),
        seconds_elapsed
      )
    ) %>%
    ungroup()
  
  return(pbp)
}

# Calculates the number of seconds each player played based on lineup data
calc_seconds_played <- function(lineups=NULL, season=most_recent_nba_season()) {
  # Load lineups if not provided
  if (is.null(lineups)) {
    lineups <- calc_lineups(season=season)
  }
  
  # Unnest and reshape lineup data to get player-level seconds played
  seconds_played <- lineups %>%
    select(game_id, game_play_number, start_game_seconds_remaining, seconds_elapsed, lineup) %>%
    unnest_longer(lineup) %>%
    rename(player_id=lineup, seconds=seconds_elapsed)
  
  # Remove duplicate rows for the same player in a lineup segment
  dup_player_segment <- duplicated(seconds_played)
  print(paste("Dropping", sum(dup_player_segment), "duplicate players in a given lineup"))
  
  return(seconds_played[!dup_player_segment, ])
}

# Adds player names to play-by-play data using player dimension table
add_player_names <- function(season=most_recent_nba_season()) {
  # Get mapping of athlete ID to name
  player_dim <- select(create_player_dim(season=season), athlete_id, athlete_display_name)
  
  # Load play-by-play and join with player names involved in the play
  pbp <- hoopR::load_nba_pbp(season=season) %>%
    left_join(player_dim, by=join_by(athlete_id_1 == athlete_id)) %>%
    rename(athlete_name_1=athlete_display_name) %>%
    left_join(player_dim, by=join_by(athlete_id_2 == athlete_id)) %>%
    rename(athlete_name_2=athlete_display_name) %>%
    left_join(player_dim, by=join_by(athlete_id_3 == athlete_id)) %>%
    rename(athlete_name_3=athlete_display_name)
  
  return(pbp)
}

# Prepares a simplified play-by-play table for calculating cumulative stats
prep_pbp_for_cum_stats <- function(season=most_recent_nba_season()) {
  pbp <- add_player_names(season=season) %>%
    # Keep only plays with one or two players involved
    filter(is.na(athlete_name_3), !is.na(athlete_name_1)) %>%
    # Select only the relevant columns for stat calculations
    select(
      game_id, 
      game_play_number,
      start_game_seconds_remaining,
      athlete_id_1,
      athlete_id_2,
      athlete_name_1, 
      athlete_name_2, 
      text, 
      type_text, 
      shooting_play, 
      scoring_play
    )
  
  return(pbp)
}

player_stats <- function(
    game_id, 
    game_play_number, 
    start_game_seconds_remaining,
    end_game_seconds_remaining,
    player, 
    text, 
    type_text, 
    shooting_play, 
    scoring_play,
    player_num,
    player_id
) {
  box <- data.frame(
    game_id=game_id,
    game_play_number=game_play_number,
    start_game_seconds_remaining,
    player_id=player_id,
    player_name=player,
    twoa=0,
    twom=0,
    threea=0,
    threem=0,
    fta=0,
    ftm=0,
    oreb=0,
    dreb=0,
    ast=0,
    stl=0,
    blk=0,
    to=0,
    fls=0,
    tech=0,
    flagrant=0
  )
  if (player_num == 1) {
    # If it's a shooting play, need to determine free throw, 2 pointer, or 3 pointer
    if (shooting_play) {
      distance <- as.numeric(str_extract(text, "\\d+"))
      type <- case_when(
        str_detect(text, "free") ~ "ft", 
        str_detect(text, "three|(?=.*misses)(?=.*running pullup jump shot)") | (!is.na(distance) & distance >= 24) ~ "three", 
        TRUE ~ "two"
      )
      # Increment attempts
      box[, paste0(type, "a")] <- 1
      if (scoring_play) {
        box[, paste0(type, "m")] <- 1
      }
    } else if (type_text == "Offensive Rebound") {
      box$oreb <- 1
    } else if (type_text == "Defensive Rebound") {
      box$dreb <- 1
    } else if (str_detect(type_text, "Turnover|Traveling|Charge")) {
      box$to <- 1
    } else if (type_text == "Flagrant Foul Type 1") {
      box$flagrant <- 1
    } else if (type_text == "Flagrant Foul Type 2") {
      box$flagrant <- 2
    } else if (str_detect(type_text, "Technical Foul")) {
      box$tech <- 1
    } else if (str_detect(type_text, "Foul") & !str_detect(type_text, "Technical|Flagrant")) {
      box$fls <- 1
    }
  } else {
    action <- str_extract(text, paste0("(?<=", player, " )\\S+"))
    if (!is.na(action)) {
      if (action == "assists)") {
        box$ast <- 1
      } else if (action == "blocks") {
        box$blk <- 1
      } else if (action == "steals)") {
        box$stl <- 1
      }
    } else if (type_text == "Double Technical Foul") {
      box$tech <- 1
    }
  }
  return(box)
}

calc_play_stats <- function(season=most_recent_nba_season()) {
  # Prepare play-by-play data for the given season
  pbp <- prep_pbp_for_cum_stats(season=season)
  
  # Extract stats for player 1 and player 2 in each play and combine them
  play_stats <- bind_rows(
    mutate(rename(pbp, player=athlete_name_1, player_id=athlete_id_1), player_num=1) %>%
      select(-c(athlete_id_2, athlete_name_2)) %>%
      pmap_dfr(player_stats),
    mutate(rename(pbp, player=athlete_name_2, player_id=athlete_id_2), player_num=2) %>%
      filter(!is.na(player)) %>%
      select(-c(athlete_id_1, athlete_name_1)) %>%
      pmap_dfr(player_stats)
  ) %>%
    arrange(game_id, game_play_number)
  return(play_stats)
}

# Add time played (in seconds) to play-level stats and fill missing values with 0
play_stats_add_time_played <- function(play_stats, seconds_played) {
  # List of stat columns that should default to 0 if missing
  replace_zeroes <- c("twoa", "twom", "threea", "threem", "fta", "ftm", "oreb", "dreb", "ast", "stl", "blk", "to", "fls", "tech", "flagrant", "seconds")
  
  # Join seconds played data and replace missing stat values with 0
  play_stats <- seconds_played %>%
    left_join(
      select(play_stats, -start_game_seconds_remaining, -player_name), 
      join_by(game_id==game_id, game_play_number==game_play_number, player_id==player_id)
    ) %>%
    mutate(across(all_of(replace_zeroes), ~ replace_na(., 0)))
  return(play_stats)
}

# Compute box score (cumulative) stats for each player per game
current_box_score <- function(play_stats) {
  stats <- c("seconds", "twoa", "twom", "threea", "threem", "fta", "ftm", "oreb", "dreb", "ast", "stl", "blk", "to", "fls", "tech", "flagrant")
  
  box_score <- play_stats %>%
    group_by(game_id, player_id) %>%
    arrange(game_play_number, .by_group=TRUE) %>%
    # Compute running totals for stats
    mutate(across(all_of(stats), cumsum)) %>% 
    mutate(
      pts=twom*2 + threem*3 + ftm,
      treb=oreb + dreb,
      minutes=seconds / 60,
      ejected=case_when(
        fls > 5 | tech > 1 | flagrant > 1 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    # Double-double and triple-double logic
    mutate(
      dbl_dbl=(rowSums(across(all_of(c("pts", "ast", "treb", "stl", "blk")), ~ .x >= 10)) >= 2)*1,
      tri_dbl=(rowSums(across(all_of(c("pts", "ast", "treb", "stl", "blk")), ~ .x >= 10)) >= 3)*1 
    )
  return(box_score)
}

# Add time played (in seconds) to play-level stats and fill missing stat values with 0
play_stats_add_time_played <- function(play_stats, seconds_played) {
  # List of stat columns that should default to 0 if missing
  replace_zeroes <- c("twoa", "twom", "threea", "threem", "fta", "ftm", "oreb", "dreb", "ast", 
                      "stl", "blk", "to", "fls", "tech", "flagrant", "seconds")
  
  # Join seconds played data with play stats and replace NA values in stat columns with 0
  play_stats <- seconds_played %>%
    left_join(
      select(play_stats, -start_game_seconds_remaining, -player_name), 
      join_by(game_id==game_id, game_play_number==game_play_number, player_id==player_id)
    ) %>%
    mutate(across(all_of(replace_zeroes), ~ replace_na(., 0)))
  
  return(play_stats)
}

# Calculate cumulative box score stats for each player in each game
current_box_score <- function(play_stats) {
  # Stat columns to accumulate
  stats <- c("seconds", "twoa", "twom", "threea", "threem", "fta", "ftm", "oreb", "dreb", "ast", 
             "stl", "blk", "to", "fls", "tech", "flagrant")
  
  box_score <- play_stats %>%
    group_by(game_id, player_id) %>%
    arrange(game_play_number, .by_group=TRUE) %>%
    # Compute cumulative stats
    mutate(across(all_of(stats), cumsum)) %>%
    mutate(
      pts=twom*2 + threem*3 + ftm,
      treb=oreb + dreb,
      minutes=seconds / 60,
      ejected=case_when(
        fls > 5 | tech > 1 | flagrant > 1 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    # Double-double and triple-double logic
    mutate(
      dbl_dbl=(rowSums(across(all_of(c("pts", "ast", "treb", "stl", "blk")), ~ .x >= 10)) >= 2)*1,
      tri_dbl=(rowSums(across(all_of(c("pts", "ast", "treb", "stl", "blk")), ~ .x >= 10)) >= 3)*1,
    )
  
  return(box_score)
}

# Fill in missing play rows for players and forward-fill cumulative stats
fill_missing_plays <- function(play_stats, season=most_recent_mbb_season()) {
  # Load full list of plays with game and time context
  pbp <- hoopR::load_nba_pbp(season=season) %>%
    select(game_id, game_play_number, start_game_seconds_remaining)
  
  # Columns to fill down across missing plays
  stats <- c("seconds", "twoa", "twom", "threea", "threem", "fta", "ftm", "oreb", "dreb", "ast", "stl", 
             "blk", "to", "fls", "tech", "flagrant", "pts", "treb", "minutes", "ejected", "dbl_dbl", "tri_dbl")
  
  # Fill in plays each player was out of the game
  play_stats <- play_stats %>%
    select(game_id, player_id) %>%
    distinct() %>%
    full_join(pbp, by="game_id") %>%
    left_join(play_stats, by=c("game_id", "game_play_number", "player_id", "start_game_seconds_remaining")) %>%
    group_by(game_id, player_id) %>%
    arrange(game_play_number) %>%
    fill(all_of(stats), .direction="down") %>%
    mutate(across(all_of(stats), ~replace_na(., 0))) %>%
    ungroup() %>%
    arrange(game_id, player_id, game_play_number)
  return(play_stats)
}

# Return the final box score (last play per player per game)
final_box_score <- function(play_stats) {
  box_score <- play_stats %>%
    group_by(game_id, player_id) %>%
    slice_max(order_by=game_play_number, n=1, with_ties=FALSE) %>%
    ungroup()
  return(box_score)
}

# Add double and triple double target flags to play-level stats
add_targets <- function(play_stats, box_calcs) {
  targets <- box_calcs %>%
    rename(double_double=dbl_dbl, triple_double=tri_dbl) %>%
    select(game_id, player_id, double_double, triple_double)
  play_stats <- play_stats %>%
    left_join(targets, by=c("game_id", "player_id"))
  return(play_stats)
}

get_official_box_score <-function(season=most_recent_nba_season()) {
  box_score <- hoopR::load_nba_player_box(season=season) %>%
    filter(!is.na(minutes)) %>%
    mutate(treb=offensive_rebounds + defensive_rebounds) %>%
    mutate(
      twoa=field_goals_attempted - three_point_field_goals_attempted,
      twom=field_goals_made - three_point_field_goals_made,
      dbl_dbl=(rowSums(across(all_of(c("points", "assists", "treb", "steals", "blocks")), ~ .x >= 10)) >= 2) * 1,
      tri_dbl=(rowSums(across(all_of(c("points", "assists", "treb", "steals", "blocks")), ~ .x >= 10)) >= 3) * 1,
    ) %>%
    rename(
      player_id=athlete_id,
      pts=points,
      threea=three_point_field_goals_attempted,
      threem=three_point_field_goals_made,
      fta=free_throws_attempted,
      ftm=free_throws_made,
      oreb=offensive_rebounds,
      dreb=defensive_rebounds,
      ast=assists,
      stl=steals,
      blk=blocks,
      to=turnovers,
      fls=fouls
    ) %>%
    select(
      game_id,
      player_id,
      minutes,
      pts,
      threea,
      threem,
      fta,
      ftm,
      oreb,
      dreb,
      ast,
      stl,
      blk,
      to,
      fls,
      ejected,
      dbl_dbl,
      tri_dbl
    ) %>%
    arrange(game_id, player_id)
  return(box_score)
}

# Join calculated and ground truth box scores for comparison
create_comp_df <- function(box_calc, box_truth) {
  return(full_join(box_calc, box_truth, by=c("game_id", "player_id"), suffix=c("_calc", "_truth")))
}

# Validate calculated box scores against truth using specified metric
val_calc_box <- function(comp_df, cols, type="exact", within=1) {
  if (type=="exact") {
    comp <- sapply(cols, function(x) {
      mean(comp_df[, paste0(x, "_truth")] == comp_df[, paste0(x, "_calc")], na.rm=TRUE)
    }) 
  } else if (type=="avg") {
    comp <- sapply(cols, function(x) {
      mean(abs(unlist(comp_df[, paste0(x, "_truth")] - comp_df[, paste0(x, "_calc")])), na.rm=TRUE)
    }) 
  } else {
    comp <- sapply(cols, function(x) {
      mean(abs(unlist(comp_df[, paste0(x, "_truth")] - comp_df[, paste0(x, "_calc")])) <= within, na.rm=TRUE)
    })
  }
  return(comp)
}

create_in_game_player_stats <- function(season=most_recent_nba_season()) {
  # Calculating play stats
  print("Calculating play stats")
  start1 <- Sys.time()
  play_stats <- calc_play_stats(season=season)
  #play_stats <- readRDS("triple-double-predictor/R/play_stats_25.rds")
  end1 <- Sys.time()
  print(paste("Calculating play stats took", round(difftime(end1, start1, units="mins")), "minutes"))
  
  # Identifying lineups
  print("Identifying lineups")
  start2 <- Sys.time()
  lineups <- calc_lineups(season=season)
  #lineups <- readRDS("triple-double-predictor/R/lineups_25.rds")
  end2 <- Sys.time()
  print(paste("Identifying lineups took", round(difftime(end2, start2, units="mins")), "minutes"))
  
  # Calculating lineup durations
  print("Calculating lineup durations")
  seconds_played <- calc_seconds_played(lineups=lineups)
  # Adding minutes played to play stats
  print("Adding minutes played to play stats")
  play_stats <- play_stats_add_time_played(play_stats, seconds_played)
  # Calculating cumulative box score
  print("Calculating cumulative box scores")
  play_stats <- current_box_score(play_stats)
  # Filling in stats on missing plays
  print("Filling in stats for plays each player wasn't involved in")
  play_stats <- fill_missing_plays(play_stats, season=season)
  # Calculating full game box scores
  print("Calculating full game box scores")
  box_calcs <- final_box_score(play_stats)
  # Retrieving official box scores
  print("Retrieving official box scores")
  box_truth <- get_official_box_score(season=season)
  # Combining calculated and official box scores
  print("Combining calculated and official box scores")
  comp_df <- create_comp_df(box_calcs, box_truth)
  
  # Comparing calculated and official box scores
  print("Comparing calculated and official box scores")
  comp_stats <- c("minutes", "pts", "threea", "threem", "fta", "ftm", "oreb", "dreb", "ast", "stl", "blk", "to", "ejected", "dbl_dbl", "tri_dbl")
  print("Perfect match rate")
  print(val_calc_box(comp_df, comp_stats))
  print("Average difference")
  print(val_calc_box(comp_df, comp_stats, type="avg"))
  print("Proportion with error <= 1")
  print(val_calc_box(comp_df, comp_stats, type="within"))
  
  # Adding double-double and triple-double targets
  play_stats <- add_targets(play_stats, box_calcs)
  
  # Drop redundant rows
  play_stats <- play_stats %>%
    arrange(game_id, game_play_number) %>%
    distinct(game_id, player_id, start_game_seconds_remaining, .keep_all=TRUE)
  return(play_stats)
}
