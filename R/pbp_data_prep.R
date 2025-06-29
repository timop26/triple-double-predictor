library(hoopR)
library(stringr)
library(tidyverse)

#pbp <- hoopR::load_nba_pbp()
#box_scores <- hoopR::load_nba_player_box()

create_player_dim <- function(seasons=most_recent_nba_season()) {
  box_scores <- hoopR::load_nba_player_box(seasons)
  return(unique(box_scores[, c("athlete_id", "athlete_display_name", "season")]))
}

find_starters <- function(season=most_recent_nba_season(), home_away="home") {
  hoopR::load_nba_player_box(season=season) %>%
    filter(starter, home_away == "home") %>%
    group_by(game_id) %>%
    mutate(starters=paste(athlete_id, collapse=", ")) %>%
    select(game_id, starters) %>%
    distinct() %>%
    return()
}

prep_pbp_for_cum_stats1 <- function(season=most_recent_nba_season()) {
  home_starters <- find_starters(season=season, home_away="home")
  away_starters <- find_starters(season=season, home_away="away")
  player_dim <- create_player_dim(season=season)
  hoopR::load_nba_pbp(season=season) %>%
    left_join(home_starters, by=join_by(game_id == game_id)) %>%
    left_join(away_starters, by=join_by(game_id == game_id)) %>%
    left_join(player_dim, by=join_by(athlete_id_1 == athlete_id)) %>%
    rename(athlete_name_1=athlete_display_name) %>%
    left_join(player_dim, by=join_by(athlete_id_2 == athlete_id)) %>%
    rename(athlete_name_2=athlete_display_name) %>%
    left_join(player_dim, by=join_by(athlete_id_3 == athlete_id)) %>%
    rename(athlete_name_3=athlete_display_name) %>%
    mutate(
      home_starters=ifelse(game_play_number > 2, NA, home_starters), 
      away_starters=ifelse(game_play_number > 2, NA, away_starters)
    ) %>%
    return()
}

prep_pbp_for_cum_stats2 <- function(season=most_recent_nba_season()) {
  pbp <- prep_pbp_for_cum_stats1(season=season) %>%
    filter(is.na(athlete_name_3), !is.na(athlete_name_1)) %>%
    select(
      game_id, 
      game_play_number,
      start_game_seconds_remaining,
      athlete_name_1, 
      athlete_name_2, 
      text, 
      type_text, 
      shooting_play, 
      scoring_play
    )
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
    player_id 
) {
  box <- data.frame(
    game_id=game_id,
    game_play_number=game_play_number,
    start_game_seconds_remaining,
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
  if (player_id == 1) {
    # If it's a shooting play, need to determine free throw, 2 pointer, or 3 pointer
    if (shooting_play) {
      type <- case_when(str_detect(text, "free") ~ "ft", str_detect(text, "three") ~ "three", TRUE ~ "two")
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
  pbp <- prep_pbp_for_cum_stats2(season=season)
  bind_rows(
    mutate(rename(pbp, player=athlete_name_1), player_id=1) %>%
      select(-athlete_name_2) %>%
      pmap_dfr(player_stats),
    mutate(rename(pbp, player=athlete_name_2), player_id=2) %>%
      filter(!is.na(player)) %>%
      select(-athlete_name_1) %>%
      pmap_dfr(player_stats)
  ) %>%
    arrange(game_id, game_play_number) %>%
    return()
}

player_dim <- create_player_dim(2021:2025)

home_starters <- find_starters(home_away="home")

pbp <- prep_pbp_for_cum_stats2()

start <- Sys.time()
play_stats <- calc_play_stats()
end <- Sys.time()


box_calcs <- play_stats %>%
  group_by(game_id, player_name) %>%
  summarize_all(sum) %>%
  mutate(pts=twom * 2 + threem * 3 + ftm, treb=oreb + dreb) %>%
  select(
    game_id,
    player_name,
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
    tech,
    flagrant
  )

box_truth <- box_scores %>%
  filter(game_id <= 401705000, !is.na(minutes)) %>%
  mutate(
    twoa=field_goals_attempted - three_point_field_goals_attempted,
    twom=field_goals_made - three_point_field_goals_made
  ) %>%
  rename(
    game_id=game_id,
    player_name=athlete_display_name,
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
    player_name,
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
    ejected
  ) %>%
  arrange(game_id, player_name)


# minutes, field goals made, field goals attempted (do these after the fact), three pointers made, 
## three pointers attempted, free throws made, free throws attempted, total points (do this after 
## the fact), total rebounds (do this after the fact), assists, steals, fouls, technical fouls

# there are some name mappings i'll have to clean up. Jimmy Butler vs Jimmy Butler III, Bub Carrington, Alex Sarr, , some 
# blocked shots are "Player 1 blocks Player 2's shot" instead of "Player 1 blocks Player 2 's shot"


woof <- nba_live_boxscore(game_id="0042400401")
woof2 <- load_nba_schedule()
woof3 <- nba_playercareerstats()
woof4 <- load_nba_player_box()

# Strip out statistics from each event
# Create rolling box score
# Will like need to join box score with prior season/career stats (create with load_nba_player_box), 
## venue stats (load_nba_schedule), etc.
# Will need to make sure features in model are available in live box score (nba_live_boxscore(game_id))
# Can use load_nba_schedule() to find all game ids for the day
# Can then use nba_live_boxscore(game_id) to get current box score
