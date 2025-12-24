library(hoopR)
library(tidyverse)

# Creating team-season level data
team_df <- load_nba_team_box(season=2022:most_recent_nba_season()) %>%
  group_by(team_id, season) %>%
  summarize(
    team_location=stat_mode(team_location),
    team_name=stat_mode(team_name),
    team_abbreviation=stat_mode(team_abbreviation),
    team_color=paste0("#", stat_mode(team_color)),
    team_alternate_color=paste0("#", stat_mode(team_alternate_color)),
    team_logo=stat_mode(team_logo)
  )

# Writing team-season level data to csv
write.csv(team_df, "~/triple-double-predictor/data/team_data.csv", row.names=FALSE)
