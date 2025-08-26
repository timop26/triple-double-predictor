woof <- nba_live_boxscore(game_id="0042400401")
woof2 <- load_nba_schedule()
woof3 <- nba_playercareerstats()
woof4 <- load_nba_player_box()

# Will like need to join box score with prior season/career stats (create with load_nba_player_box)
# Will need to make sure features in model are available in live box score (nba_live_boxscore(game_id))
# Can use load_nba_schedule() to find all game ids for the day
# Can then use nba_live_boxscore(game_id) to get current box score

