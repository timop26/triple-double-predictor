## Overview

pbp_data_prep.R: creates cumulative box score by player per play per game for a given season

add_features.R: adds the player's team, home/away status, scoring margin to that point in the game based on the player's team, and game attributes (date, venue, type)

player_dim.R: creates or appends to a player_data.csv with one row per player and season with player attributes and statistics. Will need to think about how often this should be re-ran.

team_dim.R: creates a team_data.csv with one row per team and season with team attributes. Only needs to be re-ran once per season.

prep_modeling_data.R: function to create the in-game player stats (using pbp_data_prep.R) and store them. Function to create the modeling data and store it. Includes adding features from add_features.R, dropping the last 30 seconds of each game and players on and after the play they achieved a triple-double, their team, and the player's stats from the prior season.

generate_train_data.R: runs the functions from prep_modeling_data.R for each season to be used in training

generate_prediction_data.R: runs the functions from prep_modeling_data.R for the current season. Need to decide how often to re-run.