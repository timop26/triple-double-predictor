library(tidyverse)

data <- read.csv("~/triple-double-predictor/data/sample_data.csv")

fit1 <- glm(
  triple_double ~ start_game_seconds_remaining * (pts + treb + ast + stl + blk) 
  + home_away + month + broadcast_market + score_margin,
  data=data, family="binomial"
)
