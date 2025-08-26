library(tidyverse)

data <- read.csv("~/triple-double-predictor/data/modeling_data_2025.csv") %>%
  filter(type_abbreviation == "STD", start_game_seconds_remaining > 0)

data$pts_needed <- ifelse(data$pts >= 10, 0, 10 - data$pts)
data$reb_needed <- ifelse(data$treb >= 10, 0, 10 - data$treb)
data$ast_needed <- ifelse(data$ast >= 10, 0, 10 - data$ast)
data$stl_needed <- ifelse(data$stl >= 10, 0, 10 - data$stl)
data$blk_needed <- ifelse(data$blk >= 10, 0, 10 - data$blk)

data$pts_needed_per_min <- data$pts_needed / (data$start_game_seconds_remaining / 60)
data$reb_needed_per_min <- data$reb_needed / (data$start_game_seconds_remaining / 60)
data$ast_needed_per_min <- data$ast_needed / (data$start_game_seconds_remaining / 60)
data$stl_needed_per_min <- data$stl_needed / (data$start_game_seconds_remaining / 60)
data$blk_needed_per_min <- data$blk_needed / (data$start_game_seconds_remaining / 60)


train_games <- sample(unique(data$game_id), 1000)
train_df <- filter(data, game_id %in% train_games)
test_df <- filter(data, !game_id %in% train_games)

fit1 <- glm(
  triple_double ~ home_away + as.factor(month) + I(broadcast_market == "national") + score_margin 
  + start_game_seconds_remaining * (pts_needed + reb_needed + ast_needed + stl_needed + blk_needed),
  data=train_df, family="binomial"
)

fit1 <- glm(
  triple_double ~ home_away + as.factor(month) + I(broadcast_market == "national") + score_margin 
  + pts_needed_per_min + reb_needed_per_min + ast_needed_per_min + stl_needed_per_min + blk_needed_per_min,
  data=train_df, family="binomial"
)

summary(fit1)

test_df <- arrange(test_df, game_id, player_id, game_play_number) %>%
  mutate(
    pred=predict(fit1, test_df, type="response"), 
    bucket=cut(pred, breaks=c(-1, seq(0, 0.01, by=0.001), 0.02, 0.03, 0.04, seq(0.05, 0.15, by=0.05), seq(0.2, 1, by=0.1))),
    )

cal <- test_df %>%
  group_by(bucket) %>%
  summarize(avg_pred=mean(pred), event_rate=mean(triple_double))

