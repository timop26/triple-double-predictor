source("~/triple-double-predictor/R/prep_modeling_data.R")

train_seasons <- 2022:2025

# Generating training base data
for (i in train_seasons) {
  start <- Sys.time()
  generate_base_data(i)
  end <- Sys.time()
  print(paste("Took", round(difftime(end, start, units="mins")), "minutes"))
}

# Adding game and score margin features to training data
for (i in train_seasons) {
  start <- Sys.time()
  create_modeling_data(i)
  end <- Sys.time()
  print(paste("Took", round(difftime(end, start, units="mins")), "minutes"))
}
