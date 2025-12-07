source("~/triple-double-predictor/R/prep_modeling_data.R")

season <- 2026

# Generating training base data
start <- Sys.time()
generate_base_data(season)
end <- Sys.time()
print(paste("Took", round(difftime(end, start, units="mins")), "minutes"))


# Adding game and score margin features to training data
start <- Sys.time()
create_modeling_data(season)
end <- Sys.time()
print(paste("Took", round(difftime(end, start, units="mins")), "minutes"))
