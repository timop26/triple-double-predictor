import streamlit as st
import pandas as pd
import numpy as np

# Page title
st.title("Triple Double Predictor")

player_df = pd.read_csv("data/player_data.csv")

def get_player_prior_season_stats(player_id, season):
    info = player_df.loc[
        (player_df["athlete_id"] == player_id) & (player_df["season"] == season),
        ["athlete_id", "season", "headshot_href", "full_name", "pos", "height", "years"]
    ]
    stats = player_df.loc[
        (player_df["athlete_id"] == player_id) & (player_df["season"] == (season - 1)), 
        ["gp", "ppg", "apg", "rpg", "min", "tdbl"]
    ]
    stats.columns = stats.columns + "_prior_season"
    return pd.concat([info.reset_index(drop=True), stats.reset_index(drop=True)], axis=1)

def get_all_names(season):
    return player_df.loc[player_df["season"] == season, ["athlete_id", "full_name"]]

def get_prior_season_stats(season):
    return pd.concat([get_player_prior_season_stats(id, season) for id in get_all_names(season)["athlete_id"]])

y25 = get_prior_season_stats(2025)

choice = st.selectbox("Player:", y25["full_name"])
choice_df = y25[y25["full_name"] == choice].iloc[0]

col1, col2 = st.columns([1, 1])
with col1:
    st.image(choice_df["headshot_href"], caption=choice, use_container_width=True)

with col2:
    st.text("Prior Season")
    prior_season_gp = st.number_input(
        "Games Played",
        value=choice_df["gp_prior_season"]
    )
    prior_season_ppg = st.number_input(
        "Points/Game",
        value=choice_df["ppg_prior_season"]
    )
    prior_season_reb = st.number_input(
        "Rebounds/Game",
        value=choice_df["rpg_prior_season"]
    )
    prior_season_ast = st.number_input(
        "Assists/Game",
        value=choice_df["apg_prior_season"]
    )
    prior_season_min = st.number_input(
        "Minutes/Game",
        value=choice_df["min_prior_season"]
    )
    prior_season_tdbl = st.number_input(
        "Triple Doubles",
        value=choice_df["tdbl_prior_season"]
    )

    # Basic text
st.write("This is a simple Streamlit app. Change the code and watch it update!")

# Input widget
name = st.text_input("What's your name?", "World")

# Button
if st.button("Say hello"):
    st.success(f"Hello, {name}!")

# Slider example
x = st.slider("Select a number", 0, 100, 25)
st.write("You picked:", x)

# Chart example
chart_data = pd.DataFrame(
    np.random.randn(20, 3),
    columns=["a", "b", "c"]
)
st.line_chart(chart_data)