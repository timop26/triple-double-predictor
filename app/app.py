import streamlit as st
import pandas as pd
import numpy as np
from huggingface_hub import hf_hub_download
from joblib import load
import os

# Utilities
def stats_needed(x):
    return np.where(x >= 10, 0, 10 - x)

DATASET_REPO = "timop26/nba-running-box-scores"
MODEL_REPO = "timop26/triple-double-predictor"
HF_TOKEN = os.environ.get("HF_TOKEN")

# Cached resources
@st.cache_resource
def load_model():
    model_path = hf_hub_download(
        repo_id=MODEL_REPO,
        filename="model_pipeline_v1.pkl",
        token=HF_TOKEN
    )
    return load(model_path)

@st.cache_data
def load_player_data():
    data_path = hf_hub_download(
        repo_id=DATASET_REPO,
        filename="player_data.csv",
        repo_type="dataset",
        token=HF_TOKEN
    )
    return pd.read_csv(data_path)

@st.cache_data
def get_prior_season_stats(player_df, season):
    def _single(player_id):
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

    ids = player_df.loc[player_df["season"] == season, "athlete_id"]
    return pd.concat([_single(i) for i in ids], ignore_index=True)

# Load model and player data
model = load_model()
player_df = load_player_data()
prior_season_stats = get_prior_season_stats(player_df, 2026)

# Session state
if "prob_percent" not in st.session_state:
    st.session_state.prob_percent = None

# Custom css
st.markdown(
    """
    <style>
        .block-container {
            padding-top: 1rem;
            padding-bottom: 3rem;
            padding-left: 1rem;
            padding-right: 1rem;
        }
        /* Update form submit formatting */
        .stFormSubmitButton>button {
            font-size: 20px;
            height: 3em;
            background-color: #C9082A;
            color: #F5F5F5;
        }
        .stFormSubmitButton>button p {
            font-size: 20px;
            font-weight: 700 !important;
        }
        div[data-testid="stFormSubmitButton"] > button:hover {
            background-color: #E63946;
            color: #F5F5F5;
        }

        /* Remove the default Streamlit form border */
        div[data-testid="stForm"] {
            border: none !important;
            padding: 0 !important;
        }
    </style>
    """,
    unsafe_allow_html=True
)

# Title
st.markdown(
    """
    <h1 style='text-align: center; font-size: 50px; font-weight: bold;'>
        Triple Double Predictor
    </h1>
    """,
    unsafe_allow_html=True
)

# Player selection
col1, col2 = st.columns([2, 1], vertical_alignment="center")

with col1:
    st.markdown(
        "<div style='text-align: center; font-size:25px; font-weight:600;'>Player</div>",
        unsafe_allow_html=True
    )
    choice = st.selectbox(
        "",
        prior_season_stats["full_name"],
        label_visibility="collapsed",
        key="selected_player"
    )
    if st.session_state.get("last_player") != st.session_state.selected_player:
        st.session_state.prob_percent = None
        st.session_state.last_player = st.session_state.selected_player
    choice_df = prior_season_stats.loc[
        prior_season_stats["full_name"] == choice
    ].iloc[0]

with col2:
    st.image(choice_df["headshot_href"], use_container_width=True)

# Form submit
with st.form("triple_double_form"):

    st.markdown(
        "<h1 style='text-align: center; font-size: 25px; font-weight: bold;'>Prior Season</h1>",
        unsafe_allow_html=True
    )

    c1, c2, c3 = st.columns(3)
    with c1:
        prior_season_ppg = st.number_input("Points/Game", value=choice_df["ppg_prior_season"], step=0.1)
        prior_season_gp = st.number_input("Games Played", value=choice_df["gp_prior_season"], step=0.1)
    with c2:
        prior_season_reb = st.number_input("Rebounds/Game", value=choice_df["rpg_prior_season"], step=0.1)
        prior_season_min = st.number_input("Minutes/Game", value=choice_df["min_prior_season"], step=0.1)
    with c3:
        prior_season_ast = st.number_input("Assists/Game", value=choice_df["apg_prior_season"], step=0.1)
        prior_season_tdbl = st.number_input("Triple Doubles", value=choice_df["tdbl_prior_season"], step=0.1)

    st.markdown(
        "<h1 style='text-align: center; font-size: 25px; font-weight: bold;'>Game Status</h1>",
        unsafe_allow_html=True
    )

    c1, c2, c3 = st.columns(3)
    with c1:
        pts = st.number_input("Points", value=0)
        reb = st.number_input("Rebounds", value=0)
        ast = st.number_input("Assists", value=0)
        blk = st.number_input("Blocks", value=0)
    with c2:
        min_remaining = st.number_input("Min Remaining", value=48, max_value=48)
        score_margin = st.number_input("Score Margin", value=0)
        fouls = st.number_input("Fouls", value=0)
        minutes_played = st.number_input("Minutes Played", value=0, max_value=88)
    with c3:
        stl = st.number_input("Steals", value=0)
        month = st.number_input("Month of the Year", value=1)
        home_away = st.segmented_control(
            "Home/Away", ["home", "away"], default="home", width="stretch"
        )

    left, center, right = st.columns([1, 2.5, 1])
    with center:
        submitted = st.form_submit_button("Calculate Triple Double Probability")

# Create inference data
if submitted:
    data = {
        "home_away": home_away,
        "month": month,
        "pos": choice_df["pos"],
        "start_game_seconds_remaining": min_remaining * 60,
        "score_margin": score_margin,
        "fls": fouls,
        "minutes": minutes_played,
        "height": choice_df["height"],
        "years": choice_df["years"],
        "gp_prior_season": prior_season_gp,
        "ppg_prior_season": prior_season_ppg,
        "apg_prior_season": prior_season_ast,
        "rpg_prior_season": prior_season_reb,
        "min_prior_season": prior_season_min,
        "tdbl_prior_season": prior_season_tdbl,
        "pts": pts,
        "treb": reb,
        "ast": ast,
        "stl": stl,
        "blk": blk
    }

    input_df = pd.DataFrame([data])
    prob = model.predict_proba(input_df)[:, 1][0]
    st.session_state.prob_percent = round(prob * 100, 1)

# Result
if st.session_state.prob_percent is not None:
    st.markdown(
        f"""
        <h1 style='text-align: center; font-size: 25px; font-weight: bold;'>
            🏀 Triple Double Probability: {st.session_state.prob_percent}%
        </h1>
        """,
        unsafe_allow_html=True
    )
