import streamlit as st
import pandas as pd
import numpy as np
from huggingface_hub import hf_hub_download
from joblib import load
import os

def stats_needed(x):
    return np.where(x >= 10, 0, 10 - x)

# Configuration
DATASET_REPO = "timop26/nba-running-box-scores"
MODEL_REPO = "timop26/triple-double-predictor"
HF_TOKEN = os.environ.get("HF_TOKEN")

@st.cache_resource
def load_model():
    """Download and load model from HuggingFace"""
    model_path = hf_hub_download(
        repo_id=MODEL_REPO,
        filename="model_pipeline_v1.pkl",
        token=HF_TOKEN
    )
    return load(model_path)

@st.cache_data
def load_player_data():
    """Download and load player data from HuggingFace"""
    data_path = hf_hub_download(
        repo_id=DATASET_REPO,
        filename="player_data.csv",
        repo_type="dataset",
        token=HF_TOKEN
    )
    return pd.read_csv(data_path)

def get_player_prior_season_stats(player_df, player_id, season):
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

def get_all_names(player_df, season):
    return player_df.loc[player_df["season"] == season, ["athlete_id", "full_name"]]

def get_prior_season_stats(player_df, season):
    return pd.concat([get_player_prior_season_stats(player_df, id, season) for id in get_all_names(player_df, season)["athlete_id"]])

st.markdown("""
    <style>
        .block-container {
            padding-top: 1rem;
            padding-bottom: 3rem;
            padding-left: 1rem;
            padding-right: 1rem;
        }
        /* App containers */
        .stApp,
        html,
        body,
        [data-testid="stSidebar"],
        [data-testid="stMarkdownContainer"] * {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI",
                     Helvetica, sans-serif !important;
        }

        /* Base Web widgets (this is the critical addition) */
        [data-baseweb],
        [data-baseweb] * {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI",
                     Helvetica, sans-serif !important;
        }

        /* Adjusting selectbox font size */
        div.stSelectbox > div > div {
            font-size: 20px;
        }

        /* Adjusting form submit button */
        .stFormSubmitButton>button {
            font-size: 20px; /* Adjust the font size as needed */
            height: 3em;     /* Optional: Adjust button height to fit the larger text */
            background-color: #C9082A;   /* change color */
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
    </style>
    """, unsafe_allow_html=True
)

# Page title
st.markdown(
    """
    <h1 style='text-align: center; font-size: 50px; font-weight: bold;'>
        Triple Double Predictor
    </h1>
    """,
    unsafe_allow_html=True
)

# Load the model and data (replaces your original two lines)
with st.spinner("Loading model and player data..."):
    model = load_model()
    player_df = load_player_data()
    prior_season_stats = get_prior_season_stats(player_df, 2026)

col1, col2 = st.columns([2, 1], vertical_alignment="center")
with col1:

    st.markdown(
        "<div style='text-align: center; font-size:25px; font-weight:600;'>Player</div>",
        unsafe_allow_html=True
    )
    choice = st.selectbox("", prior_season_stats["full_name"], label_visibility="collapsed")
    choice_df = prior_season_stats[prior_season_stats["full_name"] == choice].iloc[0]

with col2:
    st.image(choice_df["headshot_href"], use_container_width=True)


with st.form("triple_double_form"):

    # ---------- PRIOR SEASON ----------
    st.markdown(
        """
        <h1 style='text-align: center; font-size: 25px; font-weight: bold;'>
            Prior Season
        </h1>
        """,
        unsafe_allow_html=True
    )

    col1, col2, col3 = st.columns(3)

    with col1:
        prior_season_ppg = st.number_input(
            "Points/Game", value=choice_df["ppg_prior_season"]
        )
        prior_season_gp = st.number_input(
            "Games Played", value=choice_df["gp_prior_season"]
        )

    with col2:
        prior_season_reb = st.number_input(
            "Rebounds/Game", value=choice_df["rpg_prior_season"]
        )
        prior_season_min = st.number_input(
            "Minutes/Game", value=choice_df["min_prior_season"]
        )

    with col3:
        prior_season_ast = st.number_input(
            "Assists/Game", value=choice_df["apg_prior_season"]
        )
        prior_season_tdbl = st.number_input(
            "Triple Doubles", value=choice_df["tdbl_prior_season"]
        )

    # ---------- GAME STATUS ----------
    st.markdown(
        """
        <h1 style='text-align: center; font-size: 25px; font-weight: bold;'>
            Game Status
        </h1>
        """,
        unsafe_allow_html=True
    )

    col1, col2, col3 = st.columns(3)

    with col1:
        pts = st.number_input("Points", value=0)
        reb = st.number_input("Rebounds", value=0)
        ast = st.number_input("Assists", value=0)
        blk = st.number_input("Blocks", value=0)

    with col2:
        min_remaining = st.number_input("Min Remaining", value=48)
        score_margin = st.number_input("Score Margin", value=0)
        fouls = st.number_input("Fouls", value=0)
        minutes_played = st.number_input("Minutes Played", value=0)

    with col3:
        stl = st.number_input("Steals", value=0)
        month = st.number_input("Month of the Year", value=1)
        home_away = st.segmented_control("Home/Away", ["home", "away"], width="stretch", default="home")

    # ---------- SUBMIT ----------
    left, center, right = st.columns([1, 2.5, 1])

    with center:
        submitted = st.form_submit_button(
            "Calculate Triple Double Probability"
        )

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
        prob_percent = round(prob * 100, 1)

        st.markdown(
            f"""
            <h1 style='text-align: center; font-size: 25px; font-weight: bold;'>
                🏀 Triple Double Probability: {prob_percent}%
            </h1>
            """,
            unsafe_allow_html=True
        )
