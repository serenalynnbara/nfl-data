
#install necessary packages
install.packages(c(
  "nflreadr",
  "tidyverse",
  "easyr",
  "purrr",
  "effectsize",
  "car",
  "pROC"))

##load libraries####
library(nflreadr)
library(tidyverse)
library(easyr)
library(purrr)
library(effectsize)
library(car)
library(pROC)

runfolder("src")

#load data
pbp <- load_pbp(2020:2024)

##select team data
games <- pbp %>%
  select(
    game_id,
    season,
    week,
    game_date,
    home_team,
    away_team,
    home_score,
    away_score
  ) %>%
  distinct() %>% 
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    home_score < away_score ~ away_team,
    home_score == away_score ~ "tie"),
    win_location = case_when(
      home_score > away_score ~ "home",
      home_score < away_score ~ "away",
      home_score == away_score ~ "tie"),
  )

##select vars from pbp
plays <- pbp %>%
  select(
    # identifiers
    game_id,
    play_id,
    season,
    season_type,
    week,
    
    # teams
    posteam,
    defteam,
    
    # situation
    qtr,
    down,
    ydstogo,
    yardline_100,
    score_differential,
    
    # play outcome
    play_type,
    yards_gained,
    
    # efficiency
    epa,
    wpa,
    wp,
    success,
    
    # scoring
    touchdown,
    field_goal_result,
    extra_point_result,
    
    # turnovers
    interception,
    fumble_lost,
    
    # passing
    pass_attempt,
    complete_pass,
    passing_yards,
    air_yards,
    yards_after_catch,
    cpoe,
    
    # rushing
    rush_attempt,
    rushing_yards,
    
    # pressure
    sack,
    qb_hit,
    
    # players
    passer_player_name,
    rusher_player_name,
    receiver_player_name,
    
    # decisions
    fourth_down_converted,
    fourth_down_failed,
    third_down_converted,
    third_down_failed
  ) 


##create new cols for analysis
plays <- plays %>% 
  mutate(
    turnover = case_when(
      interception == 1 ~ 1,
      fumble_lost == 1 ~ 1,
      TRUE ~ 0),
    explosive_play = case_when(
      pass_attempt == 1 & yards_gained >= 20 ~ 1,
      rush_attempt == 1 & yards_gained >= 10 ~ 1,
      TRUE ~ 0),
    red_zone_td = case_when(
      yardline_100 <= 20 & touchdown == 1 ~ 1,
      TRUE ~ 0),
    play_category = case_when(
      pass_attempt == 1 ~ "pass",
      rush_attempt == 1 ~ "rush",
      TRUE ~ "other"
    ))

##team games table
team_games <- bind_rows(
  games %>%
  transmute(
    game_id,
    team = home_team,
    opponent = away_team,
    points_for = home_score,
    points_against = away_score,
    home = 1,
    winner
  ),
 games %>%
  transmute(
    game_id,
    team = away_team,
    opponent = home_team,
    points_for = away_score,
    points_against = home_score,
    home = 0,
    winner
  )) %>% 
    mutate(
      win= if_else(team == winner, 1,0))


##metrics per game per team
team_play_metrics <- plays %>%
  filter(!is.na(posteam)) %>%
  group_by(game_id, posteam) %>%
  summarize(
    offensive_epa = mean(epa, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    explosive_rate = mean(explosive_play, na.rm = TRUE),
    yards_per_play = mean(yards_gained, na.rm = TRUE),
    pass_rate = mean(pass_attempt, na.rm = TRUE),
    rush_rate = mean(rush_attempt, na.rm = TRUE),
    red_zone_td_rate = mean(red_zone_td, na.rm = TRUE),
    turnovers = sum(turnover, na.rm = TRUE),
    fourth_down_attempts = sum(fourth_down_converted == 1 |
                                 fourth_down_failed == 1, na.rm = TRUE),
    third_down_conv_rate =
      sum(third_down_converted, na.rm = TRUE) /
      (sum(third_down_converted, na.rm = TRUE) +
         sum(third_down_failed, na.rm = TRUE)),
    fourth_down_conv_rate =
      if_else(
        sum(fourth_down_converted == 1 | fourth_down_failed == 1, na.rm = TRUE) >0,
        sum(fourth_down_converted == 1, na.rm = TRUE) /
          sum(fourth_down_converted == 1 | fourth_down_failed == 1, na.rm = TRUE),
        NA_real_
      ),
    .groups = "drop"
  )

analysis_metrics <- c(
  "offensive_epa",
  "success_rate",
  "explosive_rate",
  "yards_per_play",
  "pass_rate",
  "rush_rate",
  "red_zone_td_rate",
  "turnovers",
  "fourth_down_attempts",
  "third_down_conv_rate",
  "fourth_down_conv_rate"
)
model_metrics <- c(
  "offensive_epa",
  "explosive_rate",
  "red_zone_td_rate",
  "turnovers",
  "third_down_conv_rate"
)
names_metrics <- c(
  offensive_epa = "Offensive EPA",
  success_rate = "Success Rate",
  explosive_rate = "Explosive Play Rate",
  yards_per_play = "Yards per Play",
  pass_rate = "Pass Rate",
  rush_rate = "Rush Rate",
  red_zone_td_rate = "Red Zone TD Rate",
  turnovers = "Turnovers",
  fourth_down_attempts = "4th Down Attempts",
  third_down_conv_rate = "3rd Down Conversion Rate",
  fourth_down_conv_rate = "4th Down Conversion Rate"
)
