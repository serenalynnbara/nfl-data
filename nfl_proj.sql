
SELECT DATABASE();
create database nfl_project; 
USE nfl_project;
SHOW databases;

SHOW VARIABLES LIKE 'local_infile';
set global local_infile = 1;
truncate table model_data;

CREATE TABLE games (
game_id VARCHAR(25) PRIMARY KEY,
season INT,
week INT,
game_date DATE,
home_team VARCHAR(5),
away_team VARCHAR(5),
home_score INT,
away_score INT,
winner VARCHAR(5),
win_location VARCHAR(5)
);
SELECT COUNT(*) FROM games;
select * from games 
limit 10;

CREATE TABLE model_data (
game_id VARCHAR(25),
team VARCHAR(5),
opponent VARCHAR(5),
points_for INT,
points_against INT,
home INT,
winner VARCHAR(5),
win INT,
offensive_epa DECIMAL,
success_rate DECIMAL,
explosive_rate DECIMAL,
yards_per_play DECIMAL,
pass_rate DECIMAL,
rush_rate DECIMAL,
red_zone_rate DECIMAL,
turnover_rate DECIMAL,
fourth_down_attempts INT,
third_down_conv_rate DECIMAL,
fourth_down_conv_rate DECIMAL,
primary key (game_id, team)
);

LOAD DATA LOCAL INFILE 'C:/Users/18104/Documents/R/Football Analysis/data/model_data.csv'
INTO TABLE model_data
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 1 ROWS
(
    game_id,
    team,
    opponent,
    points_for,
    points_against,
    home,
    winner,
    win,
    @offensive_epa,
    @success_rate,
    @explosive_rate,
    @yards_per_play,
    @pass_rate,
    @rush_rate,
    @red_zone_rate,
    @turnover_rate,
    @fourth_down_attempts,
    @third_down_conv_rate,
    @fourth_down_conv_rate
)
SET
offensive_epa = NULLIF(@offensive_epa, 'NA'),
success_rate = NULLIF(@success_rate, 'NA'),
    explosive_rate = NULLIF(@explosive_rate, 'NA'),
    yards_per_play = NULLIF(@yards_per_play, 'NA'),
    pass_rate = NULLIF(@pass_rate, 'NA'),
    rush_rate = NULLIF(@rush_rate, 'NA'),
    red_zone_rate = NULLIF(@red_zone_rate, 'NA'),
    turnover_rate = NULLIF(@turnover_rate, 'NA'),
    fourth_down_attempts = NULLIF(@fourth_down_attempts, 'NA'),
    third_down_conv_rate = NULLIF(@third_down_conv_rate, 'NA'),
    fourth_down_conv_rate = NULLIF(@fourth_down_conv_rate, 'NA');

select * from model_data
limit 10;



CREATE TABLE team_games (
    game_id VARCHAR(25),
    team VARCHAR(5),
    opponent VARCHAR(5),
    points_for INT,
    points_against INT,
    home INT,
    winner VARCHAR(5),
    win INT,
    primary key (game_id, team)
);

LOAD DATA LOCAL INFILE 'C:/Users/18104/Documents/R/Football Analysis/data/model_data.csv'
INTO TABLE team_games
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(
    game_id,
    team,
    opponent,
    points_for,
    points_against,
    home,
    winner,
    win
);

select * from team_games
limit 10;
select count(*) from team_games;

##scratch work
show tables;
drop table team_games;
drop table model_data;
drop table games;
select count(*) from model_data;
