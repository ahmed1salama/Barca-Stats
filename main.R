library(tidyverse)
library(DT)
library(lubridate)



# home tab summary part
matches = read_csv("barca_matches.csv")

matches = matches %>% mutate(match_date = mdy(match_date))

matches_num = nrow(matches)
goals_for = 0
goals_against = 0
wins = 0
draws = 0

for(i in 1:matches_num){
  goals_for = ifelse(matches$home_team.home_team_name[i] == "Barcelona", goals_for + matches$home_score[i],
                 goals_for + matches$away_score[i])
  
  goals_against = ifelse(matches$home_team.home_team_name[i] == "Barcelona", goals_against + matches$away_score[i],
                     goals_against + matches$home_score[i])
  
  wins = ifelse((matches$home_team.home_team_name[i] == "Barcelona" & matches$home_score[i] > matches$away_score[i]) | 
                (matches$away_team.away_team_name[i] == "Barcelona" & matches$away_score[i] > matches$home_score[i]),
                wins + 1, wins)
  
  draws = ifelse(matches$home_score[i] == matches$away_score[i], draws + 1, draws)
}

losses = matches_num - wins - draws

# players stats tab 

players_stats = read_csv("barca_players_stats.csv", locale = readr::locale(encoding = "latin1"))

fifa_stats = read_csv("fifa_stats.csv")


players_stats = players_stats %>% inner_join(fifa_stats %>% select(player_id,
                                                                 jersy_id,
                                                                 Age,
                                                                 Height,
                                                                 Pos,
                                                                 Ovr))

fifa_stats = fifa_stats %>% select(c(2,9:14))

# el classico events

classico_events = read_csv("el_classico_events.csv", locale = readr::locale(encoding = "latin1")) %>% as.data.frame()


classico_events = as.data.frame(classico_events)