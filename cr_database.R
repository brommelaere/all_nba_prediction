rm(list=ls()) # Removes all objects from the current workspace (R memory)
options(scipen=999) # Do not print with Scientific Notation

# Program Details: Overview -----------------------------------------------
# *************************************************************************

script.meta <- list(
  
  ##
  Programmer   = "Ben Rommelaere",
  Case				 = "ALL NBA TEAM DATASET CREATION",
  Program      = "",
  Version      = 1,
  Date_Created = "12/26/2019",
  Last_Updated = "02/16/2020",
  
  Description  = 
    
    "Load csvs & combine into dataset for analysis.",
  
  Notes        = 
    
    "Saves completed data to base folder"
  
)

# *************************************************************************

# packages ----------------------------------------------------------------
library(tidyverse)

# Paths -------------------------------------------------------------------
main <- "/Users/benrommelaere/Desktop/Data Science/NBA_Stats" #change this to local repo location
untouched <- file.path(main, "01 Untouched")
raw <- file.path(main, "02 Raw")
base <- file.path(main, "03 Base")
temp <- file.path(main, "04 Intermediate")
output <- file.path(main, "05 Output")

# ----------------------------------------------------------
# 1: Putting All the Data Together
# ----------------------------------------------------------
players.df <- read.csv(file = file.path(raw, str_c("Players", ".csv", sep="")),
                       header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") 

player.data.df <- read.csv(file = file.path(raw, str_c("player_data", ".csv", sep="")),
                           header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") 

player.stats.df <- read.csv(file = file.path(raw, str_c("Seasons_Stats", ".csv", sep="")),
                            header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  filter(Year >= 1988) %>% 
  rename(true.shoot = TS.,
         three.perc = X3PAr,
         ft.perc = FTr,
         o.rpg = ORB.,
         d.rpg = DRB.,
         rpg = TRB.,
         apg = AST., 
         spg = STL.,
         bpg = BLK.,
         to.pg = TOV.,
         usage.perc = USG.,
         win.share = WS,
         fga = FGA,
         fta = FTA,
         fg.perc = X2P.,
         three.a = X3PA,
         points = PTS,
         fouls = PF) %>% 
  mutate(ppg = points / G,
         fouls.pg = fouls/G) %>% 
  select(X, Year, Player, Pos, Age, Tm, G, GS, MP,
         ppg, o.rpg, d.rpg, rpg, apg, spg, bpg, PER,
         true.shoot, three.perc, three.a, ft.perc, fta, fga, fg.perc,
         to.pg, fouls.pg,
         usage.perc, win.share, BPM, VORP) %>% 
  left_join(players.df , by = "Player") %>% 
  mutate(Player = str_replace(Player, "\\*", "")) %>% 
  select(-X.y) %>% 
  rename(id = X.x) 

team.records.df <- read.csv(file = file.path(raw, str_c("Team_Records", ".csv", sep="")),
                            header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  mutate(Team = str_replace(Team, "\\*", ""))  

team.acro.df <- read.csv(file = file.path(raw, str_c("team_acronyms_v1", ".csv", sep="")),
                         header = F, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  rename(Team = V2,
         Acronym = V1)

all.nba.df <- read.csv(file = file.path(raw, str_c("All_NBA_Teams_v2", ".csv", sep="")),
                       header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  filter(Season!="") %>% 
  gather('C', starts_with("F"), starts_with("G"), key = "position", value = "name") %>% 
  mutate(position = str_replace(position, "1", ""),
         position = str_replace(position, "2", ""),
         Year = Year + 1,
         name = str_replace(name, " ", " "))

## Fix up players with trades
# Flag traded players
traded <- player.stats.df %>% 
  filter(Tm == "TOT") %>% 
  select("Player", "Year") %>% 
  mutate(traded = 1)

# Update team to last team played for
player.stats.df2 <- player.stats.df %>% 
  left_join(traded, by = c("Player", "Year")) %>% 
  group_by(Player, Year) %>% 
  mutate(count = cumsum(traded),
         team = Tm[count==3]) %>% 
  filter((Tm == "TOT" & traded==1) | is.na(traded)) %>% 
  mutate(Tm = ifelse(is.na(traded), Tm, team)) %>% 
  select(-count, -team) %>% 
  mutate(Tm = case_when(Tm %in% "BRK" ~ "BKN",
                        Tm %in% "PHO" ~ "PHX",
                        Tm %in% "CHH" ~ "CHO",
                        TRUE ~ Tm))

## Add Acronyms to team records
team.records.df2 <- team.records.df %>% 
  left_join(team.acro.df, by = "Team") %>% 
  mutate(Year = as.numeric(substr(Season, 1, 4)) +1) %>% 
  filter(Year != 2018) %>% 
  rename(Tm = Acronym) %>% 
  select(Team, Tm, Season, everything())

## Add team record to season stats  
player.stats.df3 <- player.stats.df2 %>% 
  left_join(team.records.df2, by = c("Year", "Tm"))

## Add All-NBA Flag to Season Stats
all.nba.df2 <- all.nba.df %>% 
  rename(Player = name,
         Position = position,
         Award = Team) %>% 
  select("Year", "Award", "Position", "Player") %>% 
  mutate(Player = str_trim(Player))

player.stats.df4 <- player.stats.df3 %>% 
  ungroup() %>% 
  mutate(Player = str_trim(Player),
         Player = as.character(Player)) %>% 
  left_join(all.nba.df2 , by=c("Year", "Player")) %>% 
  mutate(Category = ifelse(is.na(Award), "None", "All-NBA")) %>% 
  select(id, Year, Player, Pos, Age, Category, Award, everything())

## Prediction Model for All-NBA Players
## Start with Logit Model 
pred.df <- player.stats.df4 %>% 
  select(id, Year, Player, Category, Pos, Age, Tm, G, GS, MP, PER,
         ppg, o.rpg, d.rpg, rpg, apg, spg, bpg, 
         true.shoot, three.perc, three.a, ft.perc, fta, fga, fg.perc,
         to.pg, fouls.pg,
         usage.perc, win.share, BPM, VORP, 
         height, weight, 
         W, W.L., Pace, traded) %>% 
  mutate(all_nba = ifelse(Category=="None", 0, 1),
         traded = ifelse(is.na(traded), 0, traded))

pred.df1 <- pred.df %>% 
  drop_na()

## Ensure no dropped data is for ALL-NBA
pred.df %>% 
  anti_join(pred.df1, by = "id") %>% 
  group_by(Category) %>% 
  summarise(count = n())

pred.df <- pred.df %>% 
  drop_na()

save(pred.df1,
     file = file.path(base, str_c("prediction_data_v1", ".Rda", sep="")))
