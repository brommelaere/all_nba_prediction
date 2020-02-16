rm(list=ls()) # Removes all objects from the current workspace (R memory)
options(scipen=999) # Do not print with Scientific Notation

# Program Details: Overview -----------------------------------------------
# *************************************************************************

script.meta <- list(
  
  ##
  Programmer   = "Ben Rommelaere",
  Case				 = "ALL NBA TEAM PREDICTION",
  Program      = "",
  Version      = 1,
  Date_Created = "12/26/2019",
  Last_Updated = "12/26/2019",
  
  Description  = 
    
    "Get List & All Metadata",
  
  Notes        = 
    
    ""
  
)

# *************************************************************************

# packages ----------------------------------------------------------------

library(tidyverse)
library(tictoc)
library(data.table)
library(dplyr)
library(formattable)
require(stringi)
library(ggplot2)
library(mapdata)
library(ggmap)
library(maps)
library(scales)
library(ggwordcloud)

# Paths -------------------------------------------------------------------

main <- "/Users/benrommelaere/Desktop/Data Science/NBA_Stats"
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
     file = file.path(temp, str_c("prediction_data_v1", ".Rda", sep="")))

### BEGIN TRAINING DATA CODE
names(pred.df)  
dim(pred.df)
cor(pred.df[8:38])

nrow(pred.df %>% filter(all_nba==1))

glm.fit=glm(all_nba~traded + Age + GS + MP + ppg + apg + o.rpg + d.rpg + spg + PER +
              bpg + fouls.pg + to.pg + W.L. + Pace + BPM + VORP + win.share + traded + true.shoot + 
              three.perc + three.a + ft.perc + fta + fga + fg.perc,
            data = pred.df, 
            family = binomial)

summary(glm.fit)

glm.probs = predict(glm.fit ,type ="response")

glm.pred = rep("None" , 12971)
glm.pred[glm.probs >.5]="All-NBA"

table(glm.pred, pred.df$Category)
mean(glm.pred == pred.df$Category)

train = (pred.df$Year<2017)
pred.df.2017 = pred.df[!train ,]
dim(pred.df.2017)
cat.2017= pred.df$Category[!train]

## The 2017 All-NBA team
names(pred.df.2017)
pander(pred.df.2017 %>% filter(all_nba==1) %>% select('Player', 'Pos', 'Age', 'Tm', 'G', 'GS', 'W.L.'))

glm.fit=glm(all_nba~traded + Age + GS + MP + ppg + apg + o.rpg + d.rpg + spg + PER +
              bpg + fouls.pg + to.pg + W.L. + Pace + BPM + VORP + win.share + traded + true.shoot + 
              three.perc + three.a + ft.perc + fta + fga + fg.perc,
            data=pred.df, 
            family = binomial,
            subset = train)

glm.probs = predict(glm.fit, pred.df.2017, type= "response")

pred.df <- pred.df %>% 
  mutate(all_nba = as.factor(all_nba))

ggplot(data = pred.df, mapping = aes(x = ppg, y = VORP, color = all_nba)) +
  geom_point()

value = sort(glm.probs)[length(glm.probs)-15]

glm.pred = rep("None" , 484)
glm.pred[glm.probs > value]="All-NBA"

table(glm.pred, pred.df.2017$Category)

report <- data.frame(pred.df.2017$Player, pred.df.2017$Pos, glm.pred, pred.df.2017$all_nba) %>% 
  rename(Player = pred.df.2017.Player,
         Position = pred.df.2017.Pos,
         Predicted.Label = glm.pred,
         Actual.Label = pred.df.2017.all_nba) %>% 
  mutate(Actual.Label = ifelse(Actual.Label==1, "All-NBA", "None")) %>% 
  filter((Predicted.Label=="All-NBA" & Actual.Label=="None") | (Predicted.Label=="None" & Actual.Label=="All-NBA")) %>% 
  arrange(Predicted.Label)

# Base Decision rule off positions
table(pred.df.2017$Pos)
(pred.df.2017 %>% filter(Pos=="PF-C"))$Player

update.rule <- data.frame(glm.probs, pred.df.2017$Player, pred.df.2017$Pos, pred.df.2017$all_nba, pred.df.2017$W.L.) %>% 
  rename(Probability = glm.probs,
         Player = pred.df.2017.Player,
         Position = pred.df.2017.Pos,
         Team.WinLoss = pred.df.2017.W.L.,
         Actual.Label = pred.df.2017.all_nba) %>% 
  mutate(Position.Group = ifelse(Position == "PG" | Position == "SG", "G",
                                 ifelse(Position == "SF" | Position == "PF", "F", "C"))) %>% 
  group_by(Position.Group) %>% 
  mutate(Position.Rank = order(order(Probability, decreasing=TRUE))) %>% 
  arrange(Position.Group, -Probability) %>% 
  mutate(Actual.Label = ifelse(Actual.Label==1, "All-NBA", "None")) %>% 
  mutate(Predicted.Label = ifelse(Position.Rank<=3 & Position.Group=="C", "All-NBA", 
                               ifelse(Position.Rank<=6 & Position.Group!="C", "All-NBA", "None")))
  
table(update.rule$Predicted.Label, update.rule$Actual.Label)

report <- update.rule %>% 
  filter(Predicted.Label != Actual.Label) %>% 
  arrange(Predicted.Label)

tab1 <- pred.df.2017 %>%
  filter(all_nba==1) %>%
  select('Player', 'Pos', 'Age', 'Tm', 'G', 'GS', 'W.L.', 'ppg') %>% 
  arrange(-ppg)

table(tab1) %>% 
  kable('html') %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)

mean(glm.pred != pred.df.2017$Category)

## Just significant
glm.fit=glm(all_nba~ GS + MP + ppg + apg + d.rpg + PER +
              bpg + fouls.pg + to.pg + W.L. + BPM + VORP + win.share + true.shoot + 
              three.perc + three.a + ft.perc + fta + fga,
            data=pred.df, 
            family = binomial,
            subset = train)

summary(glm.fit)
glm.probs = predict(glm.fit, pred.df.2017, type= "response")

value = sort(glm.probs)[length(glm.probs)-15]

glm.pred = rep("None" , 484)
glm.pred[glm.probs > value]="All-NBA"

table(glm.pred, pred.df.2017$Category)

## Linear Discriminant Analysis 
library(MASS)
lda.fit=lda(all_nba~ GS + MP + ppg + apg + d.rpg + PER +
              bpg + fouls.pg + to.pg + W.L. + BPM + VORP + win.share + true.shoot + 
              three.perc + three.a + ft.perc + fta + fga,
            data=pred.df,
            subset = train)

lda.fit
lda.pred=predict(lda.fit, pred.df.2017)

base <- as_tibble(lda.pred) 

posterior1 <- base$posterior[,1] %>% 
  as_tibble() %>% 
  rename(posterior1 = value)

posterior2 <- as_tibble(base$posterior[,1]) %>% 
  as_tibble() %>% 
  rename(posterior2 = value)

posterior2 <- as_tibble(base$posterior[,1]) %>% 
  as_tibble() %>% 
  rename(posterior2 = value)

posterior2 <- as_tibble(base$posterior[,1]) %>% 
  as_tibble() %>% 
  rename(posterior2 = value)

top15only <- cbind(top15only, posterior1, posterior2) %>% 
  as_tibble()
  
table(lda.class, pred.df.2017$Category)
mean(lda.class == pred.df.2017$all_nba)
sum(lda.pred$posterior[ ,1] >=.5)
sum(lda.pred$posterior[,1] <. 5)

## Quadratic Discriminant analysis 
  # qda.fit = qda(all_nba ~ MP + GS + DRB  + AST + PER + BLK + PF + TOV + W.L. + WS + Pace + BPM,
  #               data=pred.df,
  #               subset = train)

qda.fit = qda(all_nba ~ GS + MP + ppg + apg + d.rpg + PER + traded +
                bpg + fouls.pg + to.pg + W.L. + BPM + VORP + win.share + true.shoot + 
                three.perc + three.a + ft.perc + fta + fga,
              data=pred.df,
              subset = train)

qda.fit

qda.class = predict(qda.fit, pred.df.2017)$class
table(qda.class, pred.df.2017$Category)
qda.pred = predict(qda.fit, pred.df.2017)

pred.value <- qda.pred$posterior[ ,1]

## Choose only top-15 as there can only be 15
investigate <- cbind(pred.df.2017, pred.value) %>% 
  arrange(pred.value) %>%
  mutate(top.15 = 1,
         top.15 = cumsum(top.15)) %>% 
  filter(top.15<=15 | all_nba==1)

## KNN Classification
library (class)

matrix = as.matrix(pred.df$MP, pred.df$GS, pred.df$ppg, pred.df$apg, pred.df$PER, pred.df$d.rpg, pred.df$bpg, pred.df$fouls.pg, pred.df$to.pg, pred.df$W.L., 
                pred.df$win.share, pred.df$BPM,
                pred.df$true.shoot, pred.df$VORP,
                pred.df$three.perc, pred.df$three.a,
                pred.df$ft.perc, pred.df$fga, pred.df$fta)

dim(matrix)

matrix.standardized = scale(matrix)

var(matrix.standardized[,1])

train.X = matrix.standardized[train,]

test.X = matrix.standardized[!train,]

train.all_nba = pred.df$all_nba[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.all_nba, k=10)
table(knn.pred, pred.df.2017$Category)


## Cross Validation Techniques 
set.seed(1)
library(boot)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

# 10-Fold Validation
# Logistic
  glm.fit = glm(all_nba ~ GS + MP + ppg + apg + d.rpg + PER +
                  bpg + fouls.pg + to.pg + W.L. + BPM + VORP + win.share + true.shoot + 
                  three.perc + three.a + ft.perc + fta + fga,
              data=pred.df, 
              family = binomial)
  
  logistic_error = cv.glm(pred.df, glm.fit, cost=cost, K=10)$delta[1]
  
  logistic_error

# KNN
library(caret)
# kNN requires variables to be normalized or scaled. caret provides facility to preprocess data. 
# I am going to choose centring and scaling
train.X <- pred.df %>% 
  select(GS, MP, ppg, apg, d.rpg, PER, traded,
           bpg, fouls.pg, to.pg, W.L., BPM, VORP, win.share, true.shoot,
           three.perc, three.a, ft.perc, fta, fga)

preProcValues <- preProcess(x = train.X,
                            method = c("center", "scale"))

set.seed(400)
ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(pred.df$all_nba ~ ., data = train.X, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
