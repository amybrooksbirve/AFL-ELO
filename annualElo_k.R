library(fitzRoy)
library(dplyr)
library(elo)
library(lubridate)
library(frs)       
library(Cairo)
library(ggrepel)
library(lubridate)
library(scales)
library(ggplot2)

#######################INSTALLING THE DATA###########################
data2012 <- fetch_fixture(2012, comp = "AFLM")
dataset2012 <- subset(data2012, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2012 <- tibble("Year" = 2012, "margin" = dataset2012$home.score.totalScore-dataset2012$away.score.totalScore,dataset2012)

data2013 <- fetch_fixture(2013, comp = "AFLM")
dataset2013 <- subset(data2013, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2013 <- tibble("Year" = 2013, "margin" = dataset2013$home.score.totalScore-dataset2013$away.score.totalScore,dataset2013)

data2014 <- fetch_fixture(2014, comp = "AFLM")
dataset2014 <- subset(data2014, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2014 <- tibble("Year" = 2014, "margin" = dataset2014$home.score.totalScore-dataset2014$away.score.totalScore,dataset2014)

data2015 <- fetch_fixture(2015, comp = "AFLM")
dataset2015 <- subset(data2015, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2015 <- tibble("Year" = 2015, "margin" = dataset2015$home.score.totalScore-dataset2015$away.score.totalScore,dataset2015)

data2016 <- fetch_fixture(2016, comp = "AFLM")
dataset2016 <- subset(data2016, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2016 <- tibble("Year" = 2016, "margin" = dataset2016$home.score.totalScore-dataset2016$away.score.totalScore,dataset2016)

data2017 <- fetch_fixture(2017, comp = "AFLM")
dataset2017 <- subset(data2017, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2017 <- tibble("Year" = 2017, "margin" = dataset2017$home.score.totalScore-dataset2017$away.score.totalScore,dataset2017)

data2018 <- fetch_fixture(2018, comp = "AFLM")
dataset2018 <- subset(data2018, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2018 <- tibble("Year" = 2018, "margin" = dataset2018$home.score.totalScore-dataset2018$away.score.totalScore,dataset2018)

data2019 <- fetch_fixture(2019, comp = "AFLM")
dataset2019 <- subset(data2019, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2019 <- tibble("Year" = 2019,"margin" = dataset2019$home.score.totalScore-dataset2019$away.score.totalScore, dataset2019)

data2020 <- fetch_fixture(2020, comp = "AFLM")
dataset2020 <- subset(data2020, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))
dataset2020 <- tibble("Year" = 2020, "margin" = dataset2020$home.score.totalScore-dataset2020$away.score.totalScore, dataset2020)

data2021 <- fetch_fixture(2021, comp = "AFLM")
dataset2021 <- subset(data2021, select = c(round.roundNumber,home.team.id, home.team.name, home.score.goals, 
                                           home.score.behinds, home.score.totalScore, away.team.id,
                                           away.team.name, away.score.goals, away.score.behinds, away.score.totalScore, venue.id,
                                           venue.name))

dataset2021 <- tibble("Year" = 2021, "margin" = dataset2021$home.score.totalScore-dataset2021$away.score.totalScore, dataset2021)

AFLdata1 <- data.frame(rbind(dataset2012, dataset2013, dataset2014, dataset2015, 
                             dataset2016, dataset2017, dataset2018, dataset2019, 
                             dataset2020, dataset2021))

AFLdata3 <- na.omit(AFLdata1) 

AFLdata3 <- cbind("Home_Rating" = 0, "Away_Rating" = 0, "Performance_Measure" = 0, 
                 AFLdata3, "Game" = 1:2024)
####################### CREATING THE ELO CALCULATOR ###########################

#Creating a tibble for the Elo Scores to be calculated. 
Elo_Rating1 <- tibble(
  "Team" = unique(AFLdata3$home.team.name),
  "Rating" = 1500,
  "Num_Wins" = 0,
  "Played" = 0
)

K = 5 #10,15,20,33
P = 400
L <- 2024

#For the overall ELo..
for (i in 1:2024) {
  #Identifying the home and away team
  home_team <- AFLdata3$home.team.name[i]
  away_team <- AFLdata3$away.team.name[i]
  
  #Matching the Elo in the ratings list with the corresponding team
  Home_Elo <- Elo_Rating1$Rating[match(home_team, Elo_Rating1$Team)]
  Away_Elo <- Elo_Rating1$Rating[match(away_team, Elo_Rating1$Team)]
  
  #Matches played increase by 1
  Elo_Rating1$Played[match(home_team, Elo_Rating1$Team)] <-
    (Elo_Rating1$Played[match(home_team, Elo_Rating1$Team)] + 1)
  Elo_Rating1$Played[match(away_team, Elo_Rating1$Team)] <-
    (Elo_Rating1$Played[match(away_team, Elo_Rating1$Team)] + 1)
  
  HomeW = 1 / (1 + 10 ^ ((Away_Elo - Home_Elo) / P))
  AwayW = 1 - HomeW
  
  #Home team wins
  if (AFLdata3$margin[i] > 0) {
    # HOME WIN
    #Add a win to the home team win tally
    Elo_Rating1$Num_Wins[match(home_team, Elo_Rating1$Team)] <-
      (Elo_Rating1$Num_Wins[match(home_team, Elo_Rating1$Team)] + 1)
    
    #Amending their Elo
    Home_Elo = Home_Elo + K * (1 - HomeW)
    Away_Elo = Away_Elo + K * (0 - AwayW)
    
    #Adding their Elo
    Elo_Rating1$Rating[match(home_team, Elo_Rating1$Team)] <- Home_Elo
    Elo_Rating1$Rating[match(away_team, Elo_Rating1$Team)] <- Away_Elo
    
    AFLdata3$Home_Rating[i] <- Home_Elo
    AFLdata3$Away_Rating[i] <- Away_Elo
    
  } else if (AFLdata3$margin[i] < 0) {
    # AWAY WIN
    #Add a win to away team win tally
    Elo_Rating1$Num_Wins[match(away_team, Elo_Rating1$Team)] <-
      (Elo_Rating1$Num_Wins[match(away_team, Elo_Rating1$Team)] + 1)
    
    #Amending their Elo
    Home_Elo = Home_Elo + K * (0 - HomeW)
    Away_Elo = Away_Elo + K * (1 - AwayW)
    
    #Adding their Elo
    Elo_Rating1$Rating[match(home_team, Elo_Rating1$Team)] <- Home_Elo
    Elo_Rating1$Rating[match(away_team, Elo_Rating1$Team)] <- Away_Elo
    
    AFLdata3$Home_Rating[i] <- Home_Elo
    AFLdata3$Away_Rating[i] <- Away_Elo
  }
  
  else
    (AFLdata3$margin[i] == 0)
  # A TIE
  
  Home_Elo = Home_Elo + K*(0.5-HomeW)
  Away_Elo = Away_Elo + K*(0.5-AwayW)
  
  #Adding their Elo
  Elo_Rating1$Rating[match(home_team, Elo_Rating1$Team)] <- Home_Elo
  Elo_Rating1$Rating[match(away_team, Elo_Rating1$Team)] <- Away_Elo
  
  AFLdata3$Home_Rating[i] <- Home_Elo
  AFLdata3$Away_Rating[i] <- Away_Elo
  
}

##Plotting the teams
teams <- unique(AFLdata3$home.team.name)
for (i in 1:length(teams)) {
  Team <- teams[i]
  temp <-
    AFLdata3 %>% subset(home.team.name == Team | away.team.name == Team)
  Temp_Elo <- 0
  
  for (j in 1:nrow(temp)) {
    if (temp$home.team.name[j] == Team)
    {
      Temp_Elo[j] = temp$Home_Rating[j]
    }
    
    
    if (temp$away.team.name[j] == Team)
    {
      Temp_Elo[j] = temp$Away_Rating[j]
    }
    nam <- paste(Team)
    assign(nam, Temp_Elo)
    
  }
  
  print(i)
  
}

#plotting 
a <-ggplot() + geom_line(aes(1:length(`Geelong Cats`), `Geelong Cats`, color = "Geelong Cats")) 
a <- a + p + labs(x = "Games Played since 2012", y = "Rating")+theme_bw()

#Developing a tibble of all ratings
All_Ratings <- tibble("Team" = unique(AFLdata3$home.team.name))
All_Ratings <-
  rbind(
    "Team" = All_Ratings,
    "Round" = AFLdata3$round.roundNumber,
    "Rating" = 0
  )


## PERFORMANCE MEASURE
games <- matrix(NA, 2024, 4)

games_wins <-
  tibble(
    games,
    "Played" = 0,
    "Home_Team" = 0,
    "Away_Team" = 0,
    "Winner" = 0
  )

for (i in 1:2024) {
  games_wins$Played[i] <- i
  games_wins$Home_Team[i] <- AFLdata3$home.team.id[i]
  games_wins$Away_Team[i] <- AFLdata3$away.team.id[i]
  
  if (AFLdata3$home.score.totalScore[i] > AFLdata3$away.score.totalScore[i]) {
    games_wins$Winner[i] <- AFLdata3$home.team.id[i]
  }
  else {
    games_wins$Winner[i] <- AFLdata3$away.team.id[i]
  }
}

#Row 1 is the ratings they all begin on
rating_tracker <- matrix(-1, 2025, 18)
colnames(rating_tracker) <- c(1:18)
rownames(rating_tracker) <- c(0:2024)


for (i in 1:2025) {
  for (j in 1:18) {
    rating_tracker[1, j] = 1500
  }
}

##Making the Rating tracker for team ratings over time.
for (j in 1:2024) {
  ag = AFLdata3$home.team.id[j]
  bg = AFLdata3$away.team.id[j]
  ar = AFLdata3$Home_Rating[j]
  br = AFLdata3$Away_Rating[j]
  
  rating_tracker[j+1, ag] = ar
  rating_tracker[j+1, bg] = br
  
  for (i in 1:18) {
    if (rating_tracker[j, i] < 0) {
      rating_tracker[j, i] = rating_tracker[j - 1, i]
    }
  }
}

games_wins <- subset(games_wins, select = c(Played, Home_Team, Away_Team, Winner))

#Creating the performance index
for (i in 2:2025){
  if((rating_tracker[i-1,games_wins$Winner[i]])>min(rating_tracker[i-1,games_wins$Home_Team[i]],rating_tracker[i-1,games_wins$Away_Team[i]])){
    AFLdata3$Performance_Measure[i]=1
  }else{ 
    AFLdata3$Performance_Measure[i]=0
  }
}

sum_perf <- sum(AFLdata3$Performance_Measure)
perf_perc <- sum_perf/2024

