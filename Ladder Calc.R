#AFL LADDER

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

AFLdata <- na.omit(AFLdata1) 

AFLdata <- cbind("Home_Points" = 0, "Away_Points" = 0, AFLdata)

##################### AFL LADDER #######################

Ladder <- tibble("Team" = unique(AFLdata$home.team.name), 
                     "PF" = 0,
                     "PA" = 0,
                     "Games_Won" = 0,
                     "Games_Tied" = 0, 
                     "Percentage" = 0,
                     "Points" = 0,
                     "Played" = 0)
L = 2024

for (i in 1:L){
  home_team <- AFLdata$home.team.name[i]
  away_team <- AFLdata$away.team.name[i] 
  
  #Matches played increase by 1
  Ladder$Played[match(home_team, Ladder$Team)] <- Ladder$Played[match(home_team, Ladder$Team)] + 1
  Ladder$Played[match(away_team, Ladder$Team)] <- Ladder$Played[match(away_team, Ladder$Team)] + 1
  #Points for and against the home team
  
  #Adding the score of each team to their 'for'
  Ladder$PF[match(home_team, Ladder$Team)] <-  Ladder$PF[match(home_team, Ladder$Team)] + AFLdata$home.score.totalScore[i]
  Ladder$PF[match(away_team, Ladder$Team)] <-  Ladder$PF[match(away_team, Ladder$Team)] + AFLdata$away.score.totalScore[i]
  
  #Adding the score of each team to their 'against'
  Ladder$PA[match(home_team, Ladder$Team)] <-  Ladder$PA[match(home_team, Ladder$Team)] + AFLdata$away.score.totalScore[i]
  Ladder$PA[match(away_team, Ladder$Team)] <-  Ladder$PA[match(away_team, Ladder$Team)] + AFLdata$home.score.totalScore[i]

  Ladder$Percentage[match(home_team, Ladder$Team)] <- 
     (Ladder$PF[match(home_team, Ladder$Team)])/(Ladder$PA[match(home_team, Ladder$Team)])
  Ladder$Percentage[match(away_team, Ladder$Team)] <-
     (Ladder$PF[match(away_team, Ladder$Team)])/(Ladder$PA[match(away_team, Ladder$Team)])
  
  
  if(AFLdata$margin[i]>0){
    #Home Team Wins
    Ladder$Games_Won[match(home_team, Ladder$Team)] <- (Ladder$Games_Won[match(home_team, Ladder$Team)] +1)
    Ladder$Points[match(home_team, Ladder$Team)] <- Ladder$Points[match(home_team, Ladder$Team)] + 4
    
    Home_Points <- Ladder$Points[match(home_team, Ladder$Team)]
    Away_Points <- Ladder$Points[match(away_team, Ladder$Team)]
    
    AFLdata$Home_Points[i] <- Home_Points
    AFLdata$Away_Points[i] <- Away_Points
  }
  
  else if(AFLdata$margin[i]<0){
    #Away team wins
    Ladder$Games_Won[match(away_team, Ladder$Team)] <- (Ladder$Games_Won[match(away_team, Ladder$Team)] +1)
    Ladder$Points[match(away_team, Ladder$Team)] <- Ladder$Points[match(away_team, Ladder$Team)] + 4
  
    Home_Points <- Ladder$Points[match(home_team, Ladder$Team)]
    Away_Points <- Ladder$Points[match(away_team, Ladder$Team)]
    
    AFLdata$Home_Points[i] <- Home_Points
    AFLdata$Away_Points[i] <- Away_Points
  }
  else if(AFLdata$margin[i]==0){
    #Tie
    Ladder$Games_Tied[match(away_team, Ladder$Team)] <- (Ladder$Games_Tied[match(away_team, Ladder$Team)] +1)
    Ladder$Games_Tied[match(home_team, Ladder$Team)] <- (Ladder$Games_Tied[match(home_team, Ladder$Team)] +1)
    Ladder$Points[match(away_team, Ladder$Team)] <- Ladder$Points[match(away_team, Ladder$Team)] + 2
    Ladder$Points[match(home_team, Ladder$Team)] <- Ladder$Points[match(home_team, Ladder$Team)] + 2
  
    Home_Points <- Ladder$Points[match(home_team, Ladder$Team)]
    Away_Points <- Ladder$Points[match(away_team, Ladder$Team)]
    
    AFLdata$Home_Points[i] <- Home_Points
    AFLdata$Away_Points[i] <- Away_Points
    }

}


teams <- unique(AFLdata$home.team.name)
for(i in 1:length(teams)) {
  Team <- teams[i]
  temp <- AFLdata %>% subset(home.team.name == Team | away.team.name == Team)
  Temp_Elo <-0
  
  for (j in 1:nrow(temp)) {
    if(temp$home.team.name[j] == Team)
    {Temp_Elo[j] = temp$Home_Points[j]}
    
    
    if (temp$away.team.name[j] == Team)
    {
      Temp_Elo[j] = temp$Away_Points[j]
    }
    nam<- paste(Team)
    assign(nam,Temp_Elo)
    
  }
 
  print(i)
  
}

#Plotting a cumulative ladder of the 5 teams
p <- ggplot()+geom_line(aes(1:length(`Geelong Cats`),`Geelong Cats`,color = "Geelong Cats")) +
  geom_line(aes(1:length(`Gold Coast Suns`),`Gold Coast Suns`,color = "Gold Coast Suns")) +
  geom_line(aes(1:length(Hawthorn),Hawthorn,color = "Hawthorn")) +
  geom_line(aes(1:length(Melbourne),Melbourne,color = "Melbourne")) +
  geom_line(aes(1:length(`Port Adelaide`),`Port Adelaide`,color = "Port Adelaide")) 

p <- p +theme_bw() + labs(x = "Games Played since 2012", y = "Rating")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


