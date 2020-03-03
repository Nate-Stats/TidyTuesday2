library(ggplot2)
library(tidyverse)
library(dplyr) 
library(tidyr)
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

## Focus on the data we want to use
Top5 <- filter(season_goals, rank <=5)
Top5 <- subset(Top5, select = c(player, season, goals))

##Making dates look better for plotting(19xx-xx to 19xx)
Top5$season <- substr(Top5$season, 0,4)

##Aggregate number of goals for each season 
##(some players played for different teams in one season, this condenses that in to one data point for each season)
Top5 <- Top5 %>%
  group_by(player, season) %>%
  summarise(goals=sum(goals))

##Lets make a line plot
LP <- ggplot(data=Top5, aes(x = season, y = goals, group=player))+
      geom_line( aes(color = player))+
      geom_point(aes(color = player))+
      theme_economist()+
      xlab("Season")+
      ylab("Goals")+
      ggtitle("Goal's Per Season Of The NHL'S Top Five Goal Scorers")+
      theme(axis.text.x = element_text(angle=90),legend.title=element_blank())
LP

##Wow! the consistency between seasons is wild, lets look at box plots.

DP <- ggplot(data=Top5, aes(x = player, y = goals)) +
  geom_boxplot( aes(color= player))+
  theme_economist()+
  xlab(" ")+
  ylab("Goals")+
  labs(" ") +
  ggtitle("NHL's Top 5 Goal Scorers Season to Season Conistancy")+
  theme(legend.title=element_blank())
DP


 