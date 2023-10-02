library(tidyverse)
library(nflfastR)
library(nflplotR)
library(ggimage)
library(nflreadr)
library(ggimage)
pbp<-load_pbp(2022)

second_downs<-pbp|>
  filter(down== 1, !is.na(play_type))

second_long<- second_downs|>
  filter(pass== 1 |rush== 1, ydstogo>=7)|>
  filter(!is.na(epa))|>
  group_by(posteam)|>
  summarize(
    plays=n(),
    epa_play= mean(epa),
    pass_attempts= sum(complete_pass+incomplete_pass, na.rm= TRUE))|>
      mutate(pass_rate= pass_attempts/ plays)|>
      left_join(teams_colors_logos, by= c("posteam"= "team_abbr"))

second_long|>
  ggplot(aes(pass_rate, epa_play))+
  geom_image(aes(image= team_logo_espn), size=.05, asp= 16/9)+
  theme_classic()+
  geom_vline(xintercept = mean(second_long$pass_rate), linetype= "dashed")+
  geom_hline(yintercept= mean(second_long$epa_play), linetype= "dashed")+
  labs(
    x= "Pass Rate",
    y= "EPA Per Play",
    title= "EPA Per Play on Second Downs of more then 7 Yards",
    subtitle= "2022 Regular Season")
ggsave("second-Long.png", width= 14, height= 10, dpi= "retina")
