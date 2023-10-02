library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)
pbp<-load_pbp(2018:2022)

pass_rate<-pbp|>
  filter(!is.na(play_type), !is.na(epa))|>
  mutate(off_snap = ifelse(play_type == c("pass" , "run"), 1, 0))
  



pass_rate|>
  filter(!is.na(off_snap))

pass_rate_log<-glm(pass~ydstogo+yardline_100+wp+down,
               data= pass_rate)
pass_rate|>
  mutate(pass_prob = pass_rate_log$fitted.values)|>
  mutate(pass_oe= pass- pred_prob) 

pass_rate|>
  group_by(play_type)|>
  tally(sort=T)

