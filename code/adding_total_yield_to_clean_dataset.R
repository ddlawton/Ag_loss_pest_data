#####
# Joining the data together
#
######

rm(list=ls())

library(tidyverse)
library(readxl)

names(dat2)
dat <- read_excel("data/raw/CottonLosses_Master_3-3-22.xlsx") %>%
  select(year,Pest,`% Acres Infested`,`% Acres Treated`,location)

dat2 <- read_excel("data/raw/CottonLosses_Master_3-3-22.xlsx",sheet=2,skip=1) %>%
  select(Year, State,`Total Acres (Upland)`) %>%
  mutate(location = tolower(State))

library(fuzzyjoin)
test<-stringdist_left_join(dat, dat2,
                      by ="location",method='lv')
unique(factor(dat2$location))
test %>% filter(State == "Texas Area 3")

summary(dat)
fuzzy_join(
  dat,
  dat2,
  by = location,
  mode = "left")

summary(dat2)

view(test %>%
  group_by(location.x,location.y) %>%
  summarise(n=n()))
