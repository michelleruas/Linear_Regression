library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#When showing relationship between 2 variables- normally should use a scatter plot
#Plot home runs agains runs per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Plot shows very strong positive correlation

#Relationship betweens stolen bases and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
#Relationship is not very clear

#Relationship between base on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)
#Strong positive correlation, not as strong as home runs

#Due to Confounding- home runs cause both more overall runs and base on balls


#Relationship between fielding errors per dame v runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game = E/G, R_per_game = R/G) %>%
  ggplot(aes(E_per_game, R_per_game)) +
  geom_point(alpha = 0.5)


#Relationship between at bats per dame v runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Relationship between at triples vs doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) +
  geom_point(alpha = 0.5)

#Correlation coefficient between no. runs and no. bats
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarise(cor(AB_per_game, R_per_game))

#Correlation coefficient between win rate and no. of errors
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  summarise(cor(W_per_game, E_per_game))

#Correlation coefficient between triples vs doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  summarise(cor(X3B_per_game, X2B_per_game))







