library(HistData)
data("GaltonFamilies")
GaltonFamilies

#Getting father's height and first son's height
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% 
  summarize(mean(father), sd(father), mean(son), sd(son))

#Plot father's height against son's
galton_heights %>% 
  ggplot(aes(father,son)) +
  geom_point(alpha = 0.5)

#Correlation coefficient = regression value - how two variables interact with eachother

#Correlation between father and son: 0.5
galton_heights %>% summarise(cor(father,son))


set.seed(0)

#Random sampling
R <- sample_n(galton_heights, 25, replace = TRUE) %>% 
  summarise(cor(father, son))

B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarise(r = cor(father, son)) %>% .$r
})

data.frame(R) %>%
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color = "black")

#To guess the height of a sun we would guess 70.5 as this is the expected value
#However, if we know the height of the father is 72.1 then we do stratifying, finding the conditional average for the father's height
#Computing the average son height conditioned on the father being 72 inches tall

conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarise(avg = mean(son)) %>% .$avg

conditional_avg

#Plotting distribution in each group
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()
  

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

#Bivariate normal distribution- shape of points- oval or circle

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

average(female_heights$mother)
  
  
  
  
  
  
  
  


