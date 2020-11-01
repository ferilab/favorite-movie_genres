### Finding genres that have best users ratings

# Required libs.
library(tidyverse)
library(lubridate)

load('rdas/movielens.rda')

top25 <- movielens %>% filter(year >= 1970) %>% group_by(movieId) %>% 
  summarise(avg_rate = mean(rating), first_rating_year = min(year(as_datetime(timestamp))), 
            avg_annual_rated = n()/(2017 - first_rating_year)) %>%
  top_n(25, avg_annual_rated)

# We stratify the post 1970 movies by average number of rating per year and compute
# their average rating.

post_1970 <- movielens %>% filter(year >= 1970) %>% group_by(movieId) %>% 
  summarise(avg_rate = mean(rating), first_rating_year = min(year(as_datetime(timestamp))), 
            avg_annual_rated = n()/(2017 - first_rating_year), list=FALSE) %>%
  mutate(rated_strata = round(avg_annual_rated, 0)) 

post_1970 %>%
  ggplot(aes(rated_strata, avg_rate)) + geom_point() + 
  geom_abline(intercept = 3, slope = 0.09, col = 'red') +
  ggtitle('Average rating versus average anual nuumber of rating') +
  xlab('Strata: the average number of annual ratings') +
  ylab('Average rate') +
  ggsave('fig/rating-vs-rating-No.png')


movielens <- movielens %>% mutate(date = as_datetime(timestamp))

# Compute the average rating for each week and plot this average against day.

movielens %>% mutate(week = round_date(date, 'week')) %>% 
  group_by(week) %>% mutate(avg_rate = mean(rating)) %>% 
  select(week, avg_rate) %>% unique() %>%
  ggplot(aes(week, avg_rate)) + geom_point() +
  ggtitle('Average rating versus week') +
  xlab('Rating week') +
  ylab('Average rate') +
  ggsave('fig/rating-vs-week.png')

# movie genres are considered same as whatever combination the genre column represents. 

movielens %>% group_by(genres) %>% 
  summarize(genres, avg_rate = mean(rating), upper = avg_rate + sd(rating)/sqrt(n()), 
            lower = avg_rate - sd(rating)/sqrt(n()))  %>%
  filter(n()>=500) %>% ggplot(aes(x = genres, ymin = lower, ymax = upper)) + 
  geom_errorbar() + geom_point(aes(genres, avg_rate)) + 
  ylab('Average rating of genres with their standard errors') +
  coord_flip() +
  ggsave('fig/rating-vs-genre.png')
