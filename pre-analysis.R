
### defining the years with enoght number of ratings for the analysis

# Required libs.
library(dslabs)
library(tidyverse)

movielens <- data("movielens") %>%
save(movielens, file = 'rdas/movielens.rda')

# The total number of ratings against year.

load('rdas/movielens.rda')
movielens %>% group_by(year) %>% summarise(n = n()) %>%
  ggplot(aes(year, n), scale_y_sqrt()) + geom_point() +
  ggtitle('Total number of ratings received by movies versus their first screening year') +
  xlab("The first screening year") +
  ylab('Total number of ratings') +
  ggsave('fig/ratings-per-year.png')
