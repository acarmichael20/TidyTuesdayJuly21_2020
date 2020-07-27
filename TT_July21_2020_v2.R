library(tidyverse)
library(readr)
library(scales)


animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
#animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
#brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')


# Select a dataset that contains only cats and dogs 
# Limit outcomes to the big three.

CatsDogs <- animal_outcomes %>%
  filter(animal_type %in% c('Cats', 'Dogs'), outcome %in% c('Euthanized', 'Reclaimed','Rehomed')) %>% 
  select(year, animal_type, outcome, Total)


# plot CatsDogs data
ggplot(data = CatsDogs, aes(x = year, y = Total, fill = outcome))+
  geom_col()+
  facet_wrap(~animal_type)+
  labs(title = 'Selected RSPCA Outcomes With Cats and Dogs', x = 'Year', y = 'Number of Animals')+
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name  ="Outcome")+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align = 0.5,
  )
