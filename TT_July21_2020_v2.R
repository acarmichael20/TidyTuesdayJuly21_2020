library(tidyverse)
library(readr)
library(scales)


animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
#animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')
#brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')

animal_outcomes <- animal_outcomes %>%
  pivot_longer(names_to ='location',
               values_to = 'number',
               values_drop_na = TRUE,
               cols = -c(year, animal_type, outcome))


# look at dogs dealt with per year by outcome
x <- animal_outcomes_edit %>%
  group_by(year, outcome, animal_type) %>%
  filter((animal_type %in% c('Dogs', 'Cats')), (outcome %in% c('Euthanized', 'Reclaimed','Rehomed'))) %>%
  summarise(num_animals = sum(number), pop = max(pop))


ggplot(data = x, aes(x = year, y =num_animals, fill = outcome))+
  geom_col()+
  facet_wrap(~animal_type) +
  labs(title = 'Selected RSPCA Outcomes When Dealing With Cats and Dogs', x = 'Year', y = 'Number of Interactions')+
  scale_y_continuous(labels = comma) +
  scale_fill_discrete(name  ="Outcome")+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title.align = 0.5,
        ) 



