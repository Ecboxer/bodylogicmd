library(tidyverse)
library(maps)
library(mapproj)
library(ggthemes)
library(scales)
library(statebins)

df_pop <- read_csv('data/state_popularity_poll.csv')
us_states <- ggplot2::map_data('state')

# Popularity, including undecided
df_pop_2018 <- df_pop %>%
  filter(state!='Number of respondents') %>% 
  select(region=state,
         popularity='2018') %>% 
  mutate(region=tolower(region))

# Popularity, normalized for undecided and non-states
df_pop_2018_norm <- df_pop %>%
  filter(state!='Number of respondents') %>% 
  select(region=state,
         popularity='2018') %>% 
  mutate(region=tolower(region)) %>% 
  filter(!region%in%c('undecided',
                      'canada',
                      'u.s. territory/possession',
                      'other foreign country',
                      'puerto rico'))

# Write state and popularity df
us_states_pop <- left_join(us_states,
                           df_pop_2018,
                           by=c('region'='region'))
us_states_pop_norm <- left_join(us_states,
                           df_pop_2018_norm,
                           by=c('region'='region'))

# Popularity, unnormalized
ggplot(us_states_pop,
             aes(x=long, y=lat,
                 group=group, fill=popularity)) +
  geom_polygon(color='gray90', size=0.1) +
  coord_map(projection='albers',
            lat0=39, lat1=45) +
  scale_fill_gradient(low='red',
                         high='yellow') +
  labs(fill='Percentage') +
  theme_map()

# Popularity, normalized
# Not a big difference
ggplot(us_states_pop_norm,
       aes(x=long, y=lat,
           group=group, fill=popularity)) +
  geom_polygon(color='gray90', size=0.1) +
  coord_map(projection='albers',
            lat0=39, lat1=45) +
  scale_fill_gradient(low='red',
                      high='yellow') +
  labs(fill='Percentage') +
  theme_map()

# Statebins
# Data
df_pop_sb <- df_pop %>%
  filter(state!='Number of respondents') %>% 
  select(region=state,
         popularity='2018')

statebins_continuous(df_pop_sb,
                     state_col='region',
                     text_color='black',
                     value_col='popularity',
                     brewer_pal='OrRd',
                     font_size=5,
                     legend_title='Percentage')

# TODO neaten
# Normalize by population?