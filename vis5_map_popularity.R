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

# Popularity, not normalized for other
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

# Normalize by population?
df_enr <- read_csv('data/factstableb1-2.csv')
df_enr_total <- df_enr %>% 
  group_by(State) %>% 
  mutate(total=sum(All)) %>% 
  ungroup() %>% 
  select(State, total) %>% 
  unique() %>% 
  rbind(data.frame(list(State='AK', total=0))) %>% 
  rbind(data.frame(list(State='WY', total=0))) %>% 
  rbind(data.frame(list(State='ID', total=0))) %>% 
  rbind(data.frame(list(State='MT', total=0))) %>% 
  rbind(data.frame(list(State='DE', total=0))) %>% 
  rbind(data.frame(list(State='ME', total=0))) %>% 
  filter(State!='Total Enrollment')
get_name <- function (state_abb) {
  if (state_abb=='PR') {
    return('Puerto Rico')
  } else if (state_abb=='DC') {
    return('District of Columbia')
  } else {
    return(state.name[which(state.abb==state_abb)])
  }
}
df_enr_total$region <- df_enr_total$State %>%
  lapply(get_name) %>% unlist()
df_pop_sb_norm <- merge(df_pop_sb,
                        df_enr_total,
                        by=c('region')) %>% 
  mutate(pop_norm=ifelse(total!=0,
                         popularity/total*100,
                         0))
df_pop_sb_norm <- df_pop_sb_norm %>% 
  select(state=region, pop_norm)

# Normalized for student population
p5_sb <- statebins_continuous(df_pop_sb_norm,
                     state_col='state',
                     text_color='Black',
                     value_col='pop_norm',
                     brewer_pal='Blues',
                     font_size=8,
                     legend_title='Measure of popularity',
                     legend_position='bottom')
p5_sb_note <- ggdraw(add_sub(p5_sb,
               'Popularity is measured as the percentage of medical students planning to practice in a state,\n normalized by the total medical student population for that state\n
               Legend goes from 0.00 to 0.25 in increments of 0.05'))
ggsave('vis5_sb.svg', plot=p5_sb_note,
       device='svg', path='assets',
       width=20, height=16)