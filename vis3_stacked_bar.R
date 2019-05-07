library(tidyverse)
library(colorspace)

df_app <- read_csv('data/factstablea10.csv')
df_mat <- read_csv('data/factstablea11.csv')

# Perc of applicants by state and race
p3_bar <- df_app %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=app,
         -c(Region, state)) %>% 
  group_by(state) %>% 
  mutate(total_app=sum(app),
         perc_app=app/total_app) %>% 
  ungroup() %>% 
  mutate(state=factor(state),
         state=factor(state,
                      levels=rev(levels(state)))) %>% 
  ggplot() +
  geom_bar(aes(x=state,
               y=perc_app,
               fill=race),
           stat='identity') +
  coord_flip() +
  scale_fill_discrete_qualitative(palette='Dynamic') +
  theme_eric() +
  ggtitle(label='Share of medical school applicants by race/ethnicity',
          subtitle='2018-2019') +
  labs(fill='Race/Ethnicity') +
  xlab('') + ylab('Share of applicants')
p3_bar
ggsave('vis3_bar.svg', plot=p3_bar,
       device='svg', path='assets',
       width=20, height=16)

# Maybe switch to matriculants?
# Perc of matriculants by state and race
df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=mat,
         -c(Region, state)) %>% 
  group_by(state) %>% 
  mutate(total_mat=sum(mat),
         perc_mat=mat/total_mat) %>% 
  ggplot() +
  geom_bar(aes(x=state,
               y=perc_mat,
               fill=race),
           stat='identity') +
  coord_flip()

# TODO decide on best metric,
# is applicant or matriculant used in other assets?

# Scratch regions are not visually interesting
# Perc of applicants by region and race
df_app %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=app,
         -c(Region, state)) %>% 
  group_by(Region) %>% 
  mutate(total_app=sum(app),
         perc_app=app/total_app) %>% 
  ggplot() +
  geom_bar(aes(x=Region,
               y=perc_app,
               fill=race),
           stat='identity') +
  coord_flip()

# Perc of matriculants by region and race
df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=mat,
         -c(Region, state)) %>% 
  group_by(Region) %>% 
  mutate(total_mat=sum(mat),
         perc_mat=mat/total_mat) %>% 
  ggplot() +
  geom_bar(aes(x=Region,
               y=perc_mat,
               fill=race),
           stat='identity') +
  coord_flip()

