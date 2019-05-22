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
# ggsave('vis3_bar.svg', plot=p3_bar,
#        device='svg', path='assets',
#        width=20, height=16)

# Switch x to %
# Perc of matriculants by state and race
p3_bar_mat <- df_mat %>%
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
  coord_flip() +
  scale_fill_discrete_qualitative(palette='Dynamic') +
  theme_eric() +
  ggtitle(label='Race/ethnicity of medical school students by state of legal residence',
          subtitle='2018-2019') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(fill='Race/Ethnicity') +
  xlab('') + ylab('Percentage of students')
p3_bar_mat
# ggsave('vis3_bar_mat.svg', plot=p3_bar_mat,
#        device='svg', path='assets',
#        width=20, height=16)

## TODO
# Revisions
# Group Other as in sankey
race_all <- df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=mat,
         -c(Region, state)) %>% 
  select(race) %>% unique() %>% unlist()
race_keep <- race_all[c(2,3,4,6)]
state_order <- df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=mat,
         -c(Region, state)) %>% 
  select(state) %>% unique() %>% 
  arrange(state) %>% unlist()
p3_bar_mat_other <- df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=mat,
         -c(Region, state)) %>% 
  group_by(state) %>% 
  mutate(total_mat=sum(mat),
         perc_mat=mat/total_mat) %>% 
  ungroup() %>%
  mutate(race=ifelse(race%in%race_keep,
                     race,
                     'Other')) %>% 
  group_by(state, race) %>% 
  summarise(perc_mat_other=sum(perc_mat)) %>% 
  ungroup() %>% 
  mutate(state=factor(state, 
                      levels=rev(state_order))) %>% 
  ggplot() +
  geom_bar(aes(x=state,
               y=perc_mat_other,
               fill=race),
           stat='identity') +
  coord_flip() +
  scale_fill_discrete_qualitative(palette='Dynamic') +
  theme_eric() +
  ggtitle(label='Race/ethnicity of medical school students by state of legal residence',
          subtitle='2018-2019') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  labs(fill='Race/Ethnicity') +
  xlab('') + ylab('Percentage of students')
p3_bar_mat_other
# ggsave('vis3_bar_mat_other.svg',
#        plot=p3_bar_mat_other,
#        device='svg', path='assets',
#        width=20, height=16)

# # Scratch regions are not visually interesting
# # Perc of applicants by region and race
# df_app %>%
#   filter(`State of Legal Residence`!='Total for the Region') %>%
#   select(-Total) %>% 
#   rename(state=`State of Legal Residence`) %>% 
#   gather(key=race,
#          value=app,
#          -c(Region, state)) %>% 
#   group_by(Region) %>% 
#   mutate(total_app=sum(app),
#          perc_app=app/total_app) %>% 
#   ggplot() +
#   geom_bar(aes(x=Region,
#                y=perc_app,
#                fill=race),
#            stat='identity') +
#   coord_flip()
# 
# # Perc of matriculants by region and race
# df_mat %>%
#   filter(`State of Legal Residence`!='Total for the Region') %>%
#   select(-Total) %>% 
#   rename(state=`State of Legal Residence`) %>% 
#   gather(key=race,
#          value=mat,
#          -c(Region, state)) %>% 
#   group_by(Region) %>% 
#   mutate(total_mat=sum(mat),
#          perc_mat=mat/total_mat) %>% 
#   ggplot() +
#   geom_bar(aes(x=Region,
#                y=perc_mat,
#                fill=race),
#            stat='identity') +
#   coord_flip()
