library(tidyverse)

df_app_mat <- read_csv('data/factstablea1.csv')[-1,]
df_enr <- read_csv('data/factstableb1-2.csv')
df_grd <- read_csv('data/factstableb2-2.csv')

df_plot_app <- df_app_mat %>%
  select(State,
         school=`Medical School`,
         app=Applicants,
         perc_app_male=`Perc Applicants Men`,
         perc_app_female=`Perc Applicants Women`) %>% 
  filter(State!='Total') %>% 
  mutate(num_app_male=app*perc_app_male/100,
         num_app_female=app*perc_app_female/100)

# Top 10 schools for women (applicants) (num)
df_plot_app %>% 
  arrange(desc(num_app_female)) %>% 
  head(10) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(school, -num_app_female),
               y=num_app_female),
           stat='identity') +
  coord_flip()

# Top 10 schools for women (applicants) (perc)
df_plot_app %>% 
  arrange(desc(perc_app_female)) %>% 
  head(10) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(school, -perc_app_female),
               y=perc_app_female),
           stat='identity') +
  coord_flip()

# Bottom 10 schools for women (applicants) (num)
df_plot_app %>% 
  arrange(desc(num_app_female)) %>% 
  tail(10) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(school, -num_app_female),
               y=num_app_female),
           stat='identity') +
  coord_flip()

# Bottom 10 schools for women (applicants) (perc)
df_plot_app %>% 
  arrange(desc(perc_app_female)) %>% 
  tail(10) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(school, -perc_app_female),
               y=perc_app_female),
           stat='identity') +
  coord_flip()

# TODO descide on num or perc, num has greater variety but is confounded by school size
# Decide btw app, mat, enr, grd
# Neaten