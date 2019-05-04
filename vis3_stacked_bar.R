library(tidyverse)

df_app <- read_csv('data/factstablea10.csv')
df_mat <- read_csv('data/factstablea11.csv')

# Perc of applicants by state and race
df_app %>%
  filter(`State of Legal Residence`!='Total for the Region') %>%
  select(-Total) %>% 
  rename(state=`State of Legal Residence`) %>% 
  gather(key=race,
         value=app,
         -c(Region, state)) %>% 
  group_by(state) %>% 
  mutate(total_app=sum(app),
         perc_app=app/total_app) %>% 
  ggplot() +
  geom_bar(aes(x=state,
               y=perc_app,
               fill=race),
           stat='identity') +
  coord_flip()

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

# Perc of matriculants by region and race
df_app %>%
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

# TODO decide on best metric
# Neaten 
# Use perc or total? Potentially misleading for small
# state with diverse applicants, eg Hawaii