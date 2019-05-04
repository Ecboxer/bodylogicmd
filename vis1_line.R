library(tidyverse)

df_app <- read_csv('data/factstablea3.csv')
df_mat <- read_csv('data/factstablea4.csv')

# Number of applicants by region
df_app %>%
  filter(`State of Legal Residence`=='Total for the Region') %>%
  select(-'State of Legal Residence',
         -'% Change') %>% 
  gather(key=year, value=count, -c('Region')) %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=Region, color=Region))

# Number of applicants by state
df_app %>%
  filter(`State of Legal Residence`!='Total for the Region') %>% 
  select(-'% Change') %>% 
  gather(key=year, value=count,
         -c('Region', 'State of Legal Residence')) %>% 
  rename(State='State of Legal Residence') %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=State, color=State))

# Number of matriculants by region
df_mat %>%
  filter(`State of Legal Residence`=='Total for the Region') %>%
  select(-'State of Legal Residence',
         -'% Change') %>% 
  gather(key=year, value=count, -c('Region')) %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=Region, color=Region))

# Number of applicants by state
df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>% 
  select(-'% Change') %>% 
  gather(key=year, value=count,
         -c('Region', 'State of Legal Residence')) %>% 
  rename(State='State of Legal Residence') %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=State, color=State))

# TODO include non-states
# Decide on best metric
# Neaten
df_mat %>% tail()