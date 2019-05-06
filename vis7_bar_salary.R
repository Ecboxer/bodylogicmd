library(tidyverse)

df_salary <- read_csv('data/salary_school.csv')
df_salary %>% colnames()

# Highest salaried professors
df_salary %>% 
  arrange(desc(Professors)) %>% 
  head(10) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(Institution, Professors),
               y=Professors),
           stat='identity') +
  coord_flip()

# Lowest salaried professors
df_salary %>% 
  arrange(Professors) %>% 
  head(10) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(Institution, Professors),
               y=Professors),
           stat='identity') +
  coord_flip()

df_spec <- read_csv('data/salary_specialty_md_2018.csv')

# Highest salaried md professors
df_spec %>% 
  arrange(desc(Median)) %>% 
  filter(!grepl('Total',Department)) %>% 
  head(10) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(Department, Median),
               y=Median),
           stat='identity') +
  coord_flip()

# Highest salaried md professors
df_spec %>% 
  arrange(Median) %>% 
  filter(!grepl('Total',Department)) %>% 
  head(10) %>%  
  ggplot() +
  geom_bar(aes(x=reorder(Department, Median),
               y=Median),
           stat='identity') +
  coord_flip()

# TODO neaten