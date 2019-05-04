library(tidyverse)
library(networkD3)

df_sex_enr <- read_csv('data/factstableb3.csv')
df_sex_grd <- read_csv('data/factstableb4.csv')
df_race <- read_csv('data/factstableb5-2.csv')

# Create nodes vector
nodes <- c()
nodes <- c(nodes, list('name'='Men'))
nodes <- c(nodes, list('name'='Women'))
races <- df_sex_enr %>%
  select(starts_with('Student')) %>% 
  unique() %>% pull()
races <- races[-c(11,12,13)]
for (race in races) {
  nodes <- c(nodes, list('name'=race))
}
nodes_list <- nodes %>% unlist(use.names=F)

df_nodes <- nodes_list %>%
  as_data_frame()
df_nodes <- df_nodes %>%
  rename(name=value)

# Create links
df_sex_enr_sankey <- df_sex_enr %>%
  select(Sex,
         `Student Race/Ethnicity Responses`,
         `2018-2019`) %>% 
  mutate(source=Sex,
         target=`Student Race/Ethnicity Responses`,
         value=`2018-2019`) %>% 
  select(source, target, value) %>% 
  filter(source!='All') %>% 
  filter(target!='Total for Men') %>% 
  filter(target!='Total for Women')

idx_nodes <- function (node_name) {
  which(nodes_list==node_name) - 1
}
df_sex_enr_sankey$source_idx <- df_sex_enr_sankey$source %>%
  lapply(idx_nodes) %>% unlist()
df_sex_enr_sankey$target %>% 
  lapply(idx_nodes) %>% unlist()
df_sex_enr_sankey$target_idx <- df_sex_enr_sankey$target %>% 
  lapply(idx_nodes) %>% unlist()
df_sex_enr_sankey <- df_sex_enr_sankey %>%
  select(-c('source', 'target'))

links <- jsonlite::toJSON(df_sex_enr_sankey)

# Create sankey network
sankeyNetwork(Links=df_sex_enr_sankey,
              Nodes=df_nodes,
              Source='source_idx',
              Target='target_idx',
              Value='value',
              NodeID='name',
              units='enrollees',
              fontSize=16,
              nodeWidth=30)

# TODO Neaten
# Animation?
# Add state