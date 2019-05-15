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

# Write nodes df with states
states <- df_race %>% 
  filter(State!='Total Enrollment') %>% 
  select(State) %>%
  unique() %>% pull()
states
for (state in states) {
  nodes <- c(nodes, list('name'=state))
}
nodes_list_states <- nodes %>% unlist(use.names=F)
df_nodes_states <- nodes_list_states %>% 
  as_data_frame()
df_nodes_states <- df_nodes_states %>%
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
df_sex_enr_sankey %>% head()

idx_nodes <- function (node_name) {
  which(nodes_list_states==node_name) - 1
}
df_sex_enr_sankey$source_idx <- df_sex_enr_sankey$source %>%
  lapply(idx_nodes) %>% unlist()
df_sex_enr_sankey$target_idx <- df_sex_enr_sankey$target %>% 
  lapply(idx_nodes) %>% unlist()
df_sex_enr_sankey <- df_sex_enr_sankey %>%
  select(-c('source', 'target'))

# Write race->state links
cols_race <- colnames(df_race)[-c(1,2)]
df_race_sankey <- df_race %>%
  filter(State!='Total Enrollment') %>% 
  gather(key=race, value=count,
         -c(State, `Medical School`)) %>% 
  filter(race!='Unduplicated\nTotal \nEnrollment') %>%
  group_by(State, race) %>% 
  mutate(state_count=sum(count)) %>% 
  ungroup() %>% 
  select(State, race, state_count) %>% 
  unique() %>%
  mutate(race=str_replace_all(race,
                              "[\r\n]", ''),
         race=str_squish(race))
# Remove multiple races
df_nodes_st2reg <- df_nodes_st2reg[-8,]

df_race_sankey$source_idx <- df_race_sankey$race %>% 
  lapply(idx_nodes) %>% unlist()
df_race_sankey$target_idx <- df_race_sankey$State %>% 
  lapply(idx_nodes) %>% unlist()
df_race_sankey <- df_race_sankey %>% 
  select(-c(State, race)) %>% 
  rename(value=state_count)
df_race_sankey %>% head()
df_sankey_full <- rbind(df_sex_enr_sankey,
                        df_race_sankey)

# Create sex->race sankey network
p2_sankey <- sankeyNetwork(Links=df_sex_enr_sankey,
              Nodes=df_nodes,
              Source='source_idx',
              Target='target_idx',
              Value='value',
              NodeID='name',
              units='enrolled students',
              fontSize=16,
              nodeWidth=30)
p2_sankey

# Remove sex from nodes
df_nodes_states <- df_nodes_states %>% 
  filter(!name%in%c('Men', 'Women'))
# Remove sex from links
df_sankey_states <- df_sankey_full %>% 
  filter(!source_idx%in%c(0,1)) %>% 
  mutate(source_idx=source_idx-2,
         target_idx=target_idx-2)

# Create race->state sankey network
p2_sankey_st <- sankeyNetwork(Links=df_sankey_states,
              Nodes=df_nodes_states,
              Source='source_idx',
              Target='target_idx',
              Value='value',
              NodeID='name',
              units='enrolled students',
              fontSize=16,
              nodeWidth=30)
p2_sankey_st

# Truncate to just race->region
df_nodes_race <- df_nodes_states[seq(1,10),]
df_nodes_region <- data.frame(name=c('Northeast',
                  'Central',
                  'South',
                  'West'))
df_nodes_st2reg <- rbind(df_nodes_race,
                         df_nodes_region)
ne_list <- c('CT','DE','DC','ME','MD','MA','NH',
             'NJ','NY','PA','RI','VT')
c_list <- c('IL','IN','IA','KS','MI','MN','MO',
            'NE','ND','OH','SD','WI')
s_list <- c('AL','AR','FL','GA','KY','LA','MS',
            'NC','OK','PR','SC','TN','TX','VA','WV')
w_list <- c('AK','AZ','CA','CO','HI','ID','MT',
            'NV','NM','OR','UT','WA','WY')
idx_nodes <- function (node_name) {
  if (node_name%in%ne_list) {
    return('Northeast')
  } else if (node_name%in%c_list) {
    return('Central')
  } else if (node_name%in%s_list) {
    return('South')
  } else if (node_name%in%w_list) {
    return('West')
  }
}
df_race_reg_sankey <- df_race %>%
  filter(State!='Total Enrollment') %>% 
  gather(key=race, value=count,
         -c(State, `Medical School`)) %>%
  filter(race!='Unduplicated\nTotal \nEnrollment') %>% 
  group_by(State, race) %>% 
  mutate(state_count=sum(count)) %>% 
  ungroup() %>% 
  select(State, race, state_count) %>% 
  unique() %>% 
  mutate(race=str_replace_all(race,
                              "[\r\n]", ''),
         race=str_squish(race))
df_race_reg_sankey$Region <- df_race_reg_sankey$State %>% 
  lapply(idx_nodes) %>% unlist()

idx_nodes <- function (node_name) {
  which(df_nodes_st2reg==node_name) - 1
}
df_race_reg_sankey$source_idx <- df_race_reg_sankey$race %>% 
  lapply(idx_nodes) %>% unlist()
df_race_reg_sankey$target_idx <- df_race_reg_sankey$Region %>% 
  lapply(idx_nodes) %>% unlist()
df_race_reg_sankey <- df_race_reg_sankey %>% 
  select(value=state_count,
         source_idx, target_idx)

df_race_reg_sankey <- df_race_reg_sankey %>% 
  group_by(target_idx, source_idx) %>% 
  mutate(value_reg=sum(value)) %>% 
  select(-value) %>% unique()

# Create race->region sankey network
p2_sankey_reg <- sankeyNetwork(Links=df_race_reg_sankey,
                              Nodes=df_nodes_st2reg,
                              Source='source_idx',
                              Target='target_idx',
                              Value='value_reg',
                              NodeID='name',
                              units='enrolled students',
                              fontSize=16,
                              nodeWidth=30)
p2_sankey_reg

# Sex -> Race
df_sex_enr_sankey %>%
  select(value) %>% sum()

# Race -> Region
df_race_reg_sankey %>% 
  select(value_reg) %>% sum()

# Combined Sex -> Race -> Region
df_race_reg_sankey <- df_race_reg_sankey %>% 
  select(value=value_reg,
         source_idx,
         target_idx)

# Write full nodes dataframe
df_temp <- as_data_frame(c('Men','Women')) %>% 
  select(name=value)
df_nodes_full <- rbind(df_temp,
      df_nodes_st2reg)

# Shift Race -> Region indices by two
df_race_reg_shift <- df_race_reg_sankey %>% 
  ungroup() %>% 
  mutate(source_idx=source_idx+2,
         target_idx=target_idx+2)

df_sankey_full <- rbind(df_sex_enr_sankey, 
      df_race_reg_shift)

# Create Sex->Race->Region sankey network
p2_sankey_full <- sankeyNetwork(Links=df_sankey_full,
                               Nodes=df_nodes_full,
                               Source='source_idx',
                               Target='target_idx',
                               Value='value',
                               NodeID='name',
                               units='enrolled students',
                               fontSize=16,
                               nodeWidth=30)
p2_sankey_full

# Group races apart from White, Hispanic/Latino, 
# Black/African-American, Asian into Other
other_group <- c(3,7,10,11,12)
df_nodes_full
df_nodes_other <- df_nodes_full[-other_group,]
df_nodes_other
# Move values in other_group to Other idx 7
df_nodes_full[1,] %>% as.character()
full2other <- function(idx) {
  return(as.character(df_nodes_full[idx+1,]))
}
df_sankey_full$source_name <- df_sankey_full$source_idx %>% 
  lapply(full2other) %>% unlist()
df_sankey_full$target_name <- df_sankey_full$target_idx %>% 
  lapply(full2other) %>% unlist()

other2idx <- function(other_name) {
  if (other_name%in%df_nodes_other$name) {
    return(which(df_nodes_other==other_name))
  } else {
    return(7)
  }
}
df_sankey_full$source_idx_other <- df_sankey_full$source_name %>% 
  lapply(other2idx) %>% unlist()
df_sankey_full$target_idx_other <- df_sankey_full$target_name %>% 
  lapply(other2idx) %>% unlist()

# Group Other idx 7
df_sankey_other <- df_sankey_full %>% 
  select(value, 
         source_idx_other, 
         target_idx_other) %>% 
  group_by(source_idx_other,
           target_idx_other) %>% 
  summarise(value=sum(value)) %>% 
  ungroup()

# Subtract off 1
df_sankey_other <- df_sankey_other %>% 
  mutate(source_idx_other=source_idx_other-1,
         target_idx_other=target_idx_other-1)

p2_sankey_other <- sankeyNetwork(Links=df_sankey_other,
              Nodes=df_nodes_other,
              Source='source_idx_other',
              Target='target_idx_other',
              Value='value',
              NodeID='name',
              units='enrolled students',
              fontSize=16,
              nodeWidth=30)
p2_sankey_other