library(tidyverse)
library(cowplot)

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
# Decide on applicants or matriculants
# Neaten

# Cleveland dot plot of top and bottom 10 schools for
# female app, mat, enr, grd (determined by app)
# Write dfs of app
df_app_mat_clean <- df_app_mat %>%
  select(State,
         school=`Medical School`,
         perc_app_fem=`Perc Applicants Women`) %>% 
  filter(State!='Total') %>% 
  select(-State) %>% 
  arrange(desc(perc_app_fem))
df_top_app_mat <- df_app_mat_clean %>% 
  head(10)
df_bottom_app_mat <- df_app_mat_clean %>% 
  tail(10)
df_app_mat_plot <- rbind(df_top_app_mat,
      df_bottom_app_mat)

# Write dfs of enr and grd
df_enr_perc <- df_enr %>%
  mutate(perc_enr_fem=Women/All*100) %>% 
  select(school=`Medical School`,
         perc_enr_fem)
df_grd_perc <- df_grd %>%
  mutate(Women=as.integer(Women),
         All=as.integer(All),
         perc_grd_fem=Women/All*100) %>% 
  select(school=`Medical School`,
         perc_grd_fem)

# Write combined df
df_dot_plot <- df_app_mat_plot %>% 
  left_join(df_enr_perc,
            by=c('school')) %>% 
  left_join(df_grd_perc,
            by=c('school')) 

# Two schools are missing graduation data
df_dot_plot %>% 
  filter(is.na(perc_grd_fem))

colnames(df_dot_plot) <- c('Medical School',
                           'Applicants',
                           'Enrollees',
                           'Graduates')
# Order schools by perc of applicants
school_order <- df_dot_plot %>% 
  select(`Medical School`) %>% 
  pull()

# Plot app, mat, enr, grd for top and bottom schools
p4_dot <- df_dot_plot %>% 
  mutate(`Medical School`=as_factor(`Medical School`),
         `Medical School`=factor(`Medical School`,
                                 levels=rev(school_order))) %>% 
  gather(key=Type,
         value=Count,
         -c(`Medical School`)) %>% 
  mutate(Count=Count/100) %>% 
  ggplot() +
  geom_hline(yintercept=.5,
             linetype='dotted',
             size=2) +
  geom_point(aes(x=`Medical School`,
                 y=Count,
                 color=Type,
                 group=Type),
             size=10,
             alpha=.8) +
  coord_flip() +
  theme_eric() +
  ggtitle('Top and bottom 10 medical schools for female students',
          subtitle='2018-2019') +
  xlab('') + ylab('Percentage of Students') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_color_discrete_qualitative(palette='Dark 3')
p4_dot
p4_dot_note <- ggdraw(add_sub(p4_dot,
               'Missing data for graduates of CUNY and Carle Illinois',
               x=.827))
ggsave('vis4_dot.svg', plot=p4_dot_note,
       device='svg', path='assets',
       width=20, height=16)
