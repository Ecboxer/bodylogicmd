library(tidyverse)
library(colorspace)
library(ggalt)

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
                group=Region, color=Region),
            size=2) +
  scale_color_discrete_qualitative(palette='Dark 3') +
  theme_eric() +
  ggtitle(label='Medical school applicants by region of legal residence',
        subtitle='2009-2018') +
  xlab('') + ylab('Number of matriculants')

# Number of matriculants by region
df_mat %>%
  filter(`State of Legal Residence`=='Total for the Region') %>%
  select(-'State of Legal Residence',
         -'% Change') %>% 
  gather(key=year, value=count, -c('Region')) %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=Region, color=Region),
            size=2) +
  scale_color_discrete_qualitative(palette='Dark 3') +
  theme_eric() +
  ggtitle(label='Medical school matriculants by region of legal residence',
          subtitle='2009-2018') +
  xlab('') + ylab('Number of matriculants')

# Combine applicants and matriculants
df1 <- df_app %>%
  filter(`State of Legal Residence`=='Total for the Region') %>%
  select(-'State of Legal Residence',
         -'% Change') %>% 
  gather(key=year, value=count, -c('Region'))
df2 <- df_mat %>%
  filter(`State of Legal Residence`=='Total for the Region') %>%
  select(-'State of Legal Residence',
         -'% Change') %>% 
  gather(key=year, value=count, -c('Region'))
p1_line <- merge(df1, df2,
      by=c('year', 'Region'),
      suffixes=c('_app', '_mat')) %>% 
  gather(key=key, value=count,
         -c('year', 'Region')) %>% 
  mutate(Type=ifelse(key=='count_app',
                     'Applicants',
                     'Matriculants')) %>% 
  ggplot(aes(x=year, y=count,
             color=Region, linetype=Type,
             group=interaction(Region, Type))) +
  geom_line(size=2) + geom_point(size=3) +
  scale_color_discrete_qualitative(palette='Dark 3') +
  scale_linetype_manual(values=c('solid',
                                 'twodash')) +
  theme_eric() +
  ggtitle(label='Medical school applicants and matriculants by region of legal residence',
          subtitle='2009-2018') +
  xlab('') + ylab('Students')
p1_line
# ggsave('vis1_line.svg', plot=p1_line,
#        device='svg', path='assets',
#        width=20, height=16)

# Split into two line charts, app and mat
# Change y-axis into % of total
# Applicants -> df1 Matriculants -> df2
p1_line_app <- df1 %>% 
  group_by(year) %>% 
  mutate(total_year=sum(count),
         perc_year=count/total_year) %>% 
  ggplot(aes(x=year, y=perc_year,
             color=Region, group=Region)) +
  geom_line(size=2) + geom_point(size=3) +
  scale_color_discrete_qualitative(palette='Dark 3') +
  theme_eric() +
  ggtitle(label='Medical school applicants by region of legal residence',
          subtitle='2009-2018') +
  xlab('') + ylab('Percentage of Students') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1),
                     limits=c(.18,.36),
                     breaks=seq(.18,.36,by=0.02))
p1_line_app

p1_line_mat <- df2 %>% 
  group_by(year) %>% 
  mutate(total_year=sum(count),
         perc_year=count/total_year) %>% 
  ggplot(aes(x=year, y=perc_year,
             color=Region, group=Region)) +
  geom_line(size=2) + geom_point(size=3) +
  scale_color_discrete_qualitative(palette='Dark 3') +
  theme_eric() +
  ggtitle(label='Medical school matriculants by region of legal residence',
          subtitle='2009-2018') +
  xlab('') + ylab('Percentage of Students') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1),
                     limits=c(.18,.36),
                     breaks=seq(.18,.36,by=0.02))
p1_line_mat
# ggsave('vis1_line_app.svg', plot=p1_line_app,
#        device='svg', path='assets',
#        width=20, height=16)
# ggsave('vis1_line_mat.svg', plot=p1_line_mat,
#        device='svg', path='assets',
#        width=20, height=16)

# Revisions
# Butterfly chart
df_bfly_app <- df1 %>% 
  group_by(year) %>% 
  mutate(total_year=sum(count),
         perc_year=count/total_year) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  summarise(Applications=mean(perc_year))
df_bfly_mat <- df2 %>% 
  group_by(year) %>% 
  mutate(total_year=sum(count),
         perc_year=count/total_year) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  summarise(Matriculations=mean(perc_year))
df_bfly <- merge(df_bfly_app, df_bfly_mat) %>% 
  gather(key=student_type,
         value=perc_students,
         -c('Region'))
region_order <- df_bfly %>% 
  filter(student_type=='Applications') %>% 
  arrange(desc(perc_students)) %>% 
  select(Region) %>% unlist()
df_bfly <- df_bfly %>% 
  mutate(perc_students=ifelse(student_type=='Applications',
                              -1*perc_students,
                              perc_students),
         Region=factor(Region,
                      levels=region_order))
df_bfly
p1_bfly <- ggplot(df_bfly, aes(x=Region,
                    y=perc_students,
                    fill=student_type)) +
  geom_bar(data=subset(df_bfly,
                   student_type=='Applications'),
           stat='identity') +
  geom_bar(data=subset(df_bfly,
                   student_type=='Matriculations'),
           stat='identity') +
  coord_flip() +
  theme_eric() +
  ggtitle('Medical school applications and matriculations by region',
          subtitle='Averages taken over the period 2009-2018') +
  xlab('Region') + ylab('Percentage of students') +
  labs(fill='Student Type') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1),
                     breaks=seq(-.35,.35,.05))
p1_bfly
df_bfly
p1_db <- df_bfly %>% 
  spread(student_type,perc_students) %>% 
  mutate(Applications=-1*Applications,
         app_lab=round(Applications,3)*100,
         app_lab=paste(app_lab, '%', sep=''),
         mat_lab=round(Matriculations,3)*100,
         mat_lab=paste(mat_lab, '%', sep='')) %>% 
  ggplot(aes(x=Applications,
             xend=Matriculations,
             y=Region,
             group=Region)) +
  geom_segment(aes(yend=Region),
               color='black',
               size=4,
               show.legend=T) +
  geom_dumbbell(colour_x='#a3c4dc',
                colour_xend='#0e668b',
                size_x=8,
                size_xend=8,
                show.legend=T) +
  geom_text(color='black',
            size=4,
            vjust=-2,
            aes(x=Applications, 
                label=app_lab)) +
  geom_text(color='black',
            size=4,
            vjust=-2,
            aes(x=Matriculations, 
                label=mat_lab)) +
  theme_eric() +
  ggtitle('Share of medical school applications and matriculations by region',
          subtitle='Averages taken over the period 2009-2018') +
  xlab('Percentage of students') +
  labs(y=NULL, fill='Student Type') +
  scale_x_continuous(labels=scales::percent_format(accuracy=1),
                     breaks=seq(.2,.35,.02),
                     limits=c(.2,.35)) +
  scale_color_manual(name='',
                     values=c('Applications'='#a3c4dc',
                              'Matriculations'='#0e668b'))
p1_db_note <- p1_db + 
  annotate('text',
                 x=.213, y=4.3,
                 size=5,
                 label='Applications') +
  annotate('text',
           x=.237, y=4.3,
           size=5,
           label='Matriculations')
p1_db_note
p1_bfly
# ggsave('vis1_bfly.svg', plot=p1_bfly,
#        device='svg', path='assets',
#        width=20, height=16)
# ggsave('vis1_db.svg', plot=p1_db_note,
#        device='svg', path='assets',
#        width=20, height=16)

# Scratch: too many states
# Number of applicants by state
# df_app %>%
#   filter(`State of Legal Residence`!='Total for the Region') %>% 
#   select(-'% Change') %>% 
#   gather(key=year, value=count,
#          -c('Region', 'State of Legal Residence')) %>% 
#   rename(State='State of Legal Residence') %>% 
#   ggplot() +
#   geom_line(aes(x=year, y=count,
#                 group=State, color=State),
#             size=2) +
#   scale_color_discrete_qualitative(palette='Dark 3') +
#   theme_eric()
# 
# # Number of matriculants by state
# df_mat %>%
#   filter(`State of Legal Residence`!='Total for the Region') %>% 
#   select(-'% Change') %>% 
# p  gather(key=year, value=count,
#          -c('Region', 'State of Legal Residence')) %>% 
#   rename(State='State of Legal Residence') %>% 
#   ggplot() +
#   geom_line(aes(x=year, y=count,
#                 group=State, color=State))
