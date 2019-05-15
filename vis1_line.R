library(tidyverse)
library(colorspace)

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

# Scratch: too many states
# Number of applicants by state
df_app %>%
  filter(`State of Legal Residence`!='Total for the Region') %>% 
  select(-'% Change') %>% 
  gather(key=year, value=count,
         -c('Region', 'State of Legal Residence')) %>% 
  rename(State='State of Legal Residence') %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=State, color=State),
            size=2) +
  scale_color_discrete_qualitative(palette='Dark 3') +
  theme_eric()

# Number of matriculants by state
df_mat %>%
  filter(`State of Legal Residence`!='Total for the Region') %>% 
  select(-'% Change') %>% 
p  gather(key=year, value=count,
         -c('Region', 'State of Legal Residence')) %>% 
  rename(State='State of Legal Residence') %>% 
  ggplot() +
  geom_line(aes(x=year, y=count,
                group=State, color=State))
