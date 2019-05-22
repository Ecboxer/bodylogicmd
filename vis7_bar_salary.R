library(tidyverse)

df_salary <- read_csv('data/salary_school.csv')
df_spec <- read_csv('data/salary_specialty_md_2018.csv')

# Order schools by professor salary
sal_order <- df_salary %>% 
  arrange(desc(Professors)) %>% 
  gather(key=Position,
         value=Salary,
         -c(Institution, State, Type)) %>% 
  select(Institution, Salary, Position) %>% 
  group_by(Institution) %>% 
  mutate(avg_sal=mean(Salary, na.rm=T)) %>% 
  ungroup() %>% 
  select(Institution, avg_sal) %>% 
  unique() %>% 
  arrange(desc(avg_sal)) %>%
  select(Institution) %>% pull()

# Highest salaried professors
p7_inst <- df_salary %>%
  arrange(desc(Professors)) %>% 
  gather(key=Position,
         value=Salary,
         -c(Institution, State, Type)) %>% 
  select(Institution, Salary, Position) %>% 
  filter(Institution%in%sal_order) %>% 
  mutate(Institution=as_factor(Institution),
         Institution=factor(Institution,
                            levels=rev(sal_order))) %>% 
  filter(Position!='Unranked') %>% 
  ggplot() +
  geom_point(aes(x=reorder(Institution,
                           -Salary),
                 y=Salary,
                 group=Position,
                 color=Position),
             size=6,
             alpha=1) +
  coord_flip() +
  ggtitle('Medical school faculty salaries',
        subtitle='2017-2018') +
  xlab('') + ylab('Annual Salary ($)') +
  scale_color_discrete_qualitative(palette='Dark 3') +
  scale_y_continuous(labels=scales::comma,
                     breaks=seq(40000,200000,20000)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour='#dddddd',
                                          size=6),
        panel.grid.major.x = element_line(colour='white'),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
p7_inst_note <- ggdraw(add_sub(p7_inst,
               'Data from The Chronicle of Higher Education\nSchools were ordered by average salary across all positions for which data was available',
               hjust=.0,
               x=0))
p7_inst_note
# ggsave('vis7_inst_grey.svg', plot=p7_inst_note,
#        device='svg', path='assets',
#        width=20, height=16)

# By specialty
df_spec_clean <- df_spec %>%
  filter(!grepl('Total', Department),
         !grepl('Other', Department))

n <- 10
# Highest and lowest salaries
df_spec_top <- df_spec_clean %>% 
  arrange(desc(Median)) %>% 
  head(n)
df_spec_bottom <- df_spec_clean %>% 
  arrange(desc(Median)) %>% 
  tail(n)
df_spec_both <- rbind(df_spec_top,
                      df_spec_bottom)

p7_spec <- df_spec_both %>% 
  mutate(c=ifelse(Department%in%df_spec_top$Department,
                  'high',
                  'low'),
         Median=Median*1000,
         Median_label=formatC(Median,
                              format='f',
                              big.mark=",",
                              digits=0),
         Median_label=paste('$',
                            Median_label)) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(Department, Median),
               y=Median,
               fill=c),
           stat='identity') +
  geom_text(aes(x=reorder(Department, Median),
                y=Median,
                label=Median_label),
            color='white',
            size=5,
            position=position_stack(vjust=0.5)) +
  coord_flip() +
  theme_eric() +
  theme(legend.position='None') +
  scale_y_continuous(labels=scales::comma) +
  ggtitle('Medical school salaries by specialty for faculty with an MD',
          subtitle='2018-2019') +
  xlab('') + ylab('Median salary ($)')
p7_spec
p7_spec_note <- ggdraw(add_sub(p7_spec,
               'Figure displays 10 highest and lowest paid specialties for medical school faculty,\nout of 107 specialties identified in the AAMC Faculty Salary Report 2018/2019'))
# ggsave('vis7_spec.svg', plot=p7_spec_note,
#        device='svg', path='assets',
#        width=20, height=16)