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
  ggplot() +
  geom_point(aes(x=reorder(Institution,
                           -Salary),
                 y=Salary,
                 group=Position,
                 color=Position),
             size=6,
             alpha=0.8) +
  coord_flip() +
  theme_eric() +
  ggtitle('Medical school faculty salaries',
        subtitle='2017-2018') +
  xlab('') + ylab('Annual Salary ($)') +
  scale_color_discrete_qualitative(palette='Dark 3') +
  scale_y_continuous(labels=scales::comma)
p7_inst_note <- ggdraw(add_sub(p7_inst,
               'Data from The Chronicle of Higher Education\nSchools were ordered by average salary across all positions for which data was available',
               hjust=.0,
               x=0))
p7_inst_note
# ggsave('vis7_inst.svg', plot=p7_inst_note,
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
         Median=Median*1000) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(Department, Median),
               y=Median,
               fill=c),
           stat='identity') +
  geom_text(aes(x=reorder(Department, Median),
                y=Median,
                label=Department),
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
# ggsave('vis7_spec.svg', plot=p7_spec,
#        device='svg', path='assets',
#        width=20, height=16)