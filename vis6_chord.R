# Assuming "All other states" is uniformly distributed or distributed according to population

# TODO Fix inner arcs
# Or switch to normal sankey/chord
library(rjson)
library(tidyverse)
library(circlize)
library(Matrix)
library(graphics)
library(gridExtra)

j <- fromJSON(file='data/region_matrix.json')
m_region <- j$matrix
m_1 <- m_region[[1]]
m_2 <- m_region[[2]]
m_3 <- m_region[[3]]
m_4 <- m_region[[4]]
mat_region <- matrix(c(m_1, m_2, m_3, m_4), 4)
regions_tr <- c('NE-trained',
                'C-trained',
                'S-trained',
                'W-trained')
regions_pr <- c('NE-practicing',
                'C-practicing',
                'S-practicing',
                'W-practicing')
rownames(mat_region) <- regions_tr
colnames(mat_region) <- regions_pr
regions_order <- c(rev(regions_pr), regions_tr)
mat_region
c1 <- '#1b9e77'
c2 <- '#d95f02'
c3 <- '#7570b3'
c4 <- '#e7298a'
grid.col <- c('NE-trained'=c1,
              'C-trained'=c2,
              'S-trained'=c3,
              'W-trained'=c4,
              'NE-practicing'=c1,
              'C-practicing'=c2,
              'S-practicing'=c3,
              'W-practicing'=c4)
chordDiagram(mat_region,
             order=regions_order,
             grid.col=grid.col)

prac <- mat_region %>% colSums()
train <- mat_region %>% rowSums()
# Share of students trained in the region to stay
mat_region/train
mat_region
# # TODO
# # Add states as inner layer
# k <- fromJSON(file='data/state_matrix.json')
# l <- fromJSON(file='data/states_list.json')
# state_list <- l$list
# state_list
# mat_state_temp <- k$matrix
# mat_state <- do.call(rbind, mat_state_temp)
# 
# states_tr <- paste0(state_list, '-trained')
# states_pr <- paste0(state_list, '-practicing')
# rownames(mat_state) <- states_tr
# colnames(mat_state) <- states_pr
# states_order <- c(rev(states_pr), states_tr)
# chordDiagram(mat_state,
#              order=states_order)