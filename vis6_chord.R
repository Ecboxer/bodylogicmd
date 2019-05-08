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

# TODO
# Add states as inner layer
k <- fromJSON(file='data/state_matrix.json')
l <- fromJSON(file='data/states_list.json')
state_list <- l$list
state_list
k$matrix
apply(mat_region, 1, rev)
t(mat_region)
mat_2 <- matrix(bdiag(mat_region, mat_region), 8)
mat_2
apply(mat_2, 1, rev)
chordDiagram(mat_2)