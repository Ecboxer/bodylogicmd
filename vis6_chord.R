# Assuming "All other states" is uniformly distributed or distributed according to population

# TODO Fix inner arcs
# Or switch to normal sankey/chord
library(rjson)
library(tidyverse)
library(circlize)

j <- fromJSON(file='data/region_matrix.json')
m_region <- j$matrix
m_1 <- m_region[[1]]
m_2 <- m_region[[2]]
m_3 <- m_region[[3]]
m_4 <- m_region[[4]]
mat_region <- matrix(c(m_1, m_2, m_3, m_4), 4)
regions_list <- c('Northeast',
                  'Central',
                  'South',
                  'West')
rownames(mat_region) <- regions_list
colnames(mat_region) <- regions_list
mat_region

chordDiagram(mat_region)