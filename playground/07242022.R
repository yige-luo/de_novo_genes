all_motif_hits <- read.csv("data/all_motif_hits_sorted", header = TRUE, sep = '\t')

all_motif_hits_lst <- split(all_motif_hits, all_motif_hits$motif_id)


freq_tab = ftable(all_motif_hits$type, all_motif_hits$motif_id)
freq_tab = as.matrix(freq_tab)


library(dplyr)
library(tidyr)
library(gplots)

loc_freq_tab <- 
  all_motif_hits %>% 
  group_by(motif_id, type) %>% 
  summarise(n = length(unique(sequence_name)), .groups = "keep")  %>% 
  spread(key = motif_id, value = n)
  

all_motif_hits %>% 
  group_by(type) %>% 
  summarise(n = length(unique(sequence_name)), .groups = "keep")  %>% 
  spread(key = type, value = n)

loc_by_type <- 
  all_motif_hits %>% 
  split(.$type) %>% 
  lapply( function(x) unique(x$sequence_name))

v.table <- venn( list(A = loc_by_type[[1]], 
                      B = loc_by_type[[2]],
                      C = loc_by_type[[3]],
                      D = loc_by_type[[4]],
                      E = loc_by_type[[5]],
                      G = loc_by_type[[7]]), show.plot = FALSE )
sapply(attr(v.table, "intersections"), length)

prob = sapply(attr(v.table, "intersections"), length)
prob = prob / sum(prob)
prob
rmultinom(n = 5, size = 17285, prob = prob)
