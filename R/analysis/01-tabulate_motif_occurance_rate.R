source("R/helper_func/corr_grad_fill.R")
library(dplyr)
library(tidyr)
library(tibble)
library(GGally)
library(plotly)

all_motif_hits <- 
  read.csv("data/all_motif_hits_sorted", header = TRUE, sep = '\t') %>% 
  filter(type != "type") %>% 
  separate(sequence_name, into = c("ref_chr", "ref_start", "ref_end"), sep = ":|-", remove = FALSE) %>% 
  mutate(ref_end = as.integer(ref_end),
         ref_start = as.integer(ref_start)) %>% 
  mutate(up_length = ref_end - ref_start)

summary(all_motif_hits$up_length)
length(which(all_motif_hits$up_length == 999))

all_motif_hits_lst <- split(all_motif_hits, all_motif_hits$motif_id)

# count of abdA motifs in each gene class
all_motif_hits_lst[["abd-A"]] %>% 
  group_by(type) %>% 
  summarise(n = n())

# A tibble: 6 x 2
# type                       n
# <chr>                  <int>
#   1 AG_specific_expression  1739
# 2 allgenes                3383
# 3 dn                        31
# 4 expressed_genes          138
# 5 intergenic_seq           296
# 6 unann_in_outgroups        56

# count of unique motifs containing abdA in each gene class
all_motif_hits_lst[["abd-A"]] %>% 
  group_by(type ) %>% 
  summarise(n = length(unique(sequence_name)))

# A tibble: 6 x 2
# type                       n
# <chr>                  <int>
#   1 AG_specific_expression  1530
# 2 allgenes                2921
# 3 dn                        28
# 4 expressed_genes          120
# 5 intergenic_seq           249
# 6 unann_in_outgroups        46

freq_tab <- 
  all_motif_hits %>% 
  group_by(motif_id, type) %>% 
  summarise(n = n(), .groups = "keep") %>% 
  spread(key = motif_id, value = n) %>% 
  column_to_rownames("type")

loc_tab <- 
  all_motif_hits %>% 
  group_by(type) %>% 
  summarise(n = length(unique(sequence_name)), .groups = "keep") %>% 
  column_to_rownames("type")

occur_rate <-  freq_tab / loc_tab$n

occur_rate_long <- 
  occur_rate %>% 
  rownames_to_column("gene_class") %>% 
  gather(key = "motif_name", value = "occur_rate", -gene_class)

occur_rate_sub <- 
  # t(occur_rate) %>% 
  # t(select(occur_rate, -c(bab1)) ) %>% 
  t(select(occur_rate, -c(bab1, hb)) ) %>%
  as.data.frame() %>% 
  select(c(dn, AG_specific_expression, allgenes, unann_in_outgroups, expressed_genes, intergenic_seq))