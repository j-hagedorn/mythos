
library(tidyverse); library(tidygraph)
library(visNetwork)

# Clean up messiness
df <- motifs %>% 
  filter(name != "") %>%
  filter(!is.na(level_0))


df %>%
  filter(level_1 == level_2) %>%
  View()

ntwk_df <-
  bind_rows(
    tibble(from = "Root", to = unique(df$section_name)),
    df %>% ungroup() %>% select(from = section_name,to = level_0) %>% distinct(),
    df %>% ungroup() %>% select(from = level_0,to = level_1) %>% distinct(),
    df %>% ungroup() %>% select(from = level_1,to = level_2) %>% distinct(),
    df %>% ungroup() %>% select(from = level_2,to = level_3) %>% distinct(),
    df %>% ungroup() %>% select(from = level_3,to = level_4) %>% distinct(),
    df %>% ungroup() %>% select(from = level_4,to = level_5) %>% distinct()
  ) %>%
  # Need to tidy up in prep grouping, avoid distinct(to, .keep_all = T)
  distinct(to, .keep_all = T) %>% ungroup() %>% 
  filter(!is.na(to) & !is.na(from))
  
ntwk <- 
  ntwk_df %>% 
  as_tbl_graph(directed = T) %>% 
  activate(edges) %>% filter(!edge_is_multiple()) %>%
  activate(nodes) %>% 
  mutate(
    center = node_is_center(),
    neighbors = centrality_degree()
  )

rm(ntwk_df)  
