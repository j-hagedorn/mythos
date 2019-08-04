

library(tidyverse); library(tidygraph)
library(visNetwork)

df <- 
  motifs %>% 
  filter(section_name == "Religion")

df <-
  bind_rows(
    df %>% ungroup() %>% select(from = section_name,to = level_0) %>% distinct(),
    df %>% ungroup() %>% select(from = level_0,to = level_1) %>% distinct(),
    df %>% ungroup() %>% select(from = level_1,to = level_2) %>% distinct(),
    df %>% ungroup() %>% select(from = level_2,to = level_3) %>% distinct(),
    df %>% ungroup() %>% select(from = level_3,to = level_4) %>% distinct()
  ) %>%
  distinct() %>% ungroup()
  
ntwk <- df %>% as_tbl_graph(directed = F) %>% activate(edges) %>% filter(!edge_is_multiple())

  ntwk <- list()
  
  ntwk$nodes <-
    unique(
      c(unique(as.character(df$from)),
        unique(as.character(df$to))
      )
    ) %>%
    data.frame("name_id" = .) %>%
    # Alphabetize
    arrange(name_id) %>%
    # Get values to size nodes
    # Assign ids starting at 0
    mutate(
      title = as.character(name_id),
      id = row_number(name_id)-1 
    )
    
  
  ntwk$edges <-
    df %>%
    left_join(ntwk$nodes %>% select(-title), by = c("from" = "name_id")) %>%
    rename(
      name_from = from,
      from = id
    ) %>%
    left_join(ntwk$nodes %>% select(-title), by = c("to" = "name_id")) %>%
    rename(
      name_to = to,
      to = id
    ) %>%
    filter(!is.na(from) & !is.na(to)) %>%
    droplevels()
  
  missing <-
  c(
    setdiff(unique(ntwk$nodes$id),unique(c(ntwk$edges$from,ntwk$edges$to))),
    setdiff(unique(c(ntwk$edges$from,ntwk$edges$to)),unique(ntwk$nodes$id))
  )
  
  ntwk$nodes <- ntwk$nodes %>% filter(!id %in% missing)
  
  
  visNetwork(ntwk$nodes, ntwk$edges) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = FALSE) %>%
  visOptions(collapse = list(enabled = TRUE))
  
  %>%
  visIgraphLayout()
  
  %>% 
    visOptions(
       
      highlightNearest = list(enabled = T, degree = 1, hover = F)
    ) %>%
    visEdges(
      color = list(color = "#E0EEEE", highlight = "#E1AF00"),
      arrows = list(from = list(enabled = TRUE, scaleFactor = 1))
    ) %>%
    visPhysics(maxVelocity = 5,stabilization = F) %>%
    visLayout(randomSeed = 123) %>%
    visLegend(width = 0.1, position = "right") 
  