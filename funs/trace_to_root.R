

trace_to_root <- function(search_term){
  
  take_1 <- ntwk %>%
    activate(nodes) %>% as_tibble() %>%
    filter(str_detect(motif_name,regex(search_term,ignore_case = T))) %>%
    .$row
  
  take_2 <- 
    ntwk %>% activate(nodes) %>% 
    filter(node_is_adjacent(to = take_1, mode = 'in',include_to = F)) %>%
    as_tibble() %>% .$row
  
  take_3 <- 
    ntwk %>% activate(nodes) %>% 
    filter(node_is_adjacent(to = take_2, mode = 'in',include_to = F)) %>%
    as_tibble() %>% .$row
  
  take_4 <- 
    ntwk %>% activate(nodes) %>% 
    filter(node_is_adjacent(to = take_3, mode = 'in',include_to = F)) %>%
    as_tibble() %>% .$row
  
  take_5 <- 
    ntwk %>% activate(nodes) %>% 
    filter(node_is_adjacent(to = take_4, mode = 'in',include_to = F)) %>%
    as_tibble() %>% .$row
  
  combined <- sort(unique(c(take_1,take_2,take_3,take_4,take_5)))
  
  return(combined)
  
}

# search_term = "blood"
# search <- trace_to_root("Shaman")