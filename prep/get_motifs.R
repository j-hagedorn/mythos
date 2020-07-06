# get_motifs.R

library(tidyverse); library(rvest)

# url <- "https://sites.ualberta.ca/~urban/Projects/English/Content/a.htm"
# section_name <- "Myths"

get_motifs <- function(section_name,url){
  library(tidyverse); library(rvest)
  
  html_df <- 
    read_html(url) %>% 
    html_nodes('p') %>%
    html_text() %>%
    enframe(name = NULL)
  
  df <-
    html_df %>%
    slice(-1:-3) %>%
    mutate(
      # Remove everything before the initial '†'
      value = str_remove(value,"^[^†]*†"),
      # Extract and remove section ID (up to first space)
      section = str_extract(value,"[^\\s]+"),
      value = str_remove(value,"[^\\s]+"),
      # Extract and remove brief section name (up to first period)
      name = str_extract(value,"[^\\.]+"),
      name = str_replace_all(name,"\r?\n|\r"," "),
      notes = str_sub(value,start = str_locate(value,"\\.")[,1] + 1)
    ) %>%
    select(section,name,notes) %>%
    mutate_all(.funs = list(~str_trim(.))) %>%
    mutate_all(.funs = list(~na_if(.,""))) %>%
    filter(!section %in% c("Note:","DETAILED")) %>%
    # Remove final decimal/dot
    mutate(section = str_remove(section,"\\.$")) %>%
    separate(section, into = c("a","b","c","d","e"),sep = "\\.",remove = F) %>%
    filter(!is.na(name)) %>%
    # Remove title levels which are duplicated beneath
    filter(!str_detect(name,"†")) %>%
    filter(str_length(section) > 1) %>% 
    mutate(
      name = str_to_title(name),
      level_0 = ifelse(str_detect(section,pattern = "--"),name,NA),
      level_1 = ifelse(!is.na(a)&is.na(b)&is.na(c)&is.na(d)&is.na(e),name,NA),
      level_2 = ifelse(!is.na(a)&!is.na(b)&is.na(c)&is.na(d)&is.na(e)&is.na(level_0),name,NA),
      level_3 = ifelse(!is.na(a)&!is.na(b)&!is.na(c)&is.na(d)&is.na(e),name,NA),
      level_4 = ifelse(!is.na(a)&!is.na(b)&!is.na(c)&!is.na(d)&is.na(e),name,NA),
      level_5 = ifelse(!is.na(a)&!is.na(b)&!is.na(c)&!is.na(d)&!is.na(e),name,NA),
      level = case_when(
        !is.na(level_0)                                                                     ~ "0",
        !is.na(level_1) & is.na(level_2) & is.na(level_3) & is.na(level_4) & is.na(level_5) ~ "1",
        is.na(level_1) & !is.na(level_2) & is.na(level_3) & is.na(level_4) & is.na(level_5) ~ "2",
        is.na(level_1) & is.na(level_2) & !is.na(level_3) & is.na(level_4) & is.na(level_5) ~ "3",
        is.na(level_1) & is.na(level_2) & is.na(level_3) & !is.na(level_4) & is.na(level_5) ~ "4",
        is.na(level_1) & is.na(level_2) & is.na(level_3) & is.na(level_4) & !is.na(level_5) ~ "5",
        TRUE ~ NA_character_
      ),
      level = as.numeric(level)
    ) %>% 
    filter(!(str_detect(section,"--") & str_detect(lead(section),"--"))) %>%
    fill(level_0) %>% group_by(level_0) %>%
    fill(level_1) %>% group_by(level_1) %>% 
    filter(!level %in% c("0") | n() == 1) %>%
    fill(level_2) %>%
    mutate(level_2 = if_else(is.na(level_2)&!is.na(level_3)|!is.na(level_4),level_1,level_2)) %>% 
    group_by(level_2) %>% filter(!level %in% c("2") | n() == 1) %>%
    fill(level_3) %>%
    mutate(level_3 = if_else(is.na(level_3)&!is.na(level_4),level_2,level_3)) %>% 
    group_by(level_3) %>% filter(!level %in% c("3") | n() == 1) %>%
    fill(level_4) %>%
    mutate(level_4 = if_else(is.na(level_4)&!is.na(level_5),level_3,level_4)) %>% 
    group_by(level_4) %>% filter(!level %in% c("4") | n() == 1) %>%
    mutate(section_name = section_name)
  
  return(df)
  
}

motif_myth <- get_motifs("Myths","https://sites.ualberta.ca/~urban/Projects/English/Content/a.htm")
motif_animal <- get_motifs("Animals","https://sites.ualberta.ca/~urban/Projects/English/Content/b.htm")
motif_tabu <- get_motifs("Tabu","https://sites.ualberta.ca/~urban/Projects/English/Content/c.htm")
motif_magic <- get_motifs("Magic","https://sites.ualberta.ca/~urban/Projects/English/Content/d.htm")
motif_dead <- get_motifs("Death","https://sites.ualberta.ca/~urban/Projects/English/Content/e.htm")
motif_marvels <- get_motifs("Marvels","https://sites.ualberta.ca/~urban/Projects/English/Content/f.htm")
motif_ogres <- get_motifs("Ogres","https://sites.ualberta.ca/~urban/Projects/English/Content/g.htm")
motif_tests <- get_motifs("Tests","https://sites.ualberta.ca/~urban/Projects/English/Content/h.htm")
motif_wisdom <- get_motifs("Wisdom and Folly","https://sites.ualberta.ca/~urban/Projects/English/Content/j.htm")
motif_deceive <- get_motifs("Deceptions","https://sites.ualberta.ca/~urban/Projects/English/Content/k.htm")
motif_fortune <- get_motifs("Reversals of Fortune","https://sites.ualberta.ca/~urban/Projects/English/Content/l.htm")
motif_future <- get_motifs("Ordaining the Future","https://sites.ualberta.ca/~urban/Projects/English/Content/m.htm")
motif_chance <- get_motifs("Chance and Fate","https://sites.ualberta.ca/~urban/Projects/English/Content/n.htm")
motif_society <- get_motifs("Society","https://sites.ualberta.ca/~urban/Projects/English/Content/p.htm")
motif_rewards <- get_motifs("Rewards and Punishments","https://sites.ualberta.ca/~urban/Projects/English/Content/q.htm")
motif_captive <- get_motifs("Captives and Fugitives","https://sites.ualberta.ca/~urban/Projects/English/Content/r.htm")
motif_cruelty <- get_motifs("Cruelty","https://sites.ualberta.ca/~urban/Projects/English/Content/s.htm")
motif_sex <- get_motifs("Sex","https://sites.ualberta.ca/~urban/Projects/English/Content/t.htm")
motif_life <- get_motifs("Nature of Life","https://sites.ualberta.ca/~urban/Projects/English/Content/u.htm")
motif_religion <- get_motifs("Religion","https://sites.ualberta.ca/~urban/Projects/English/Content/v.htm")
motif_traits <- get_motifs("Traits of Character","https://sites.ualberta.ca/~urban/Projects/English/Content/w.htm")
motif_humor <- get_motifs("Humor","https://sites.ualberta.ca/~urban/Projects/English/Content/x.htm")
motif_misc <- get_motifs("Miscellaneous","https://sites.ualberta.ca/~urban/Projects/English/Content/z.htm")

motifs <- 
  bind_rows(
    motif_myth,motif_animal,motif_tabu,motif_magic,motif_dead,motif_marvels,
    motif_ogres,motif_tests,motif_wisdom,motif_deceive,motif_fortune,motif_future,
    motif_chance,motif_society,motif_rewards,motif_captive,motif_cruelty,motif_sex,
    motif_life,motif_religion,motif_traits,motif_humor,motif_misc
  ) %>%
  select(-a:-e) %>%
  ungroup() %>%
  mutate_all(list(~str_trim(.))) %>%
  select(section,section_name,name,notes,level,level_0:level_5)

rm(list = c("motif_myth","motif_animal","motif_tabu","motif_magic","motif_dead","motif_marvels",
            "motif_ogres","motif_tests","motif_wisdom","motif_deceive","motif_fortune","motif_future",
            "motif_chance","motif_society","motif_rewards","motif_captive","motif_cruelty","motif_sex",
            "motif_life","motif_religion","motif_traits","motif_humor","motif_misc"))  
  
feather::write_feather(motifs,"motifs.feather")
write_csv(motifs,"motifs.csv")
