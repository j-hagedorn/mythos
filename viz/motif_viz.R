
library(collapsibleTree); library(visNetwork)

motif_myth %>%
  collapsibleTree(
    c("section_name","level_0", "level_1", "level_2", "level_3","level_4","level_5")
  )

motifs %>%
  filter(str_detect(notes,"Siberia")) %>%
  # filter(section_name %in% c("Myths","Religion","Death")) %>%
  group_by(
    section_name,level_0,level_1,level_2,
    level_3,level_4,level_5
  ) %>%
  summarize(`Number of motifs` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c(
      "section_name","level_0", "level_1", "level_2", 
      "level_3","level_4","level_5"
    ),
    root = "Motifs", attribute = "Number of motifs",
    width = 1000,height = 950, linkLength= 300,
    # fontSize = 10, nodeSize = "count",
    zoomable = T, collapsed = T
  )





