
library(tidyverse); library(tidytext); library(text2vec); library(quanteda)
library(textclean)

tst <-
  df %>%
  select(id,motif_name,notes) %>%
  mutate(motif_name = str_to_lower(motif_name))

model = Collocations$new(collocation_count_min = 50)
it = itoken(paste(tst$motif_name,collapse = " "))
model$fit(it, n_iter = 3)
model$collocation_stat


clean <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  # xdtm <- VCorpus(VectorSource(x)) %>%
  #   tm_map(removeWords, stopwords("en"))
  # 
  # # convert corpus to document term matrix
  # return(DocumentTermMatrix(xdtm))
  
}

clean(tst$motif_name)
