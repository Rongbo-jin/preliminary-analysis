library(foreign)
library(readtext)
library(readxl)
library(xlsx)
library(stringr)
library(SnowballC)
library(dplyr)
library(quanteda)
library(quanteda.dictionaries)
library(stopwords)
library(ggplot2)
library(tidytext)
library(magrittr)
library(tidyr)
library(tidyverse)
library(stm)
library(textnets)
library(htmlwidgets)
library(topicmodels)
load("/Users/macintoshhd/Documents/projects/second-year project/anes.RData")  
# get rid of NAs
anes_na.omit1 <- anes %>% 
  select(party_id_cat, dem_like, ft_dif_cat) %>%  
  mutate(dem_like=ifelse(is.na(dem_like), NA,
                         ifelse(dem_like < 0, NA, dem_like))) %>% na.omit()

# creat a corpus and preprocessing 
corpus1 <- corpus(anes_na.omit1, text_field = "dem_like") %>% # here, I think I have included all the docvars inthe corpus
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_symbols = TRUE,
         padding = TRUE) %>%
  tokens_remove(c(stopwords(language = "en", source = "smart"), "Democratic", "Republican", "Party", "democrat", "republican", "party"), padding = TRUE)
# Compounding words
collocations <- textstat_collocations(corpus1, min_count = 20)
corpus1_compound <- tokens_compound(corpus1,
                                    pattern =  phrase(collocations$collocation[collocations$z > 3]))
# convert to lower case and stemming
corpus1_clean <- corpus1_compound %>%
  tokens_tolower() %>%
  tokens_wordstem()

# create DFM 
# trim DFM, delete words with counts less than 10 or more than 1000
dfm1 <- dfm(corpus1_clean) %>%
  dfm_trim(min_termfreq = 10, 
           max_termfreq = 1000)

# convert DFM to tidy version
tidy1 <- tidytext::tidy(dfm1)
# sentiment analysis
# get sentiments
sentiments1 <- tidy1 %>% 
  group_by(party_id_cat) %>%  
  # here, I got an err: The column "party_id_cat" is unknown, but I think I've include "party_id_cat" as docvar in the corpus
  # does the tidy() function make me lose the docvars? 
  mutate(n_total = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("nrc")) 

# Structural Topic Model
out <- quanteda::convert(dfm1, to = "stm")
docs <- out$documents
vocab <- out$vocab
meta <- out$meta



stm2 <- stm(documents = out$documents,
            vocab = out$vocab, 
            K = 5, 
            prevalence =~ party_id_cat,
            max.em.its = 75, 
            data = out$meta,
            init.type = "Spectral", 
            verbose = FALSE)
plot(stm2, n=8)

# to check the original texts of topic4 in stm2 
findThoughts(stm2, 
             texts = out$dem_like,
             n = 7, 
             topics = 4)
# Here, it doesn't return an err but it returned an empty string, which should be the original text falling into this topic. So I'm just quite confused where I messed up

