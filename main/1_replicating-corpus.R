# this file is to check whether using the preprocessed data in the replication
# package we can reproduce the corpus.
# lastly modified by Bolun 2022/07/30

# setting the working environment

# load the related libs
library(quanteda)
library(readtext)
library(tidyverse)
library(textclean)

# loading the library
source("0_functions.R")

# reading in all the processed documents
data <- readtext("./data/ProcessedTranscripts/*.txt")

# read in other data
vocab_replication <- read_csv("./other_data/vocab_origin.csv",
                              col_names = FALSE)
names(vocab_replication) <- c("vocab")
names(stop_list_replication) <- c("stopwords")

# load("./Fligstein_et_al_2017_Replication_Package/FOMClda.RData")

# processed the data a little bit follow the author. 

## modify the vocab
vocab_replication$vocab <- vocab_replication$vocab %>%
  str_replace_all(., "communiquÔøΩ", "communique") %>%
  str_replace_all(., "dÔøΩj", "deja") %>% # we do not need to correct dellas
  str_replace_all(., "TRUE", "true") # did not know why there is adj that is capitalized. 

## modify the original text
data$text <- data$text %>%
  str_replace_all(., "communiqu", "communique") %>%
  str_replace_all(., "d�j", "deja") %>% # note we are using a different encoder
  str_replace_all(., "dalla", "dallas")

## replace a few terms according to the Base.R in the file. 
comb_voc <- list(c("cdo","cdos"),c("clo","clos"),c("cfo","cfos"),c("congress","congression"),c("cycl","cyclic"),
                 c("day","day="),c("didn","didn=t"),c("don","don=t"),c("iraq","iraqi"),c("lbo","lbos"))

for (i in comb_voc) {
  to_replace <- i
  remove <- to_replace[2]
  replacement <- to_replace[1] 
  data$text <- data$text %>%
    str_replace_all(., remove, replacement)
}

# define a new dfm to check whether all tokens has been removed 
df2dfm_replicate <- function(df){
  corpus <- quanteda::corpus(df)
  
  # change to new quanteda version's grammar. 
  dfm <- tokens(corpus, remove_punct = FALSE) %>%
    dfm() %>%
    dfm_keep(vocab_replication$vocab) # filter with authors' vocab to achieve the best result
  return(dfm)
}

# transform to a topic model dfm
dfm_replicate <- df2dfm_replicate(data$text)

# check the discrepancy
corpus_term <- dfm_replicate@Dimnames$features %>% as.data.frame()

# check what are lost
left <- vocab_replication %>%
  filter(!(vocab_replication$vocab %in% corpus_term$.))

# now we have the same features. 
saveRDS(dfm_replicate, "./process_files/replicated_dfm.RDS")