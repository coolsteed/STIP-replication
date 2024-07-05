# this script is to scaling the hypothesis 2 test to all solutions with the
# 4 and 8 key topics replicated.
# change to the speaker coding to original one.

# changed to authors' processed data.
# change the beta to the word weight

# lastly modified by Bolun 0816

#### 0. Preparation and function setting ####
# setting the working environment
library(tidyverse)
library(stringr)
library(quanteda)
library(dplyr)
library(tidytext)
library(ggplot2)
library(topicmodels)
library(RcppHungarian)
library(MKinfer)
library(reshape2)

# sourcing functions and related corpus
source("0_functions.R")

# reading the data
df_long <-
  readRDS("./process_files/stemmed_df_long.RDS") # this is from replicate-corpus.RMD,
# since we integrate python code here.

# reading in the replicated data
fed_dfm_replicated <- readRDS("./process_files/replicated_dfm.RDS") # produced by replicating-corpus.R

fligstein_top <- read_csv("./other_data/origin_top30.csv",
                          col_names = FALSE)

fed_topics_dfm <- convert(fed_dfm_replicated, to = "topicmodels")

speaker_info <- read.csv("./other_data/speaker_fligstein.csv") # this is using their coding

#### estimating all related lda models ####
# get all the seeds

load(file = "./model_output/results_mp_v4.RData")

rs <- data.frame()
for (i in 1:200) {
  rs <- rbind(rs, results_mp[[i]][[1]])
}

# redefine the pass
rs$pass <- 0
rs$pass[which(rs$ja >= 0.15 & rs$overlap >= 10)] <- 1

# all topics are replicated
rs_all <- rs %>%
  group_by(seed) %>%
  summarise(pass_all = sum(pass)) %>%
  filter(pass_all == 15)

rs_8 <- rs %>%
  filter(V2 %in% c(1, 4, 9, 12, 3, 5, 6, 7)) %>%
  group_by(seed) %>%
  summarise(pass_all = sum(pass)) %>%
  filter(pass_all == 8)

rs_4 <- rs %>%
  filter(V2 %in% c(1, 4, 9, 12)) %>%
  group_by(seed) %>%
  summarise(pass_all = sum(pass)) %>%
  filter(pass_all == 4)

select_rs_8 <- rs %>%
  filter(V2 %in% c(1, 4, 9, 12, 3, 5, 6, 7)) %>%
  filter(seed %in% rs_8$seed) %>%
  summarise(mean = mean(ja))

# getting all the seeds
seeds <- rs_4$seed

## reading the cleaned data
word_list <- fed_topics_dfm$dimnames$Terms ## 5586 features to use in the filter

## get the document probability
## we proximate the probability of the document by dividing the length of the 
## article to the total length of corpus using observed dfm
dfm <- as.matrix(fed_topics_dfm)
doc_prob <- rowSums(dfm)/sum(dfm)

## save all the models
## library the parallel framework
library(doParallel)
library(foreach)

## making a cluster
cores <- detectCores(logical = TRUE) - 2 # leave two CPU spare...
cluster <- makeCluster(cores)
registerDoParallel(cluster)

registerDoParallel(cluster)
# loading related packages
clusterEvalQ(cluster, {
  library(topicmodels)
  library(tidyverse)
  library(tidytext)
  library(quanteda)
  library(MKinfer)
  library(reshape2)
})

clusterExport(cluster, c("fed_topics_dfm", "seeds", "df_long", 
                         "word_list", "doc_prob"))

sensitivity_test <- foreach(num = 1:length(seeds)) %dopar% {
  # for the debug
  seed <- seeds[num]
  filename <-
    paste0("./model_output/local_models/model_", as.character(seed))
  filename <- paste0(filename, ".RDS")
  rep_lda <- readRDS(filename)
  
  word_list <- rep_lda@terms
  
  ## get the beta
  beta <- tidy(rep_lda, matrix = "beta")
  beta_wide <- beta %>%
    mutate(topic = paste0("topic", topic)) %>%
    pivot_wider(names_from = topic, values_from = beta) %>%
    t()
  
  colnames(beta_wide) <- beta_wide[1,]
  beta_wide <- beta_wide[-1,]
  beta_wide <- apply(beta_wide,2, as.numeric) # beta is the word given topic prob
  
  ## get the gamma
  gamma <- tidy(rep_lda, matrix = "gamma")
  gamma_wide <- gamma %>%
    mutate(topic = paste0("topic", topic)) %>%
    pivot_wider(names_from = topic, values_from = gamma) %>%
    t()
  colnames(gamma_wide) <- gamma_wide[1,]
  gamma_wide <- gamma_wide[-1,]
  gamma_wide <- apply(gamma_wide,2, as.numeric)
  
  word_prob_given_doc <- t(beta_wide) %*% gamma_wide
  
  word_prob <- word_prob_given_doc %*% doc_prob
  topic_prob <- gamma_wide %*% doc_prob
  # get the topic_prob
  topic_prob <- gamma_wide %*% doc_prob
  
  ## calculate the tpgw using baysian law
  
  topic_given_word_prob <- apply(beta_wide, 2, function(beta) beta * topic_prob)
  topic_given_word_prob <- apply(topic_given_word_prob, 1, function(x) x / word_prob) %>% t()
  colnames(topic_given_word_prob) <- colnames(beta_wide)
  
  X <- beta_wide * topic_given_word_prob
  
  word_prob <- beta %>%
    group_by(term) %>%
    summarise(prob = sum(beta)) %>%
    filter(prob > 1e-5)
  
  # transfer back to long format
  test_dict <- melt(topic_given_word_prob) # here we are using topic given word prob
  colnames(test_dict) <- names(beta)
  test_dict$term <- as.character(test_dict$term)
  test_dict$beta[which(!(test_dict$term %in% word_prob$term))] <- 0

  result <- data.frame()
  for (i in 1:nrow(df_long)) {
    score <- list()
    if (str_count(df_long$transcript[i]) == 0) {
      score[1:15] <- 0
    }
    else {
      temp <-
        read.table(
          text = df_long$transcript[i],
          sep = " ",
          fill = TRUE,
          as.is = TRUE
        ) %>% 
        t() %>% 
        data.frame()
      # the df_long has already been stemmed.
      wordn <- df_long$nword[i]
      temp <- temp %>%
        filter(temp$. %in% word_list)
      for (j in 1:15) {
        dict <- test_dict[test_dict$topic == j, ]
        merge <-
          merge(temp,
                dict,
                by.x = ".",
                by.y = "term",
                all = FALSE)
        # score[j] <- sum(merge[,3])/nrow(merge)
        score[j] <- sum(merge[, 3]) / wordn
      }
    }
    newrow <- data.frame(score)
    colnames(newrow) <- paste0("T", seq_len(15))
    result <- rbind(result, newrow)
  }
  
  # step 6 combine info
  ### Each participant’s scores were averaged for each meeting,
  ### effectively weighting each speaker’s contribution the same.
  ### These scores represent the average probability of a speaker’s utterances given a topic.
  df_info <- df_long[, -2] # utterance
  byspeaker <- cbind(df_info, result) # utterance and meeting id
  byspeaker$nword <- NULL
  by_speaker_meeting <-
    aggregate(. ~ id + speaker, data = byspeaker, mean) #average each speaker's utterances for each meeting
  private_bank <- speaker_info %>%
    select("speaker", "private_bank")
  private_bank$speaker <-
    str_remove_all(private_bank$speaker, "\\s")
  
  # check here
  by_speaker_meeting <-
    merge(
      by_speaker_meeting,
      private_bank,
      by = "speaker",
      all.x = T,
      all.y = F
    ) # add speaker info
  
  ## do the test
  test_result <- data.frame(estimate = c(),
                            p_value = c(),
                            V1 = c())
  for (i in 1:15) {
    result <-
      perm.t.test(by_speaker_meeting[, i + 2] ~ by_speaker_meeting$private_bank,
                  data = by_speaker_meeting)
    estimate <- result[['estimate']][2] - result[['estimate']][1]
    p_value <- result[['perm.p.value']][1]
    temp_result <-
      data.frame(estimate = estimate,
                 p_value = p_value,
                 V1 = i)
    test_result <- bind_rows(test_result, temp_result)
  }
  test_result$seed <- seed
  return(test_result)
}

stopCluster(cluster)
saveRDS(sensitivity_test,
        "./model_output/sensitivity_test_result.RDS")