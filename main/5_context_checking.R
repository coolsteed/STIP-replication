# This is for figure 1
# figure 1 is for the context checking mission
# In this figure, Fligstein's lda15 is compared with the replicated models in terms of their top docs
# And figure 1 is a boxplot to show the AJIs of these comparisons.
## Coded by Yimang on 2023/12/2, before R&R submission

# preparation
library(tidytext)
library(tidyverse)
library(ggplot2)
## load average_jaccard(), lda2list(), ranked_by_x and Counter()
source("0_functions.R") 


#### load Fligstein's best model lda15 ####
load("./other_data/FOMClda.RData")
# get the top term as they presented in Table 1 of the paper

#### first we get gamma according to the topic.proportions functions #### 
gamma <- t(lda15$document_sums / colSums(lda15$document_sums)) %>% data.frame
## fligstein documents ranked by gamma
gamma$document <- paste0("text", row.names(gamma))
fligstein_doc <- gamma %>%
  gather( topic, gamma, 1:15, factor_key=TRUE) %>% 
  arrange(topic, desc(gamma))
fligstein_doc_list <- split(fligstein_doc, fligstein_doc$topic)
## test: fligstein_doc_list[5] means V5
fligstein_doc_list[5]


#### then we a top term based on beta ####
## get top terms and their beta values
top_terms <- as.data.frame(lda15$topics %>% t) 
top_terms$word <- row.names(top_terms)
## This is a function to extract top n terms from each column
get_top_n_terms <- function(df, n = 30) {
  df %>%
    gather(key = "topic", value = "frequency", -word) %>%
    group_by(topic) %>%
    top_n(n, wt = frequency) %>%
    ungroup() %>%
    arrange(topic, desc(frequency))
}
## top_30_terms is top terms by beta 
### (it's ok it is different from Table 2 of Fligstein's paper, which is based on X)
top_30_beta <- get_top_n_terms(top_terms) # it has 453 results because of the same frequency, I don't think it will be a problem
#### There is a problem: top_30_beta should be ordered as V1, V2, V3... V15
top_30_beta$topic <- factor(top_30_beta$topic,
                            levels = paste0("V",1:15))
# Combine the top terms into a list
## It should be based on beta rather than X, because the replicated lda will be based on beta, too.
fligstein_lda_vector <- split(top_30_beta$word, top_30_beta$topic)
## test: fligstein_lda_vector[5] means V5
fligstein_lda_vector[5]


#### we calculate the X for future use and to confirm our understanding of the process ####
# The purpose of this step is to use X to confirm the column names #
# wpgt is beta
word.prob.given.topic <- t(apply(lda15$topics, 1, function(x) x/(sum(x))))
# topic.proportions is gamma
topic.proportions <- t(lda15$document_sums) / colSums(lda15$document_sums)
# topic.prob is the proportion of topics across all documents 
topic.prob <- rowSums(lda15$document_sums) / sum(lda15$document_sums)
# word.topic.prob is wpgt multiplies topic.prob
## it can be understood as weighted beta
word.topic.prob <- apply(word.prob.given.topic, 2, function(wpgt) wpgt*topic.prob)
# word.prob sums columns of wtp
word.prob <- colSums(word.topic.prob)
# topic.prob.given.word is the distribution of topics across a word
topic.prob.given.word <- t(apply(word.topic.prob, 1, function(wtp) wtp/word.prob))
# X is tpgw multiplies beta
X <- topic.prob.given.word * word.prob.given.topic
# table 1
Xt <- t(X) %>% data.frame
Xt$word <- row.names(Xt)
top_30_X <- get_top_n_terms(Xt)

table1 <- top_30_X$word[top_30_X$topic == paste0("X", 1)]
for(i in 2:15){
  new.col <- top_30_X$word[top_30_X$topic == paste0("X", i)]
  table1 <- cbind(table1, new.col)
}

## The result is the same with Fligstein paper's Table 1 but in a different order

topic_name <- c("Bank\nLiquidity", "General", "Employment", "Weakness","Financial\nMarkets",
                "Models","Objectives","Housing","Inflation","Portfolio",
                "Productivity", "Energy","Housing\nBubble","Policy\nResponse","Minutes")
topic_label <- cbind(V1=1:15, label = topic_name)

# #### BTW, Bolun's way makes the same X #####
# # wpgt is beta
# word.prob.given.topic <- t(apply(lda15$topics, 1, function(x) x/(sum(x))))
# # topic.proportions is gamma
# topic.proportions <- t(lda15$document_sums) / colSums(lda15$document_sums)
# doc_prob <- colSums(lda15$document_sums) / sum(lda15$document_sums)
# word_prob_given_doc <- topic.proportions  %*% word.prob.given.topic
# word_prob <- t(word_prob_given_doc) %*% doc_prob
# topic_prob <- t(topic.proportions) %*% doc_prob
# ## calculate the tpgw using baysian law
# topic_given_word_prob <- apply(word.prob.given.topic, 2, function(beta) beta * topic_prob)
# topic_given_word_prob <- apply(topic_given_word_prob, 1, function(x) x / word_prob) %>% t()
# X <- word.prob.given.topic * topic_given_word_prob
# # table 1
# t1 <- t(X)

##### read replicated ldas ##############
## read the results of beta comparison between fligstein lda and replicated lda
## from 4_best_match_v4.R
load(file = "./model_output/results_mp_v4_231230.RData")
rs <- data.frame()
for(i in 1:200){
  rs <- rbind(rs, results_mp[[i]][[1]])
}
remove(results_mp)
## select models with 8 key topics passed by V2
rs_8 <- rs %>%
  filter(V2 %in% c(1, 4, 9, 12, 3, 5, 6, 7)) %>% 
  group_by(seed) %>%
  summarise(pass_all = sum(pass)) %>%
  filter(pass_all == 8)
## select models with all topics passed
rs_all <- rs %>%
  group_by(seed) %>%
  summarise(pass_all = sum(pass)) %>%
  filter(pass_all == 15)

##### Calculate AJI for docs based on gamma ################
## read the selected models  
filenames <- paste0("model_", rs_all$seed,".RDS" )
## If Topic A from rep_lda is matched with Topic a from best_lda via top 30 terms,
## we calculate the AJI by the top 5 and top 10 docs of the two topics.
### top_doc is defined as 5
top_doc <- 5

library(doParallel)
library(foreach)
cores <- detectCores(logical = TRUE) - 2 # leave two CPU spare...
cluster <- makeCluster(cores)
registerDoParallel(cluster)

results_top10 <- foreach(i = 1:length(filenames), .packages = c("tidytext", "dplyr", "tidyr")) %dopar% {
  rep_lda <- readRDS(paste0("./model_output/local_models/", filenames[i]))
  rep_lda_docs <- tidytext::tidy(rep_lda, matrix = "gamma")
  rep_lda_docs_list <- split(rep_lda_docs, rep_lda_docs$topic)
  rep_lda_docs_list <- lapply(rep_lda_docs_list, function(df) dplyr::arrange(df, desc(gamma)))
  # compare the replicated lda with the fligstein lda
  # Be aware V1,V2 here is different from their meanings in the rs results
  rep_lda_vector <- lda2list(rep_lda, 30)
  pair_result <- Counter(fligstein_lda_vector, rep_lda_vector, 30)
  result_rows <- data.frame()  # Initialize an empty dataframe for this iteration
  for(j in 1:15){
    V1 <- j
    V2 <- pair_result$V2[pair_result$V1==V1]
    doc_AJ <- average_jaccard(fligstein_doc_list[[V1]]$document, rep_lda_docs_list[[V2]]$document, top = top_doc)
    overlap_file <- intersect(fligstein_doc_list[[V1]]$document[1:top_doc], rep_lda_docs_list[[V2]]$document[1:top_doc])
    overlap <- length(overlap_file)
    n_column <- data.frame(filename = filenames[i], V1 = V1, doc_AJ = doc_AJ, overlap = overlap, overlap_file = I(list(overlap_file)))
    result_rows <- dplyr::bind_rows(result_rows, n_column)  # Accumulate rows
  }
  return(result_rows)
}
stopCluster(cluster)
## Combine the results from all iterations into a single dataframe
final_results_top10 <- dplyr::bind_rows(results_top10)
final_results_top10 <- merge(final_results_top10, topic_label, id = "V1")

### top_doc is defined as 5
top_doc <- 10

library(doParallel)
library(foreach)
cores <- detectCores(logical = TRUE) - 2 # leave two CPU spare...
cluster <- makeCluster(cores)
registerDoParallel(cluster)

results_top10 <- foreach(i = 1:length(filenames), .packages = c("tidytext", "dplyr", "tidyr")) %dopar% {
  rep_lda <- readRDS(paste0("./model_output/local_models/", filenames[i]))
  rep_lda_docs <- tidytext::tidy(rep_lda, matrix = "gamma")
  rep_lda_docs_list <- split(rep_lda_docs, rep_lda_docs$topic)
  rep_lda_docs_list <- lapply(rep_lda_docs_list, function(df) dplyr::arrange(df, desc(gamma)))
  # compare the replicated lda with the fligstein lda
  # Be aware V1,V2 here is different from their meanings in the rs results
  rep_lda_vector <- lda2list(rep_lda, 30)
  pair_result <- Counter(fligstein_lda_vector, rep_lda_vector, 30)
  result_rows <- data.frame()  # Initialize an empty dataframe for this iteration
  for(j in 1:15){
    V1 <- j
    V2 <- pair_result$V2[pair_result$V1==V1]
    doc_AJ <- average_jaccard(fligstein_doc_list[[V1]]$document, rep_lda_docs_list[[V2]]$document, top = top_doc)
    overlap_file <- intersect(fligstein_doc_list[[V1]]$document[1:top_doc], rep_lda_docs_list[[V2]]$document[1:top_doc])
    overlap <- length(overlap_file)
    n_column <- data.frame(filename = filenames[i], V1 = V1, doc_AJ = doc_AJ, overlap = overlap, overlap_file = I(list(overlap_file)))
    result_rows <- dplyr::bind_rows(result_rows, n_column)  # Accumulate rows
  }
  return(result_rows)
}
stopCluster(cluster)
## Combine the results from all iterations into a single dataframe
final_results_top10 <- dplyr::bind_rows(results_top10)
final_results_top10 <- merge(final_results_top10, topic_label, id = "V1")

test <- final_results_top10 %>%
  filter(label == "Employment") 

#### Visualization for figure 1 ####

## We decide to show the overlapped number of the top 10 documents for each topic of  all-similar replicated models and Fligstein et al.'s model

# ## Select the 8 key topics using best_lda_key8 
# topic_8 <- c("Bank Liquidity", "Financial Markets", "Housing Bubble", "Policy Response",
#              "Inflation", "Productivity", "Employment", "Weakness")
# key8_results_top5 <- final_results_top5[final_results_top10$label %in% topic_8, ]
# key8_results_top10 <- final_results_top10[final_results_top10$label %in% topic_8, ]


library(ggplot2)
library(hexbin)

figure1 <- ggplot(final_results_top10, aes(x = label, y = overlap)) +
  geom_count() +
  labs(x = "Topic", y = "Frequency of Overlapped Document Number Among Top 10") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) + 
  scale_size_continuous(name = "Number of Local Models")

figure1

saveRDS(figure1, "./figures/fig_1_context_checking.RDS")
