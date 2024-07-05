# this file is to clarify what we have discussed in 07/29
# we use the same exact corpus from fligstein 2017

# setting the working environment

library(pdftools)
library(tidyverse)
library(stringr)
library(quanteda)
library(dplyr)
library(tidytext)
library(ggplot2)
library(topicmodels)
library(RcppHungarian)

setwd("~/Documents/GitHub/seeing-like-a-topic-model")

# sourcing functions and related corpus
source("0_functions.R")
source("1_replicating-corpus.R")

# load the replicated corpus
fed_dfm_replicated <- readRDS("./process_files/replicated_dfm.RDS")

fligstein_top <- readRDS("./process_files/fligstein_top_stemmed.RDS")

fed_topics_dfm <- convert(fed_dfm_replicated, to = "topicmodels")

# finding a replicated model
library(doParallel)
library(foreach)

# generate random seeds
set.seed("92037")
seeds <- sample(1:20000,200, replace = FALSE) # change here after running the whole script

# making a cluster
cores <- detectCores(logical = TRUE) - 2# leave two CPU spare...
cluster <- makeCluster(cores)
registerDoParallel(cluster)

# loading related packages
clusterEvalQ(cluster, {
  library(topicmodels)
  library(tidyverse)
  library(tidytext)
  library(quanteda)
})

results_mp_matching <- data.frame(V1 = c(0), V2 = c(0), pass = c(0), ja = c(0), 
                                  overlap = c(0), seed = c(0))
results_mp_confound <- data.frame(reference = c(0), comparison_global = c(0), 
                                  comparison_local = c(0), global_aj = c(0), 
                                  local_aj = c(0), seed = c(0))
reference_topic <- c(1:15)

origin_vector <- fligstein_top %>%
  select(num_range("X", reference_topic)) %>%
  lapply(str_to_lower) %>%
  lapply(as.character)

clusterExport(cluster, c("fed_topics_dfm", "seeds", "reference_topic"))
system.time({
  results_mp <- foreach(j = 1:length(seeds)) %dopar% {
    seed <- seeds[j]
    result <- data.frame()
    result_confound <- data.frame()
    lda_result <- LDA(fed_topics_dfm, 
                      k = 15, 
                      method= "Gibbs",
                      control = list(seed = seed,alpha = 50/15, 
                                     delta = 0.2, burnin = 400))
    rep_vector <- lda2list(lda_result,30)
    pair_result <- Counter(rep_vector, origin_vector, 30)
    confound <- compare_align(rep_vector, origin_vector, 30, reference_topic)
    
    # get the global result
    result <- pair_result
    result$seed <- seed
    # get the confounding result
    if (nrow(confound)!= 0) { # the condition here is necessary
      result_confound <- confound
      result_confound$seed <- seed
    }
    
    output <- list(result,result_confound)
    return(output)
  }
}
)

# stop the cluster#
stopCluster(cluster)

# get the results
for (i in 1:length(results_mp)) {
  temp <- results_mp[[i]]
  match <- temp[1] %>% as.data.frame()
  confound <- temp[2] %>% as.data.frame()
  results_mp_matching <- bind_rows(results_mp_matching, match)
  results_mp_confound <- bind_rows(results_mp_confound, confound)
}

results_mp_matching <- results_mp_matching %>%
  filter(seed != 0)
results_mp_confound <- results_mp_confound %>%
  filter(seed != 0)

save(results_mp, file = "./model_output/results_mp_v4.RData")