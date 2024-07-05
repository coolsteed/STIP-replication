# filter the closest LDA model
#### making table2 ####

## table 2 is to evaluate the replication of all-similarity models
## originally by Bolun with slight revision of Yimang

library(pacman)
p_load(pdftools, tidyverse,stringr,quanteda,dplyr,tidytext,ggplot2,topicmodels,
       RcppHungarian, reshape2)
source("0_functions.R")

load(file = "./model_output/results_mp_v4.RData")
fed_dfm_replicated <- readRDS("./process_files/replicated_dfm.RDS") # produced by replicating-corpus.R
fed_topics_dfm <- convert(fed_dfm_replicated, to = "topicmodels") 

rs <- data.frame()
for(i in 1:200){
  rs <- rbind(rs, results_mp[[i]][[1]])
}

topics <- c("Bank Liquidity", "Housing", "Inflation", "Financial Markets",
            "Productivity", "Employment", "Weakness", "Portfolio",
            "Housing Bubble", "Energy", "Models", "Policy Response",
            "Minutes", "Objectives", "General")
rs_all <- rs %>%
  group_by(seed) %>%
  summarise(pass_all = sum(pass)) %>%
  filter(pass_all == 15)

rs_selected <- rs %>%
  filter(rs$seed %in% rs_all$seed)

fligstein_top <- readRDS("./process_files/fligstein_top_stemmed.RDS")
fligstein_top <- lapply(fligstein_top, tolower) %>% data.frame

reference_topic <- c(1:15)
origin_vector <- fligstein_top %>%
  select(num_range("X", reference_topic)) %>%
  lapply(str_to_lower) %>%
  lapply(as.character)

# landmark words used in Fligstein et al.

t1 <- c("inflation","tslf","pdcf","reserve", "banks") # bank liquidity
t2 <- c("housing","growth","labor",
        "residential","construction",
        "builders","mortgage","subprime", "home") # housing 
t3 <- c("prices","inflation","energy",
        "core","compensation","expectations") # inflation
t4 <- c("mortgage","loans","liquidity","credit",
        "cdo","tranche","turmoil","capital",
        "asset") # financial market
t5 <- c("productivity","labor","workers",
        "technology","nairu","unemployment",
        "wage","prices","pressures","growth",
        "supply","demand") # Productivity
t6 <- c("employment","hiring","growth",
        "output","business","spending",
        "labor","capital","job") # employment
t7 <- c("recession","weakness","decline",
        "ulus","rebound","downside",
        "negative") # weakness
t8 <- c("treasury","mae","freddie",
        "securities","sovereign",
        "debt","outright","rps",
        "collateral","portfolio","securities",
        "asset","collateral","liquidity") # portfolio
t9 <- c("rent","land","home","properties",
        "lenders", "overvalue", "loans",
        "bond") # housing bubble
t10 <- c("gasoline","energy","iraq",
         "barrel", "crude", "heating") # energy
t11 <- c("chart","model","simulations",
         "variables", "line", "panel") # models
t12 <- c("target","zero","program",
         "sheet","rate","tools",
         "policy", "interest") # policy response
t13 <- c("minutes","release","statement",
         "public","announcement","decision") # minutes
t14 <- c("congress","dual","mandate","price",
         "stability","inflation") # objectives
t15 <- c("rate","point","market","expectations") # general

lm_vector <- list(t1,t2,t3,t4,t5,
                  t6,t7,t8,t9,t10,
                  t11,t12,t13,t14,t15)

lm_vector <- lm_vector%>%
  lapply(as.character) %>%
  lapply(tm::stemDocument)

#dfm <- readRDS("./process_files/replicated_dfm.RDS")

#topics_dfm <- convert(dfm, to = "topicmodels")

# filter the best
library(doParallel)
library(foreach)

## making a cluster
cores <- detectCores(logical = TRUE) - 2  # leave one CPU spare...
cluster <- makeCluster(cores)
registerDoParallel(cluster)

registerDoParallel(cluster)
# loading related packages
clusterEvalQ(cluster, {
  library(topicmodels)
  library(tidyverse)
  library(tidytext)
  library(quanteda)
  library(reshape2)
})

results_match <- data.frame(V2 = c(0),V1 = c(0), top5 = c(0),landmark_match = c(0), landmark_total = c(0),seed = c(0))
results_match <- results_match[-1,] 

best_match <- foreach(j = 1:length(rs_all$seed)) %dopar% {
  seed <- rs_all$seed[j]
  
  ## save the replicated model's lda
  filename <- paste0("./model_output/local_models/model_",as.character(seed))
  filename <- paste0(filename,".RDS")
  rep_lda <- readRDS(filename)
  
  rep_vector <- ranked_by_x(rep_lda,fed_topics_dfm,30)
  pair_result <- Counter(rep_vector, origin_vector, 30)
  
  rep_topics <- tidy(rep_lda, matrix = "beta")
  rep_top_terms <- rep_topics %>%
    group_by(topic) %>%
    top_n(30, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  origin_vector_top5 <- lapply(origin_vector, head, 5)
  
  for(i in 1:15){
    v1 <- pair_result$V1[pair_result$V2==i] 
    V2 <- i
    top5 <- length(intersect(unlist(rep_top_terms$term[rep_top_terms$topic==v1]),
                             unlist(origin_vector_top5[i])))
    landmark_match <- length(intersect(unlist(rep_top_terms$term[rep_top_terms$topic==v1]),
                                       unlist(lm_vector[i])))
    landmark_total <- length(unlist(lm_vector[i]))
    aj <- pair_result$ja[pair_result$V2==i]
    overlap <- pair_result$overlap[pair_result$V2==i]
    newrow <- c(V2,v1, top5,landmark_match,landmark_total,aj,overlap,seed)
    results_match <- rbind(results_match,newrow)
  }
  return(results_match)
}

stopCluster(cluster)

matched_result <- data.frame()

for(i in 1:length(rs_all$seed)){
  temp <- best_match[[i]]
  names(temp) <- c("V2","V1", "top5", "landmark_match","landmark_total","aj","overlap","seed")
  matched_result <- bind_rows(matched_result, temp)
}

matched_result <- matched_result %>%
  mutate(landmark_rate = landmark_match/landmark_total)

check_all <- matched_result %>%
  group_by(seed) %>%
  summarise(mean_landmark = mean(landmark_rate), mean_top5 = mean(top5), min = min(landmark_rate))

check_all_v2 <- rs_selected %>% 
  group_by(seed) %>%
  summarise(mean_ja = mean(ja), mean_overlap = mean(overlap), 
            min_ja = min(ja), min_overlap = min(overlap))

check_selected <- matched_result %>%
  filter(matched_result$V2 %in% c(1, 4, 9, 12, 3, 5, 6, 7)) %>%
  group_by(seed) %>%
  summarise(mean_landmark = mean(landmark_rate), mean_top5 = mean(top5), min = min(landmark_rate))

# to make a table using the following data frame
check_by_topics <- matched_result %>%
  group_by(V2) %>%
  summarise(mean_landmark = mean(landmark_rate), max_landmark = max(landmark_rate), mean_top5 = mean(top5), 
            mean_aj = mean(aj), mean_overlap = mean(overlap))

names(check_by_topics) <- c("Topic","Mean Landmark Rate", "Maximum Landmark Rate",
                            "Mean Top 5","Mean AJ Index", "Mean Overlap")

check_by_topics$Topic <- topics

write.csv(check_by_topics, "./tables/table2.csv", 
          row.names	= FALSE)
saveRDS(check_by_topics, "./figures/table2_replication_summary.RDS")

