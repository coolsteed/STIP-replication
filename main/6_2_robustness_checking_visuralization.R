# this file is for reading the sensitivity test result
# to check how topics behaviours across different topics. 

# lastly modified by Bolun 20231128

# distinguishing terms marked by x and topic score calculated by topic given word probability.

# setting the working environment
# load the related libs
library(tidyverse)
library(quanteda)
library(readtext)

load(file = "./model_output/results_mp_v4.RData")
sensitivity_data <- readRDS("./model_output/sensitivity_test_result.RDS")

sensitivity_test <- do.call(rbind.data.frame, sensitivity_data)

rs <- data.frame()
for(i in 1:200){
  rs <- rbind(rs, results_mp[[i]][[1]])
}

# redefine the pass 
rs$pass <- 0
rs$pass[which(rs$ja >= 0.15 & rs$overlap >=10)] <- 1

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

# combine the data
sensitivity_test <- sensitivity_test %>%
  left_join(., rs, by = c("V1", "seed"))

# check whether it is significant
sensitivity_test$sig <- 0
sensitivity_test$sig[which(sensitivity_test$p_value <= 0.05)] <- 1

# select the related result
# select v2 1, 4, 9, 12

all_topics_sensitivty_test <- sensitivity_test %>%
  filter(seed %in% rs_8$seed)

selected_sensitivity_result <- sensitivity_test %>%
  filter(V2 %in% c(1, 4, 9, 12)) 

replicated_model <- sensitivity_test %>%
  filter(seed == "3532")

mean(replicated_model$ja)
mean(replicated_model$overlap)


#### figure 2 ####
# Created by Yimang
# figure 2 observes the results of sensitivity test across the 32 all-similar model.
# The figure is drawed in 6_4, I cite the code here and save its result.

topic_label <- data.frame(V2 = 1:15, 
                          label = c("Bank Liquidity", "Housing", "Inflation", "Financial Markets", "Productivity",
                                  "Employment", "Weakness", "Portfolio", "Housing Bubble", "Energy", 
                                  "Models", "Policy Response", "Minutes", "Objectives", "General"))

sensitivity_test <- merge(sensitivity_test, topic_label, id = "V2")

figure2 <- sensitivity_test %>%
  filter(seed %in% rs_all$seed) %>%
  ggplot(data = ., mapping = aes(x = ja, y = estimate, group = label)) +
  geom_point(aes(colour = factor(sig))) +
  theme_bw() +
  scale_color_manual(
    name = "Permutation Test",  # Change the legend title
    breaks = c("1", "0"),
    labels = c("Significant", "Insignificant"),  # Change the legend labels
    values = c("gray", "black")
  ) +
  facet_wrap(label ~ .) +
  labs(x = "AJI", y = "Estimate") 
figure2

saveRDS(figure2, "./figures/fig_2_all_similar_models_sensitivity_test.RDS")

#### figure 3 ####

# Figure 3 is similar with Figure 2, but it observes the test results for replicated models with only 8 key topic passed

topic_label <- data.frame(V2 = 1:15, 
                          label = c("Bank Liquidity", "Housing", "Inflation", "Financial Markets", "Productivity",
                                    "Employment", "Weakness", "Portfolio", "Housing Bubble", "Energy", 
                                    "Models", "Policy Response", "Minutes", "Objectives", "General"))

sensitivity_test <- merge(sensitivity_test, topic_label, id = "V2")

figure3 <- sensitivity_test %>%
  filter(seed %in% rs_8$seed) %>%
  filter(V2 %in% c(1, 4, 9, 12, 3, 5, 6, 7)) %>%
  ggplot(data = ., mapping = aes(x = ja, y = estimate, group = label)) +
  geom_point(aes(colour = factor(sig))) +
  theme_bw() +
  scale_color_manual(
    name = "Permutation Test",  # Change the legend title
    breaks = c("1", "0"),
    labels = c("Significant", "Insignificant"),  # Change the legend labels
    values = c("gray", "black")
  ) +
  facet_wrap(label ~ .) +
  labs(x = "AJI", y = "Estimate") 

figure3

saveRDS(figure3, "./figures/fig_3_key_similar_models_sensitivity_test.RDS")

#### figure 4 ####

# Figure 4 takes "Productivity" as an example to show 1. the uncertainty of the results, 2. the comparison between all-similar and key-similar models.
# still, the data has existed in 6_4

library(ggrepel)

topic_label <- data.frame(V2 = 1:15, 
                          label = c("Bank Liquidity", "Housing", "Inflation", "Financial Markets", "Productivity",
                                    "Employment", "Weakness", "Portfolio", "Housing Bubble", "Energy", 
                                    "Models", "Policy Response", "Minutes", "Objectives", "General"))

sensitivity_test <- merge(sensitivity_test, topic_label, id = "V2")

productivity.key <- sensitivity_test %>%
  filter(label == "Productivity") %>%
  filter(seed %in% rs_8$seed) %>%
  mutate(type = "Key similarity")

productivity.all <- sensitivity_test %>%
  filter(label == "Productivity") %>%
  filter(seed %in% rs_all$seed) %>%
  mutate(type = "all similarity")

productivity.fig <- rbind(productivity.all, productivity.key)

figure4 <- productivity.fig %>%
  ggplot(data = ., mapping = aes(x = ja, y = estimate, group = type)) +
  geom_point(aes(colour = factor(sig))) +
  theme_bw() +
  scale_color_manual(
    name = "Permutation Test",  # Change the legend title
    breaks = c("1", "0"),
    labels = c("Significant", "Insignificant"),  # Change the legend labels
    values = c("gray", "black")
  ) +
  facet_wrap(~ type) + # Corrected facet_wrap syntax
  geom_text_repel(
    data = subset(productivity.key, seed %in% c(5403, 5678, 13680)), # 4366 - 13680
    aes(label = ifelse(seed %in% c(5403, 5678, 13680), c("B", "c", "A"), NA)), # Corrected label assignment
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  ) +
  labs(x = "AJI", y = "Estimate")

figure4

saveRDS(figure4, "./figures/fig_4_productivity_results.RDS")

# Now we want to determine some special points.
# the negatively significant model in all-similarity
figure4.negsig <- productivity.fig %>%
  filter(type == "all similarity") %>%
  filter(sig == 1) %>%
  filter(estimate < 0) %>%
  filter(ja == max(ja)) %>%
  select(seed)

figure4.possig <- productivity.fig %>%
  filter(type == "Key similarity") %>%
  filter(sig == 1) %>%
  filter(estimate > 0) %>%
  filter(ja == max(ja)) %>%
  select(seed)

save(figure4.negsig, figure4.possig, file = "./tables/figure4.table6.RData")

#### figure 5 ####

# Figure 5 is very similar with Figure 4, the only difference is that it takes "objectivity" as an example to show 1. the uncertainty of the results, 2. the comparison between all-similar and key-similar models.
# still, the data has existed in 6_4

library(ggrepel)

topic_label <- data.frame(V2 = 1:15, 
                          label = c("Bank Liquidity", "Housing", "Inflation", "Financial Markets", "Productivity",
                                    "Employment", "Weakness", "Portfolio", "Housing Bubble", "Energy", 
                                    "Models", "Policy Response", "Minutes", "Objectives", "General"))

sensitivity_test <- merge(sensitivity_test, topic_label, id = "V2")

objectives.all <- sensitivity_test %>%
  filter(label == "Objectives") %>%
  filter(seed %in% rs_all$seed) %>%
  mutate(type = "all similarity")

figure5 <- objectives.all %>%
  ggplot(data = ., mapping = aes(x = ja, y = estimate)) +
  geom_point(aes(colour = factor(sig))) +
  theme_bw() +
  scale_color_manual(
    name = "Permutation Test",  # Change the legend title
    breaks = c("1", "0"),
    labels = c("Significant", "Insignificant"),  # Change the legend labels
    values = c("gray", "black")
  ) + 
  geom_text_repel(
    data = subset(objectives.all, seed %in% c(19526, 9221, 12412)), # 15900 - 19526； 3365 - 12412
    aes(label = c("E", "f", "D")),
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines")
  ) +
  labs(x = "AJI", y = "Estimate") 

figure5
saveRDS(figure5, "./figures/fig_5_objective_results.RDS")

###############
figure5.possig <- objectives.all %>%
  filter(type == "all similarity") %>%
  filter(sig == 1) %>%
  filter(estimate > 0) %>%
  select(seed, ja)

figure5.negsig <- objectives.all %>%
  filter(type == "all similarity") %>%
  filter(sig == 1) %>%
  filter(estimate < 0) %>%
  filter(ja == max(ja)) %>%
  select(seed, ja)

save(figure5.negsig, figure5.possig, file="./tables/figure5.table6.RData")

#### table 6 ####

# table6 is to compare special models with the reference model
 source("0_functions.R")
 # load("./tables/figure4.table6.RData")
 # load("./tables/figure5.table6.RData")
 
 ## reading the results
 load(file = "./model_output/results_mp_v4.RData")
 rs <- data.frame()
 for(i in 1:200){
   rs <- rbind(rs, results_mp[[i]][[1]])
 }
 rs$pass <- 0
 rs$pass[which(rs$ja > .15 & rs$overlap >= 10)] <- 1
 
 extract.topic.by.seed <- function(seed, topic_index){
   s <- seed
   ti <- topic_index
   # reading in the replicated data
   fed_dfm_replicated <- readRDS("./process_files/replicated_dfm.RDS") # produced by replicating-corpus.R
   fed_topics_dfm <- convert(fed_dfm_replicated, to = "topicmodels") 
   # reading the lda
   filename <- paste0("./model_output/local_models/model_", as.character(seed), ".RDS")
   lda <- readRDS(filename)
   x <- ranked_by_x(rep_lda = lda, input_dfm = fed_topics_dfm, top=30)
   V1 <- rs %>%
     filter(seed == s) %>%
     filter(V2 == ti) %>%
     select(V1)
   rep.vector <- x[V1$V1]
   return(rep.vector)
 }
 
 fligstein_top <- readRDS("./process_files/fligstein_top_stemmed.RDS")
 
 
 ## figure4: A and B
 etbs <- extract.topic.by.seed
 # f4.neg.vector <- etbs(seed = figure4.negsig$seed, topic_index = 5) %>% data.frame
 # f4.pos.vector <- etbs(seed = figure4.possig$seed, topic_index = 5) %>% data.frame
 # f4.in.vector <- etbs(seed = figure4.insig$seed, topic_index =5) %>% data.frame
 # f5.neg.vector <- etbs(seed = figure5.negsig$seed, topic_index = 14) %>% data.frame
 # f5.pos.vector <- etbs(seed = figure5.possig$seed, topic_index = 14) %>% data.frame
 # f5.in.vector <- etbs(seed = figure5.insig$seed, topic_index =14) %>% data.frame
 
 f4.pos.vector <- etbs(seed = 5403, topic_index = 5) %>% data.frame
 f4.neg.vector <- etbs(seed = 5678, topic_index = 5) %>% data.frame
 f4.in.vector <- etbs(seed = 4366, topic_index =5) %>% data.frame
 f5.pos.vector <- etbs(seed = 15900, topic_index = 14) %>% data.frame
 f5.neg.vector <- etbs(seed = 9221, topic_index = 14) %>% data.frame
 f5.in.vector <- etbs(seed = 3356, topic_index =14) %>% data.frame

 
 flig5 <- data.frame(lapply(fligstein_top[5],tolower))
 flig14 <- data.frame(lapply(fligstein_top[14], tolower))
 
 table6 <- cbind(flig5, f4.neg.vector, f4.pos.vector, f4.in.vector, 
                 flig14, f5.neg.vector, f5.pos.vector, f5.in.vector)
 table6 <- data.frame(table6)
 colnames(table6) <- c("Productivity", "A", "B", "c","Objectivity", "D", "E", "f")
 # A is negative significant in figure 4, B is positive, C is insignificant; 
 # D is negative significant in figure 4, E is positive, F is insignificant

 write.csv(table6, "./tables/table6.csv") 
 saveRDS(table6, "./figures/table6_topcs_comparison.rds")


 