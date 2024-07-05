# this file is to check whether hypothesis validation might be able to distinguishing models with different results.

# Figure 6 compares the trend of "Energy" over time, 
## We use both the Fligstein topic and the all-similar topics.

#### use the document info to get date info for docs ###
library(pacman)
p_load(stringr,lubridate,tidyverse)
load("./process_files/cleaned_fed_docs.RData")
doc_date <- fed_transcript %>%
  dplyr::select(id, date)
doc_date$date <- stringr::str_replace(doc_date$date, "–.*?,",",")
doc_date$date <- stringr::str_replace(doc_date$date, "-.*?,",",")
doc_date$date <- lubridate::mdy(doc_date$date)
doc_date$id <- paste0("text", doc_date$id)


##### first, we get the gamma for Fligstein model ####
## this is based on 11_1_fig1.R

load("./Fligstein_et_al_2017_Replication_Package/FOMClda.RData")
temp <- t(lda15$document_sums) %>% data.frame
fligstein.gamma <- temp / rowSums(temp) 

## The labels the same with Fligstein paper's Table 1 but in a different order

topic_name <- c("Bank Liquidity", "General", "Employment", "Weakness","Financial Markets",
                "Models","Objectives","Housing","Inflation","Portfolio",
                "Productivity", "Energy","Housing Bubble","Policy Response","Minutes")
colnames(fligstein.gamma) <- topic_name
fligstein.gamma <- cbind(fligstein.gamma, date = doc_date$date)

fligstein.gamma.long <- fligstein.gamma %>%
  pivot_longer(cols = 1:15, names_to = "topic")
fligstein.gamma.long$seed <- "Reference"
colnames(fligstein.gamma.long) <- c("date","topic","gamma","seed")

#topic_label <- cbind(V1=1:15, label = topic_name)


##### Second, we get the gamma from all-similar topics.

load(file = "./model_output/results_mp_v4_231230.RData")
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

## get gamma 
## read the selected models  
filenames <- paste0("model_", rs_all$seed,".RDS" )
filenames <- list.files("./model_output/local_models")
rep_gamma <- data.frame()
for(filename in filenames){
  rep_lda <- readRDS(paste0("./model_output/local_models/", filename))
  gamma <- tidytext::tidy(rep_lda,"gamma")
  ## gamma$document与doc_date$id匹配，以加入时间数据
  gamma <- merge(gamma, doc_date, by.x = "document", by.y = "id")
  ## 加入seed信息
  gamma$seed <- stringr::str_extract(filename,"\\d+")
  ## 汇总所有的seeds
  rep_gamma <- rbind(rep_gamma, gamma)
}

rep_gamma$seed <- as.numeric(rep_gamma$seed)

rep_gamma <- rep_gamma %>%
  left_join(., rs, by = c("topic" = "V1", "seed" = "seed")) 

rep_gamma$topic <- NULL

## gamma$topic has not been matched
## match the rep_gamma$topic with label using v2

label <- c("Bank Liquidity", "Housing", "Inflation", "Financial Markets",
            "Productivity", "Employment", "Weakness", "Portfolio",
            "Housing Bubble", "Energy", "Models", "Policy Response",
            "Minutes", "Objectives", "General")
labels <- cbind(label, V2 = 1:15) 
rep_gamma <- merge(rep_gamma, labels, by = "V2")

#change the col name to topic
names(rep_gamma)[names(rep_gamma) == "V2"] <- "topic"

gamma <- rep_gamma %>%
  select(label, gamma, seed, date)
colnames(gamma) <- c("topic","gamma","seed","date")

gamma.long <- rbind(gamma, fligstein.gamma.long)

############ select a perfect replicated model ######
### in the model, all permutation tests are permutation and all topics are passed

sensitivity_data <- readRDS("./model_output/sensitivity_test_result_231230.RDS")
sensitivity_test <- do.call(rbind.data.frame, sensitivity_data)
# combine the data
sensitivity_test <- sensitivity_test %>%
  left_join(., rs, by = c("V1", "seed"))


# check whether it is significant
sensitivity_test$sig <- 0
sensitivity_test$sig[which(sensitivity_test$p_value <= 0.05)] <- 1

## search for a perfect model
## "bank liquidity" is positively significant
bl.sig <- sensitivity_test %>%
  filter(V2 == 1) %>%
  filter(estimate >= 0) %>%
  filter(sig == 1) %>%
  select(seed)
## "financial market" is positively significant
fm.sig <- sensitivity_test %>%
  filter(V2 == 4) %>%
  filter(estimate >= 0) %>%
  filter(sig == 1) %>%
  select(seed)
## "housing bubble" is positively significant
hb.sig <- sensitivity_test %>%
  filter(V2 == 9) %>%
  filter(estimate >= 0) %>%
  filter(sig == 1) %>%
  select(seed)
##  "policy response" is positively significant 
pr.sig <- sensitivity_test %>%
  filter(V2 == 12) %>%
  filter(estimate >= 0) %>%
  filter(sig == 1) %>%
  select(seed)
## perfect model(s) having all topics passed and four topics significant in tests
all.pass <- rs_all %>%
  select(seed)

## three results, randomly select one
set.seed(231204)
perfect.seed <-  Reduce(intersect, c(all.pass, bl.sig, fm.sig, hb.sig, pr.sig)) %>% data.frame %>% sample_n(1)

## In sig.seed, at least one topic is significant. The table reports freq.
sig.seed <- rbind(bl.sig, fm.sig, hb.sig, pr.sig) %>% table 

## select some not so good model 
## select a model from rs_8 with Energy passed
set.seed(12345)
energy.pass <- rs %>%
  filter(rs$seed %in% rs_8$seed) %>%
  filter(V2 == 10) %>%
  filter(pass == 1) %>%
  select(seed) 

energy.pass.sig <- merge(energy.pass, sig.seed, id = "seed") 

##  select a seed
set.seed(123)
figure6seed <- energy.pass.sig$seed[energy.pass.sig$Freq==1] %>% sample(1)
figure6seed %in% bl.sig$seed
figure6seed %in% fm.sig$seed
figure6seed %in% hb.sig$seed
figure6seed %in% pr.sig$seed

## Figure 6 shows the trends of Energy in the full replication and a model where Energy passes the rule but housing bubble is not significant


gamma.figure6 <- gamma.long[gamma.long$seed %in% c("Reference", perfect.seed, figure6seed),] %>%
  filter(topic == "Energy")

gamma.figure6$seed[gamma.figure6$seed==perfect.seed[1,1]] <- "Full Replication"
gamma.figure6$seed[gamma.figure6$seed==figure6seed] <- "Insignificant Replication"

figure6 <- gamma.figure6 %>%
  ggplot(aes(x = date, y = gamma, group = seed)) +
  geom_line() +
  facet_wrap(~seed, scales = "free_y", ncol = 1) +
  labs(title = "Gamma Trends Across Date for Topic Energy in Three Models",
       x = "Date",
       y = "Gamma") +
  theme_bw()

figure6

saveRDS(figure6, "./figures/fig_6_energy_results.RDS")

## Figure 7 is similar with Figure 6, but it shows for all models that housing bubble is replicated, 
## it is not visually distinguishable whether a model is significant or not. 
gamma.figure7 <- gamma.long[gamma.long$seed %in% rs_8$seed, ] %>%
  filter(topic == "Housing Bubble")
gamma.figure7$sig <- "Insignifianct"
hb.neg.sig <- sensitivity_test %>%
  filter(V2 == 9) %>%
  filter(estimate < 0) %>%
  filter(sig == 1) %>%
  select(seed) # empty
gamma.figure7$sig[gamma.figure7$seed %in% hb.sig$seed] <- "Significant"
figure7 <- gamma.figure7 %>% 
  ggplot(aes(x = date, y = gamma, linetype = sig)) +
  geom_line() +
  labs(title = "Gamma Trends Across Date for Topic Housing Bubble in Key-Similar Models",
       x = "Date",
       y = "Gamma",
       linetype = "Permutation Test") +
  theme_bw()
figure7
saveRDS(figure7, "./figures/fig_7_housing_policy_results.RDS")