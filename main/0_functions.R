library(pacman)
p_load(pdftools, tidyverse, stringr, quanteda, dplyr, tidytext, ggplot2,
       topicmodels, RcppHungarian, seewave, haven, reshape2)

# reading the stop words
# these two files are from fligstein 2017 replication package
stop_list_replication <- read_csv("./other_data/Rstopwords.txt", 
                                  col_names = FALSE)
vocab_replication <- read_csv("./other_data/vocab_origin.csv",
                              col_names = FALSE)
names(vocab_replication) <- c("vocab")
names(stop_list_replication) <- c("stopwords")

#################FUNC 0#############################
# define related function
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection / union)
}

average_jaccard <- function(a, b, top) {
  score <- 0
  for (i in 1:top) {
    temp_a = head(a, i)
    temp_b = head(b, i)
    temp_jaccard <- jaccard(temp_a, temp_b)
    score <- score + temp_jaccard
  }
  return(score / top)
}
#######################################################

# ################FUNC 1###############################
# A function that turns LDA to a list of top_n terms 
lda2list <- function(lda_model, top){
  topics <- tidy(lda_model, matrix = "beta")
  topterms <- topics %>%
    group_by(topic) %>%
    top_n(top, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  rep_vector <- topterms %>%
    mutate(topic = paste0('X',topterms$topic)) %>%
    group_by(topic) %>%
    mutate(row = row_number()) %>%
    filter(row <=top) %>% # here remove extra words with the same beta. The author did not mention this in their article
    select(-beta) %>%
    spread(topic,term)
  
  # rearrange the col names so that the sequence of the topic won't change
  rep_vector <- rep_vector %>% 
    select(row, num_range("X", range = 1:ncol(rep_vector))) %>%
    ungroup() %>%
    select(-row) %>%
    as.vector() %>%
    lapply(as.character)

  return(rep_vector)
}

# ################## FUNC 2 ################################
# A function that counts the overlapping terms
Counter <- function(rep_vector, origin_vector,top) {
  
  ## count the UNIDENTICAL words between two vectors, this is the cost of the matching
  count_matrix <- matrix(nrow = length(rep_vector), ncol = length(origin_vector))
  for(i in 1:length(rep_vector)){
    for(j in 1:length(origin_vector)){
      score <- 0
      for (k in 1:top) {
        overlapped <- length(intersect(head(unlist(rep_vector[i]), n = k),
                                       head(unlist(origin_vector[j]), n = k)))
        score_round <- overlapped/(k*2-overlapped)
        score <- score + score_round
      }
      count_matrix[i,j] <- 1 - (score / top) 
    }
  }
  
  ## use RcppHungarian to count the minimum
  library(RcppHungarian)
  
  output_match <- HungarianSolver(count_matrix)
  output_match
  pair_result <- output_match$pairs %>% as.data.frame()
  pair_result$ja <- 0
  pair_result$overlap <- 0
  for (i in 1:nrow(pair_result)) {
    if (pair_result$V2[i] != 0) {
      pair_result$ja[i] <- 1- count_matrix[pair_result$V1[i],pair_result$V2[i]]
      
      pair_result$overlap[i] <- length(intersect(unlist(rep_vector[pair_result$V1[i]]),
                                                 unlist(origin_vector[pair_result$V2[i]])))
    }
  }
  pair_result$pass <- 0
  pair_result$pass[which(pair_result$ja >= 0.15 & pair_result$overlap >= top/3)] <- 1
  return(pair_result)
}

# ################## FUNC 3 ######
# a function that turns text in dataframe to dfm
df2dfm <- function(df){
  corpus <- quanteda::corpus(df)
  
  # change to new quanteda version's grammar. 
  dfm <- tokens(corpus, remove_punct = TRUE) %>%
    dfm() %>%
    dfm_remove(., stopwords("english")) %>%
    dfm_wordstem()

  dfm <- dfm_trim(dfm,
                  min_termfreq = 4,
                  termfreq_type = c("count"),
                  verbose = TRUE)
  return(dfm)
}

# ################## FUNC 3.1 ######
# a function that turns text in dataframe to dfm
# after the verbal description, using the stoplist from the replication package

df2dfm_v2 <- function(df){
  corpus <- quanteda::corpus(df)
  
  # change to new quanteda version's grammar. 
  dfm <- tokens(corpus, remove_punct = TRUE) %>%
    dfm() %>%
    dfm_remove(., stop_list_replication$stopwords) %>%
    dfm_wordstem()
  
  dfm <- dfm_trim(dfm,
                  min_termfreq = 4,
                  termfreq_type = c("count"),
                  verbose = TRUE)
  return(dfm)
}

# ################## FUNC 3.3 ######
# a function that turns text in dataframe to dfm
# replace the trim with a vocabulary list 

df2dfm_v3 <- function(df){
  corpus <- quanteda::corpus(df)
  
  # change to new quanteda version's grammar. 
  dfm <- tokens(corpus, remove_punct = TRUE) %>%
    dfm() %>%
    dfm_wordstem() %>%
    dfm_keep(vocab_replication$vocab)
  
  return(dfm)
}

# ################# FUNC 4 ######
# this is a function comparing local alignment and global alignment
# reference for selecting selecting specific topic
# reference_topic <- c(1, 4, 9, 12)
# 
# origin_vector <- fligstein_top %>%
#   select(num_range("X", reference_topic)) %>%
#   lapply(str_to_lower) %>%
#   lapply(as.character) %>%
#   lapply(tm::stemDocument)

compare_align <- function(rep_vector, origin_vector, top, reference_topic) {
  # constructing a cost matrix 
  # this part is the same in the Counter.
  # difference is exchange the position of row and col
  # origin_vector is the reference model
  # rep_vector is the comparison model
  count_matrix <- matrix(nrow = length(origin_vector), ncol = length(rep_vector))
  for(i in 1:length(origin_vector)){
    for(j in 1:length(rep_vector)){
      score <- 0
      for (k in 1:top) {
        overlapped <- length(intersect(head(unlist(origin_vector[i]), n = k),
                                       head(unlist(rep_vector[j]), n = k)))
        score_round <- overlapped/(k*2-overlapped)
        score <- score + score_round
      }
      count_matrix[i,j] <- 1 - (score / top) 
    }
  }
  
  # local alignment 
  local_align <- cbind(1:nrow(count_matrix),    # row
                       max.col(-count_matrix)) %>%
    as.data.frame()
  
  for (i in 1:nrow(local_align)) {
    local_align$local_aj[i] <- 1 - count_matrix[local_align$V1[i], local_align$V2[i]]
  }
  
  names(local_align) <- c("reference", "comparison_local", "local_aj")
  local_align$reference <- reference_topic
  
  # global alignment
  output_match <- HungarianSolver(count_matrix)
  global_align <- output_match$pair %>% as.data.frame()
  for (i in 1:nrow(global_align)) {
    global_align$global_aj[i] <- 1 - count_matrix[global_align$V1[i], global_align$V2[i]]
  }
  names(global_align) <- c("reference", "comparison_global", "global_aj")
  global_align$reference <- reference_topic
  
  # comparing two
  compare <- local_align %>% 
    left_join(.,global_align, by = c("reference"))
  
  conflict <- compare %>%
    filter(comparison_local != comparison_global) 
  topic_conflict <- conflict$comparison_local %>% as.vector()
  
  output <- compare %>%
    filter(comparison_local %in% topic_conflict)
  
  return(output)
}

# ################## FUNC 5 ##############
# A function that estimates the similarity of two lda models 
# using the methods from Roberts et al.2016
pairwise <- function(lda1, lda2, top, method = c("product", "l1", "l2", "kl")){
  # some preparation work
  term1 <- tidytext::tidy(lda1, matrix = "beta")
  term2 <- tidytext::tidy(lda2, matrix = "beta")
  
  top_term1 <- term1 %>%
    group_by(topic) %>%
    arrange(topic, beta) %>%
    mutate(n = min_rank(desc(beta))) %>%
    filter(n <= top)
  
  top_term2 <- term2 %>%
    group_by(topic) %>%
    arrange(topic, beta) %>%
    mutate(n = min_rank(desc(beta))) %>%
    filter(n <= top)
  
  k <- max(term1$topic)
  if(k != max(term2$topic)){
    print("two ldas should use an identical k")
    break} # Bolun：why we need identical k here?
  item <- 0
  matrix <- matrix(nrow = k, ncol = k)
  l1_matrix <- matrix(nrow = k, ncol = k)
  aj_matrix <- matrix(nrow = k, ncol = k)
  kl_matrix <- matrix(nrow = k, ncol = k)
  
  # similarity matrix
  for(i in 1:k){
    for(j in 1:k){
      list1 <- filter(term1, topic == i)
      list2 <- filter(term2, topic == j)
      combine <- merge(list1, list2, by = "term")
      if(anyNA(combine))
      {print("two ldas should use an identical corpus")
        break}
      else{
        if(method == "l1"){item <- sum(abs(combine$beta.x-combine$beta.y))} # l1 dis
        else if (method == "l2") {item <- (sum((combine$beta.x-combine$beta.y)^2))^(1/2)} # l2 dis
        else if (method == "kl") {
          kl <- kl.dist(combine$beta.x, combine$beta.y, base = 2)
          item <- kl[[3]] # kl distance, note the formula in steyvers and griffiths is printed wrong
        }
        else {item <- 1-t(combine$beta.x)%*%combine$beta.y} # inner product
      }
      matrix[i,j] <- item
    }
  }
  output_match <- HungarianSolver(matrix)
  
  pair_result <- output_match$pairs %>% as.data.frame()
  pair_result$overlap <- 0
  for (i in 1:nrow(pair_result)) {
    if (pair_result$V2[i] != 0) {
      pair_result$pair[i] <- matrix[pair_result$V1[i],pair_result$V2[i]]
      list1 <- filter(top_term1, topic == i)
      list2 <- filter(top_term2, topic == pair_result$V2[i])
      pair_result$overlap[i] <- nrow(na.omit(merge(list1, list2, by = "term", 
                                                   all.x = FALSE, all.y = FALSE)))
    }
  }
  return(pair_result)
}

# ################## FUNC 5 ##############
# #this is a function to add different distance measurement given the pairing result
caluculate_dis <- function(lda1, lda2, pair_result){
  term1 <- tidytext::tidy(lda1, matrix = "beta")
  term2 <- tidytext::tidy(lda2, matrix = "beta")
  
  rep_vector <- lda2list(lda1,30) # here might need to be changed
  origin_vector <- lda2list(lda2,30)
  
  k <- max(term1$topic)
  g <- max(term2$topic)
  if(k != max(term2$topic)){
    print("two ldas should use an identical k")
    break}
  item <- 0
  l1_matrix <- matrix(nrow = k, ncol = g)
  l2_matrix <- matrix(nrow = k, ncol = g)
  aj_matrix <- matrix(nrow = k, ncol = g)
  pro_matrix <- matrix(nrow = k, ncol = g)
  kl_matrix <- matrix(nrow = k, ncol = g)
  
  # adding a l1_distance matrix
  for(i in 1:k) {
    for(j in 1:g) {
      list1 <- filter(term1, topic == i)
      list2 <- filter(term2, topic == j)
      combine <- merge(list1, list2, by = "term")
      if(anyNA(combine))
      {print("two ldas should use an identical corpus")
        break}
      else{
        item <- sum(abs(combine$beta.x-combine$beta.y))
      }
      l1_matrix[i,j] <- item
    }
  }
  
  # adding a l2_distance matrix
  for(i in 1:k){
    for(j in 1:g){
      list1 <- filter(term1, topic == i)
      list2 <- filter(term2, topic == j)
      combine <- merge(list1, list2, by = "term")
      if(anyNA(combine))
      {print("two ldas should use an identical corpus")
        break}
      else{
        item <- (sum((combine$beta.x-combine$beta.y)^2))^(1/2)
      }
      l2_matrix[i,j] <- item
    }
  }
  
  # adding a aj matrix 
  for(i in 1:k){
    for(j in 1:g){
      score <- 0
      for (h in 1:30) {
        overlapped <- length(intersect(head(unlist(rep_vector[i]), n = h),
                                       head(unlist(origin_vector[j]), n = h)))
        score_round <- overlapped/(h*2-overlapped)
        score <- score + score_round
      }
      aj_matrix[i,j] <- 1 - (score / 30) 
    }
  }
  
  # adding a inner product matrix
  for(i in 1:k){
    for(j in 1:g){
      list1 <- filter(term1, topic == i)
      list2 <- filter(term2, topic == j)
      combine <- merge(list1, list2, by = "term")
      if(anyNA(combine))
      {print("two ldas should use an identical corpus")
        break}
      else{
        item <- t(combine$beta.x)%*%combine$beta.y
      }
      pro_matrix[i,j] <- item
    }
  }
  
  # adding a kl distance matrix
  for(i in 1:k){
    for(j in 1:g){
      list1 <- filter(term1, topic == i)
      list2 <- filter(term2, topic == j)
      combine <- merge(list1, list2, by = "term")
      if(anyNA(combine))
      {print("two ldas should use an identical corpus")
        break}
      else{
        kl <- kl.dist(combine$beta.x, combine$beta.y, base = 2)
        item <- kl[[3]]
      }
      kl_matrix[i,j] <- item
    }
  }
  
  pair_result$l1 <- 0
  pair_result$l2 <- 0
  pair_result$aj <- 0
  pair_result$pro <- 0
  pair_result$kl <- 0
  for (i in 1:nrow(pair_result)) {
    if (pair_result_aj$V2[i] != 0) {
      pair_result$l1[i] <- l1_matrix[pair_result$V1[i],pair_result$V2[i]]
      pair_result$l2[i] <- l2_matrix[pair_result$V1[i],pair_result$V2[i]]
      pair_result$aj[i] <- 1 - aj_matrix[pair_result$V1[i],pair_result$V2[i]]
      pair_result$pro[i] <- pro_matrix[pair_result$V1[i],pair_result$V2[i]]
      pair_result$kl[i] <- kl_matrix[pair_result$V1[i],pair_result$V2[i]]
    }
  }
  
  return(pair_result)
}

# ################## FUNC 6 ##############
# This function is compare different local alignment using different 
# measurement, instead of comparing them using the global alignment.
local_align <- function(reference_model, comparison_model, method = c("product", "l1", "l2", "kl", "aj")) {
  
  # some preparation work
  top <- 30
  term1 <- tidytext::tidy(reference_model, matrix = "beta")
  term2 <- tidytext::tidy(comparison_model, matrix = "beta")
  
  ref_vector <- lda2list(reference_model,top) # here might need to be changed
  comp_vector <- lda2list(comparison_model,top)
  
  k <- max(term1$topic)
  g <- max(term2$topic)
  
  counter_matrix <- matrix(nrow = k, ncol = g)
  
  # calculate the l1 distance
  if (method == "l1") {
    for(i in 1:k) {
      for(j in 1:g) {
        list1 <- filter(term1, topic == i)
        list2 <- filter(term2, topic == j)
        combine <- merge(list1, list2, by = "term")
        if(anyNA(combine))
        {print("two ldas should use an identical corpus")
          break}
        else{
          item <- sum(abs(combine$beta.x-combine$beta.y))
        }
        counter_matrix[i,j] <- item
      }
    }
  }
  
  # adding a l2_distance matrix
  if (method == "l2") {
    for(i in 1:k){
      for(j in 1:g){
        list1 <- filter(term1, topic == i)
        list2 <- filter(term2, topic == j)
        combine <- merge(list1, list2, by = "term")
        if(anyNA(combine))
        {print("two ldas should use an identical corpus")
          break}
        else{
          item <- (sum((combine$beta.x-combine$beta.y)^2))^(1/2)
        }
        counter_matrix[i,j] <- item
      }
    }
  }
  
  # adding a aj matrix 
  if (method == "aj") {
    for(i in 1:k){
      for(j in 1:g){
        score <- 0
        for (h in 1:top) {
          overlapped <- length(intersect(head(unlist(ref_vector[i]), n = h),
                                         head(unlist(comp_vector[j]), n = h)))
          score_round <- overlapped/(h*2-overlapped)
          score <- score + score_round
        }
        counter_matrix[i,j] <- 1 - (score / top) 
      }
    }
  }
  
  # adding a inner product matrix
  if (method == "product") {
    for(i in 1:k){
      for(j in 1:g){
        list1 <- filter(term1, topic == i)
        list2 <- filter(term2, topic == j)
        combine <- merge(list1, list2, by = "term")
        if(anyNA(combine))
        {print("two ldas should use an identical corpus")
          break}
        else{
          item <- t(combine$beta.x)%*%combine$beta.y
        }
        counter_matrix[i,j] <- item
      }
    }
  }
  
  # adding a kl distance matrix
  if (method == "kl") {
    for(i in 1:k){
      for(j in 1:g){
        list1 <- filter(term1, topic == i)
        list2 <- filter(term2, topic == j)
        combine <- merge(list1, list2, by = "term")
        if(anyNA(combine))
        {print("two ldas should use an identical corpus")
          break}
        else{
          kl <- kl.dist(combine$beta.x, combine$beta.y, base = 2)
          item <- kl[[3]]
        }
        counter_matrix[i,j] <- item
      }
    }
  }
  
  # local alignment 
  local_align <- cbind(1:nrow(counter_matrix),    # row
                       max.col(-counter_matrix)) %>%
    as.data.frame()
  
  # get the value of the measurement for this alignment
  if (method == "aj") {
    for (i in 1:nrow(local_align)) {
      local_align$value[i] <- 1 - counter_matrix[local_align$V1[i], local_align$V2[i]]}
  } else {
    for (i in 1:nrow(local_align)) {
      local_align$value[i] <- counter_matrix[local_align$V1[i], local_align$V2[i]]
    }
  } 
  
  return(local_align)
}

##### FUNC 7 ###############
# This is a function to get the distinguishing term using X in Fligstein 2017
# X is calculated by 


ranked_by_x <- function(rep_lda, input_dfm, top){
  
  # to matrix
  dfm <- as.matrix(input_dfm)
  
  # calculated the doc prob using observed data
  doc_prob <- rowSums(dfm)/sum(dfm)
  
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
  
  X <- beta_wide * topic_given_word_prob
  
  # transfer back to long format
  test_dict <- melt(X)
  colnames(test_dict) <- names(beta)
  test_dict$term <- as.character(test_dict$term)
  
  # get the top terms
  top_terms_ranked_x <- test_dict %>%
    group_by(topic) %>%
    arrange(desc(beta)) %>%
    slice_head(n = 30) %>%
    summarise(terms = list(as.character(term)))
  
  return(top_terms_ranked_x$terms)
}
