## ---------------------------
## Title: NLP Needs & Wants Functions
## ---------------------------
## Author: Dan Fellowes
## Date Created: 2022-05-27
## ---------------------------

library(tidytext)
library(textstem)
library(stopwords)
library(tidyverse)
library(openNLP)
library(NLP)

needs_wants_stats <- function(dat, col) {
  
  # make column variable usable within function
  col <- enquo(col)
  dat <- dat %>% rename(text_col = !!col)
  
  # word filters
  need_filt <- c("need", "needs", "required", "needed", "needing", "must", "require")
  want_filt <- c("want", "wants", "wanting", "wanted", "wishing", "wished", "wish")
  
  # extract sentences from text
  dat_need_words <-
    dat %>% # extract sentences
    mutate(id = seq(1:n())) %>%
    unnest_tokens(word, text_col) %>% # give unique sentence IDs
    filter(word %in% need_filt) %>%
    group_by(filter, id) %>%
    distinct(id) %>%
    group_by(filter) %>%
    summarise(type_count = n()) %>%
    mutate(type = "need")
  
  dat_want_words <-
    dat %>% # extract sentences
    mutate(id = seq(1:n())) %>%
    unnest_tokens(word, text_col) %>% # give unique sentence IDs
    filter(word %in% want_filt) %>%
    group_by(filter, id) %>%
    distinct(id) %>%
    group_by(filter) %>%
    summarise(type_count = n()) %>%
    mutate(type = "want")
  
  dat_type_count <- rbind(dat_want_words, dat_need_words)
  
  return(dat_type_count)
}

needs_wants <- function(dat, col, pos = "") {
  
  # make column variable usable within function
  col <- enquo(col)
  dat <- dat %>% rename(text_col = !!col)
  
  negative_words <- c("no", "not", "none", "nobody", "nothing", "neither",
                      "nowhere", "never", "hardly", "scarcely", "barely",
                      "doesn't", "isn't", "wasn't", "shouldn't", "wouldn't",
                      "couldn't", "won't", "can't", "don't", "without")
  
  # word filters
  need_filt <- c("need", "needs", "required", "needed", "needing", "require", "requires", "requiring")
  want_filt <- c("want", "wants", "wanting", "wanted", "wishing", "wished", "wish", "wishes")
  
  need_filter <- "need|needs|needed|needing|require|requires|required|requiring"
  want_filter <- "want|wants|wanting|wanted|wishes|wishing|wished|wish"
  
  # extract sentences from text
  dat_ngram_sentence <-
    dat %>%
    unnest_sentences(sentence, text_col) %>% # extract sentences
    mutate(sent_id = seq(1:n())) # give unique sentence IDs
  
  # extract words from sentences
  dat_ngram <- dat_ngram_sentence %>%
    unnest_ngrams(tri_gram, sentence, n = 3) %>% # unnest groups of 3 words
    separate(tri_gram, into = c("word_1", "word_2", "word_3"), sep = " ")
  
  #:::::::
  # NEEDS
  #:::::::
  
  need_ngram_sentiments <-
    dat_ngram %>%
    # Determine if the word is negated, and reverse sentiment if so
    mutate(is_preceding_negative = as.numeric((word_2 %in% negative_words | word_2 %in% data_stopwords_nltk$en & word_1 %in% negative_words))) %>%
    # Remove stop words
    filter(!(word_3 %in% data_stopwords_nltk$en),
           word_3 %in% need_filt) %>%
    # Switch from bigram to word as token
    select(-word_1, -word_2) %>%
    rename(word = word_3) %>%
    mutate(word = ifelse(is_preceding_negative == 1, paste("NEG_",word), word))
  
  # get all sent_ids containing needs
  needs_sent_all <- need_ngram_sentiments %>%
    group_by(sent_id) %>%
    summarise(negative = mean(is_preceding_negative))
  
  # filter out negated need words
  needs_sent_id <- needs_sent_all %>%
    filter(negative == 0)
  
  # filter only negated needs
  needs_sent_id_negate <- needs_sent_all %>%
    filter(negative == 1)
  
  # select "need" sentences
  sent_select <- dat_ngram_sentence %>%
    filter(sent_id %in% needs_sent_id$sent_id)
  
  # select negated "need" sentences
  sent_select_negate <- dat_ngram_sentence %>%
    filter(sent_id %in% needs_sent_id_negate$sent_id)
  
  ## token annotator setup
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  
  if (nrow(sent_select) > 0) {
    s <- as.String(sent_select$sentence)
    
    a2 <- NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
    a3 <- NLP::annotate(s, pos_tag_annotator, a2)
    
    ## Determine the distribution of POS tags for word tokens.
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    
    needs_fin <- data.frame(word = s[a3w],
                            pos_tag = tags) %>%
      filter(!(word %in% data_stopwords_nltk$en), !(grepl(paste(need_filter), word))) %>%
      mutate(word = lemmatize_strings(word)) %>%
      group_by(word, pos_tag) %>%
      summarise(freq = n(),
                type = "need")
  }
  
  if (nrow(sent_select_negate) > 0) {
    # negated needs
    s_neg <- as.String(sent_select_negate$sentence)
    
    a2_neg <- NLP::annotate(s_neg, list(sent_token_annotator, word_token_annotator))
    a3_neg <- NLP::annotate(s_neg, pos_tag_annotator, a2_neg)
    
    ## Determine the distribution of POS tags for word tokens.
    a3w_neg <- subset(a3_neg, type == "word")
    tags_neg <- sapply(a3w_neg$features, `[[`, "POS")
    
    needs_fin_negate <- data.frame(word = s_neg[a3w_neg],
                                   pos_tag = tags_neg) %>%
      filter(!(word %in% data_stopwords_nltk$en), !(grepl(need_filter, word))) %>%
      mutate(word = lemmatize_strings(word)) %>%
      group_by(word, pos_tag) %>%
      summarise(freq = n(),
                type = "NEG_need")
  }
  
  # code for when I want to add in negated words
  
  # if (exists("needs_fin") & exists("needs_fin_negate") == TRUE) {
  #   needs_all_fin <- rbind(needs_fin, needs_fin_negate)
  # } else if (exists("needs_fin") == TRUE) {
  #   needs_all_fin <- needs_fin
  # } else if (exists("needs_fin_negate") == TRUE) {
  #   needs_all_fin <- needs_fin_negate
  # }
  
  if (exists("needs_fin") == TRUE) {
    needs_all_fin <- needs_fin
  }
  
  
  #:::::::
  # WANTS
  #:::::::
  
  want_ngram_sentiments <-
    dat_ngram %>%
    # Determine if the word is negated, and reverse sentiment if so
    mutate(is_preceding_negative = as.numeric((word_2 %in% negative_words | word_2 %in% data_stopwords_nltk$en & word_1 %in% negative_words))) %>%
    # Remove stop words (including title words)
    filter(!(word_3 %in% data_stopwords_nltk$en),
           word_3 %in% want_filt) %>%
    # Switch from bigram to word as token
    select(-word_1, -word_2) %>%
    rename(word = word_3) %>%
    mutate(word = ifelse(is_preceding_negative == 1, paste("NEG_",word), word))
  
  # get all sent_ids containing wants
  wants_sent_all <- want_ngram_sentiments %>%
    group_by(sent_id) %>%
    summarise(negative = mean(is_preceding_negative))
  
  # filter out negated want words
  wants_sent_id <- wants_sent_all %>%
    filter(negative == 0)
  
  # filter only negated wants
  wants_sent_id_negate <- wants_sent_all %>%
    filter(negative == 1)
  
  # select "want" sentences
  wants_sent_select <- dat_ngram_sentence %>%
    filter(sent_id %in% wants_sent_id$sent_id)
  
  # select negated "want" sentences
  wants_sent_select_negate <- dat_ngram_sentence %>%
    filter(sent_id %in% wants_sent_id_negate$sent_id)
  
  ## token annotator setup
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  
  if (nrow(wants_sent_select) > 0) {
    
    s <- as.String(wants_sent_select$sentence)
    
    a2 <- NLP::annotate(s, list(sent_token_annotator, word_token_annotator))
    a3 <- NLP::annotate(s, pos_tag_annotator, a2)
    
    ## Determine the distribution of POS tags for word tokens.
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    
    wants_fin <- data.frame(word = s[a3w],
                            pos_tag = tags) %>%
      filter(!(word %in% data_stopwords_nltk$en), !(grepl(want_filter, word))) %>%
      mutate(word = lemmatize_strings(word)) %>%
      group_by(word, pos_tag) %>%
      summarise(freq = n(),
                type = "want")
  }
  
  if (nrow(wants_sent_select_negate) > 0) {
    # negated wants
    s_neg <- as.String(wants_sent_select_negate$sentence)
    
    a2_neg <- NLP::annotate(s_neg, list(sent_token_annotator, word_token_annotator))
    a3_neg <- NLP::annotate(s_neg, pos_tag_annotator, a2_neg)
    
    ## Determine the distribution of POS tags for word tokens.
    a3w_neg <- subset(a3_neg, type == "word")
    tags_neg <- sapply(a3w_neg$features, `[[`, "POS")
    
    wants_fin_negate <- data.frame(word = s_neg[a3w_neg],
                                   pos_tag = tags_neg) %>%
      filter(!(word %in% data_stopwords_nltk$en), !(grepl(want_filter, word))) %>%
      mutate(word = lemmatize_strings(word)) %>%
      group_by(word, pos_tag) %>%
      summarise(freq = n(),
                type = "NEG_want")
  }
  
  # code for when I want to add in negated words
  
  # if (exists("wants_fin") & exists("wants_fin_negate") == TRUE) {
  #   wants_all_fin <- rbind(wants_fin, wants_fin_negate)
  # } else if (exists("wants_fin") == TRUE) {
  #   wants_all_fin <- wants_fin
  # } else if (exists("wants_fin_negate") == TRUE) {
  #   wants_all_fin <- wants_fin_negate
  # }
  
  if (exists("wants_fin") == TRUE) {
    wants_all_fin <- wants_fin
  }
  
  if (exists("needs_all_fin") & exists("wants_all_fin") == TRUE) {
    ret <- rbind(needs_all_fin, wants_all_fin) %>% arrange(-freq) %>% filter(grepl(pos, pos_tag))
  } else if (exists("needs_all_fin") == TRUE) {
    ret <- needs_all_fin %>% arrange(-freq) %>% filter(grepl(pos, pos_tag))
  } else if (exists("wants_all_fin") == TRUE) {
    ret <- wants_all_fin %>% arrange(-freq) %>% filter(grepl(pos, pos_tag))
  }
  
  return(ret)
  
}


# visualisation prep process function

nlp_need_want <- function(com_df, text_col, group_col, top_words = 50, include_other = TRUE) {
  
  # make text_col variable usable within function
  text_col <- enquo(text_col)
  com_df <- com_df %>% rename(comments = !!text_col)
  
  # make group_col variable usable within function
  group_col <- enquo(group_col)
  com_df <- com_df %>% rename(filter = !!group_col)
  
  # choose "part of sentence" types: noun == NN, verb == VB, adjective == JJ
  pos_filt <- c("NN|VB|JJ")
  
  com_df <- com_df %>%
    mutate(comments = paste0(comments, ".")) # add full stop to help with extracting to end of sentence
  
  # summarise by group
  filt_stats <- com_df %>%
    group_by(filter) %>%
    summarise(comment_count = n()) %>%
    left_join(needs_wants_stats(com_df, comments), by = "filter") %>%
    mutate(type_perc = type_count / comment_count)
  
  #
  filt_stats_noperc <- filt_stats %>%
    select(-type_perc)
  
  # create blank dataframe
  filt_df <- data.frame()
  
  # analyse each group individually and combine together  
  for (i in 1:length(unique(com_df$filter))) {
    
    dat <- com_df %>% filter(filter == unique(com_df$filter)[i]) # select relevant group
    
    if (nrow(dat) > 0) {
      
      # run needs_wants analysis and add group col back in
      filt_i <- needs_wants(dat, comments, pos_filt) %>% 
        mutate(filter = unique(com_df$filter)[i])
      
      # append results to filt_df
      if (nrow(filt_df) == 0) {
        filt_df <- data.frame(filt_i)
      } else {
        filt_df <- rbind(data.frame(filt_df), data.frame(filt_i))
      }
    }
  }
  
  # select words longer than 2 letters and ignore negated wants/needs - may change later
  filt_df_sel <- filt_df %>%
    filter(type %in% c("need", "want"),
           nchar(word) > 2)
  
  # summary ignoring pos type
  filt_no_pos <- filt_df_sel %>% 
    group_by(word, type, filter) %>%
    summarize(freq = sum(freq))
  
  # join and reformat data to display correctly in output plot
  grouped_stats <- filt_no_pos %>% 
    left_join(filt_stats, by = c("filter", "type")) %>%
    mutate(comm_perc = freq / comment_count) %>%
    arrange(-comm_perc) %>%
    group_by(type, filter) %>%
    mutate(sum_com = sum(comm_perc)) %>%
    ungroup() %>%
    mutate(comm_perc = (comm_perc / sum_com) * type_perc) %>%
    select(-sum_com)
  
  # summarise by words to find top x words
  word_stats <- grouped_stats %>%
    group_by(word, type) %>%
    summarize(total_comm_perc = mean(comm_perc),
              total_type_perc = mean(type_perc)) %>%
    arrange(-total_comm_perc, -total_type_perc)
  
  # find top x most frequent words
  top_x <- grouped_stats %>%
    filter(word %in% word_stats$word[1:top_words])
  
  # group all other words into one row
  if (include_other == TRUE) {
    top_x_na <- grouped_stats %>%
      filter(!(word %in% word_stats$word[1:top_words])) %>%
      group_by(type, filter) %>%
      summarise(word = "Other words",
                freq = sum(freq),
                comment_count = mean(comment_count),
                type_count = mean(type_count),
                type_perc = mean(type_perc),
                comm_perc = sum(comm_perc))
    
    # combine top and other words
    top_x <- rbind(top_x, top_x_na)
  }

  # reformat top x word data
  top_x_adj <- top_x %>%
    filter() %>%
    group_by(filter, type) %>%
    mutate(type_sum = sum(comm_perc)) %>%
    group_by(filter, word, type) %>%
    mutate(comm_perc_adj = comm_perc / type_sum) %>%
    select(word, type, filter, comm_perc) %>%
    pivot_wider(id_cols = c(word, type), names_from = "filter", values_from = "comm_perc", values_fill = 0)
  
  # create final plot data
  want_need_plot <- top_x_adj %>%
    filter(type %in% c("want", "need")) %>%
    pivot_longer(cols = c(-word, -type), names_to = "filter", values_to = "comm_perc") %>%
    group_by(word) %>%
    mutate(label = as.factor(ifelse(comm_perc != max(comm_perc), "", ifelse(comm_perc < 0.001, "", word))),
           type = factor(type, levels = c("want", "need"), labels = c("WANT", "NEED")),
           word = as.factor(word),
           filter = as.factor(filter))
  
  return(want_need_plot)
  
}


# facet labeller for plot labels
fac_labeller <- function(variable,value){
  return(facet_labs[value])
}