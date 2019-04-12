library(MASS)
library(tidyverse)
library(tokenizers)
library(tidytext)
library(caret)

##extract features for the training dataset
essays_features  <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(Id, `sentiment`) %>%
  spread(sentiment, n, fill = 0)

##join the fetures with the original dataset so that we have the 
#personality classfieier as well 
essays_features  <- 
  inner_join(essays, essays_features, by = "#AUTHID") %>%
  dplyr::select(-TEXT)

head(essays_features)


##a possible improvement? Looking at sentiment in proportion to the total
#words in the text 

##extracting proportions for essays (training set)
essays_features_sent  <- transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>% 
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(Id, `sentiment`) 

proportions <- essays_features_sent %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sumVar = rowSums(.[2:11])) %>% 
  mutate_at(vars(anger:trust), funs(./ sumVar)) 

##extracting tes

#######


essays_features_nrc  <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) 

essays_features_afinn <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('afinn'), by = c(token = 'word'))

joined <- inner_join(essays_features_nrc, essays_features_afinn)

score_data <- joined %>% 
  group_by(Id, sentiment) %>% 
  summarise(sum(score)) %>% 
  spread(sentiment, `sum(score)`, fill = 0)


scaled_score_data <- score_data %>% ungroup() %>% mutate_at(2:11, scale)

train_joined <- train_set %>% inner_join(proportions, by = c('vlogId' = 'Id')) %>% inner_join(scaled_score_data, by = c('vlogId' = 'Id'))

test_joined <-  test_set %>% inner_join(proportions, by = c('vlogId' = 'Id')) %>% inner_join(scaled_score_data, by = c('vlogId' = 'Id'))


test_set %>% anti_join(test_joined, by = 'vlogId')

anti_join(testset,score_data_test)






