library(MASS)
library(tidyverse)
library(tokenizers)
library(tidytext)
library(caret)


##import the data
dir("../",recursive = T)
testset <-  read_csv("essaytest.csv", locale = locale(encoding = 'ISO-8859-1'))
essays <-  read_csv("essaytrain.csv", locale = locale(encoding = 'ISO-8859-1'))

##extract features for the training dataset
essays_features  <- 
  essays %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(`#AUTHID`, `sentiment`) %>%
  spread(sentiment, n, fill = 0)

##join the fetures with the original dataset so that we have the 
#personality classfieier as well 
essays_features  <- 
  inner_join(essays, essays_features, by = "#AUTHID") %>%
  dplyr::select(-TEXT)

head(essays_features)

##extract testset features
testset_features <- 
  testset %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(`#AUTHID`, `sentiment`) %>%
  spread(sentiment, n, fill = 0)

head(testset_features)

##a possible improvement? Looking at sentiment in proportion to the total
#words in the text 

##extracting proportions for essays (training set)
essays_features_sent  <- essays %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>% 
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(`#AUTHID`, `sentiment`) 

proportions <- essays_features_sent %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sumVar = rowSums(.[3:11])) %>% 
  mutate_at(vars(anger:trust), funs(./ sumVar)) 

essays_features_proportion  <- 
  inner_join(essays, proportions, by = "#AUTHID") %>%
  dplyr::select(-TEXT)


##extracting testset proportions
essays_test_proportions  <- testset %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>% 
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(`#AUTHID`, `sentiment`) 

proportions_test <- essays_test_proportions %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sumVar = rowSums(.[3:11])) %>% 
  mutate_at(vars(anger:trust), funs(./ sumVar)) 

#######


essays_features_nrc  <- 
  essays %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) 

essays_features_afinn <- 
  essays %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('afinn'), by = c(token = 'word'))

joined <- inner_join(essays_features_nrc, essays_features_afinn)

score_data <- joined %>% 
  group_by(`#AUTHID`, sentiment) %>% 
  summarise(sum(score)) %>% 
  spread(sentiment, `sum(score)`, fill = 0)


score_data  <- 
  inner_join(essays, score_data, by = "#AUTHID") %>%
  dplyr::select(-TEXT)

testset_features_nrc  <- 
  testset %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) 

testset_features_afinn <- 
  testset %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>%
  inner_join(get_sentiments('afinn'), by = c(token = 'word'))

joined_test <- inner_join(testset_features_nrc, testset_features_afinn)

score_data_test <- joined_test %>% 
  group_by(`#AUTHID`, sentiment) %>% 
  summarise(sum(score)) %>% 
  spread(sentiment, `sum(score)`, fill = 0)


score_data_test  <- 
  inner_join(testset, score_data_test, by = "#AUTHID") %>%
  dplyr::select(-TEXT)


essays_test_proportions  <- testset %>%
  unnest_tokens(token, TEXT, token = 'words') %>%
  anti_join(get_stopwords(), by = c(token = 'word')) %>% 
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(`#AUTHID`, `sentiment`) %>% 
  spread(sentiment, n, fill = 0)

proportions_test <- essays_test_proportions %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sumVar = rowSums(.[3:11])) %>% 
  mutate_at(vars(anger:trust), funs(./ sumVar)) 


data_predict <- inner_join(score_data_test, proportions_test, by = "#AUTHID")

anti_join(testset,score_data_test)






