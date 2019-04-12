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

joined <- left_join(essays_features_nrc, essays_features_afinn)

score_data <- joined %>% 
  group_by(Id, sentiment) %>% 
  summarise(sums = sum(score, na.rm = TRUE)) %>% 
  spread(sentiment, sums, fill = 0)

 ##################
train_joined <- train_set %>% inner_join(proportions, by = c('vlogId' = 'Id')) %>% inner_join(score_data, by = c('vlogId' = 'Id'))

test_joined <-  test_set %>% inner_join(proportions, by = c('vlogId' = 'Id')) %>% inner_join(score_data, by = c('vlogId' = 'Id'))


####################

variables = train_joined %>% dplyr::select(-c(1:6, gender))

correlations <- cor(variables)

corrplot(correlations, method = 'circle')

eigenvalues <- eigen(correlations)

data_ext <- train_joined %>% dplyr::select(-c(vlogId, Agr:Open, gender))

control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'random')

model <- train(Extr ~(.), data = data_ext,method = 'lm',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )






