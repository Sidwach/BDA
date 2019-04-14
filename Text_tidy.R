library(tidyverse)
library(tokenizers)
library(tidytext)


# extract nrc features of all transcripts ---------------------------------
transcript_features_nrc <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) %>%
  count(Id, `sentiment`) %>%
  spread(sentiment, n, fill = 0)


# extract proportions of nrc features used and add total count ------------
proportions_nrc <- transcript_features_nrc %>% 
  mutate(sumVar = rowSums(.[,2:11])) %>% 
  mutate_at(vars(anger:trust), funs(./ sumVar))


# compute weighted sentiment sums with afinn scores -----------------------
# extract nrc features
features_nrc  <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  inner_join(get_sentiments('nrc'), by = c(token = 'word')) 

# extract afinn features
features_afinn <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  inner_join(get_sentiments('afinn'), by = c(token = 'word'))

# calculate the sum of afinn scores per feature
features_scored <- 
  left_join(features_nrc, features_afinn) %>% 
  group_by(Id, sentiment) %>% 
  summarise(sums = sum(score, na.rm = TRUE)) %>% 
  spread(sentiment, sums, fill = 0)



# additional predictors ---------------------------------------------------





# join new computed variables with train and test set ---------------------
train_joined <- train_set %>% 
  inner_join(proportions_nrc, by = c('vlogId' = 'Id')) %>% 
  inner_join(features_scored, by = c('vlogId' = 'Id'))

test_joined <- test_set %>% 
  inner_join(proportions_nrc, by = c('vlogId' = 'Id')) %>% 
  inner_join(features_scored, by = c('vlogId' = 'Id'))


# analysis ----------------------------------------------------------------
library(MASS)
library(caret)
library(corrplot)

# correlations
variables = train_joined %>% dplyr::select(-c(vlogId:Open, gender))

correlations <- cor(variables)

corrplot(correlations, method = 'circle')

# eigenvalues
eigenvalues <- eigen(correlations)$values
plot(eigenvalues)

# modeling

data_ext <- train_joined %>% dplyr::select(-c(vlogId, Agr:Open, gender))

control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'random')

model <- train(Extr ~(.), data = data_ext,method = 'lm',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )






