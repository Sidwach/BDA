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

# Remove outliers!

findOutlier <- function(matrix, cutoff = 3) {
  ## Calculate the sd
  sds <- apply(matrix, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s) {
    which(d - mean(d) > cutoff * s)
  }, matrix, sds)
  result
}

outliers <- findOutlier(data_ext)
outliers_to_remove <- unlist(outliers) %>% unique()

data_ext_no_out <- data_ext %>% slice(-outliers_to_remove)



# EXPERIMENT 15 highest correlating variables

data_ext_experimental <- data_ext %>% dplyr::select(order(cor(data_ext)[,1], decreasing = TRUE)[1:15])

outliers_experimental <- findOutlier(data_ext_experimental)
outliers_to_remove_experimental <- unlist(outliers_experimental) %>% unique()

data_ext_experimental_noOut <- data_ext_experimental %>% slice(-c(outliers_to_remove))


control <- trainControl(method = 'repeatedcv', number = 5, repeats = 3, search = 'random')

model_ext <- train(Extr ~ ., data = data_ext_experimental_noOut,method = 'leapBackward',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )


### OTHER PERSONALITIES

# Agr 

data_Agr <- train_joined %>% dplyr::select(-c(vlogId, Extr, Cons:Open, gender))

data_Agr_exp  <- data_Agr %>% dplyr::select(order(cor(data_Agr)[,1], decreasing = TRUE)[1:15])

Agr_outliers <- findOutlier(data_Agr) %>% unlist() %>% unique()

data_Agr_exp_noout <- data_Agr_exp %>% slice(-Agr_outliers)

model_Agr <- train(Agr ~ ., data = data_Agr_exp_noout,method = 'leapBackward',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )

# Cons 

data_Cons <- train_joined %>% dplyr::select(-c(vlogId, Extr,Agr, Emot,Open, gender))

data_Cons_exp  <- data_Cons %>% dplyr::select(order(cor(data_Cons)[,1], decreasing = TRUE)[1:15])

Cons_outliers <- findOutlier(data_Cons) %>% unlist() %>% unique()

data_Cons_exp_noout <- data_Cons_exp %>% slice(-Cons_outliers)

model_Cons <- train(Cons ~ ., data = data_Cons_exp_noout,method = 'leapBackward',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )


# Emot

data_Emot <- train_joined %>% dplyr::select(-c(vlogId, Extr:Cons, Open, gender))

data_Emot_exp  <- data_Emot %>% dplyr::select(order(cor(data_Emot)[,1], decreasing = TRUE)[1:15])

Emot_outliers <- findOutlier(data_Emot) %>% unlist() %>% unique()

data_Emot_exp_noout <- data_Emot_exp %>% slice(-Emot_outliers)

model_Emot <- train(Emot ~ ., data = data_Emot_exp_noout,method = 'leapBackward',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )


# Open

data_Open <- train_joined %>% dplyr::select(-c(vlogId, Extr:Cons, gender))

data_Open_exp  <- data_Open %>% dplyr::select(order(cor(data_Open)[,1], decreasing = TRUE)[1:15])

Open_outliers <- findOutlier(data_Open) %>% unlist() %>% unique()

data_Open_exp_noout <- data_Open_exp %>% slice(-Open_outliers)

model_Open <- train(Open ~ ., data = data_Open_exp_noout,method = 'leapBackward',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )







