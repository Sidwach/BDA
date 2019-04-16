library(tidyverse)
library(tokenizers)
library(tidytext)


# Add pronouns to nrc lexicon ---------------------------------------------
nrc <- get_sentiments("nrc")
pronouns <- tibble(word = pos_df_pronouns$pronoun, sentiment = pos_df_pronouns$point_of_view)
XXXX <- tibble(word = "xxxx", sentiment = "XXXX")
new_nrc <- bind_rows(nrc, pronouns, XXXX)


# extract nrc features of all transcripts ---------------------------------
transcript_features_nrc <- 
  transcripts %>%
  unnest_tokens(token, vlog, token = 'words') %>%
  inner_join(new_nrc, by = c(token = 'word')) %>%
  count(Id, `sentiment`) %>%
  spread(sentiment, n, fill = 0)


# extract proportions of nrc features used and add total count ------------
proportions_nrc <- transcript_features_nrc %>% 
  mutate(sumVar = rowSums(.[,2:15])) %>% 
  mutate_at(vars(XXXX:trust), funs(./ sumVar))


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


# join new computed variables with train and test set ---------------------
train_joined <- train_set %>% 
  inner_join(proportions_nrc, by = c('vlogId' = 'Id')) %>% 
  inner_join(features_scored, by = c('vlogId' = 'Id'))

test_joined <- test_set %>% 
  inner_join(proportions_nrc, by = c('vlogId' = 'Id')) %>% 
  inner_join(features_scored, by = c('vlogId' = 'Id'))

# recode gender to factor
train_joined <- train_joined %>% 
  mutate(gender = as.factor(gender))

test_joined <- test_joined %>% 
  mutate(gender = as.factor(gender))

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
eigenvectors <- eigen(correlations)$vectors
plot(eigenvalues)

eigenvalues$vectors

# modeling

data_ext <- train_joined %>% dplyr::select(-c(vlogId, Agr:Open, gender))


###### PCA
pca <- prcomp(data_ext, scale = T)
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, ylab = "PC", xlab = "% Var")

# ggplot
pca.data <- tibble(vlogId = train$vlogId, 
                   X = pca$x[,1],
                   Y = pca$x[,2])

ggplot(data = pca.data, aes(X, Y, label = vlogId)) +
  geom_text() +
  xlab("PC1") +
  ylab("PC2") +
  theme_bw()

#
loading_scores <- pca$rotation[,2]
abs_scores <- abs(loading_scores)
abs_scores_ranked <- sort(abs_scores, decreasing = T)

pca$rotation

####
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'random')

model <- train(Extr ~(.), data = data_ext,method = 'lm',preProcess = c('center', 'scale'), trControl = control, tuneLength = 10 )






