library(tidyverse)
library(tidytext)



# Transcript extraction ---------------------------------------------------
all_files <- unzip("youtube-personality.zip", list = T)$Name

filenames <- all_files[grepl("^youtube-personality/transcripts/VLOG", all_files)]

readTextFile <- function(nm) {
  unzip("youtube-personality.zip", nm);
  text <- readChar(nm, nchars = 1000000);
  unlink(nm)
  text
}

transcripts <- tibble()

for (i in 1:404) {
  id <- str_extract(filenames[i], "VLOG\\d+")
  trans <- tibble("Id" = id, "vlog" = readTextFile(filenames[i]))
  transcripts <- bind_rows(transcripts, trans)
}


# Other files -------------------------------------------------------------
gender <- unzip("youtube-personality.zip", "youtube-personality/YouTube-Personality-gender.csv") %>% read_delim(delim = " ")
audiovisual <- unzip("youtube-personality.zip", "youtube-personality/YouTube-Personality-audiovisual_features.csv") %>% read_delim(delim = " ")
train <- unzip("youtube-personality.zip", "youtube-personality/YouTube-Personality-Personality_impression_scores_train.csv") %>% read_delim(delim = " ")

train_set <- train %>% 
  inner_join(audiovisual) %>% 
  inner_join(gender)

test_set <- audiovisual %>% 
  anti_join(train) %>% 
  inner_join(gender)


