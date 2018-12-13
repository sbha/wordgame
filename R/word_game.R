### Word Game ###

#library(tidyverse)

### Import data ###

words_url <- 'http://norvig.com/ngrams/TWL06.txt'

words <- read.table(words_url)
df_words <- data.frame(words[,1], stringsAsFactors = FALSE) %>% tbl_df()
names(df_words) <- 'word'
head(df_words)

### Functions ###
# sort letters in a string
ano_sort <- function(x){
  if(!is.character(x)) stop('x needs to be a character')
  y <- paste(sort(unlist(strsplit(x, ''))), collapse = '')
}

# get unique letters in a string
ano_unique <- function(x){
  if(!is.character(x)) stop('x needs to be a character')
  y <- paste(unique(unlist(strsplit(x, ''))), collapse = '')
}

# test if a string exist as a substring of a longer string
ano_subset <- function(ano, sub){
  sum(unlist(strsplit(sub, '')) %in% unlist(strsplit(ano, '')))
}

# shuffle the letters
ano_shuffler <- function(word, letter){
  word <- gsub(letter, '', word)
  word <- c(unlist(strsplit(word, '')))
  c(letter, sample(word,length(word),replace=FALSE))
}

# point counter
ano_pointr <- function(x){
  if (nchar(x) == 4) pts = 1
  else pts = nchar(x) - 3
  return(pts)
}

# score keeper
df_score <- data_frame(accepted_word = as.character(),
                       points = as.numeric(),
                       chars = as.numeric())

score_keeper <- function(word){
  if (!word %in% df_score$accepted_word) {
    df_score <<- bind_rows(df_score, data_frame(accepted_word = word,
                                                points = ano_pointr(word),
                                                chars = nchar(word))) %>%
      arrange(desc(chars), accepted_word)
  }
}

# score display
score_display <- function(){
  list(
    pts = function(){
      sum(df_score$points)
    },
    words = function(){
      cat(df_score$accepted_word, sep = '\n')
      #print(df_score$accepted_word, n =)
    }
  )
}



### Organize the data ###
# remove words that might be hyphanated or contain punctuation
# convert to lower case
# create new column of the sort the word
# get character counts

df_words <-
df_words %>%
  mutate_if(is.factor, as.character) %>%
  filter(!str_detect(word, '\\d+')) %>%
  filter(!str_detect(word, '[[:punct:]]')) %>%
  #filter(!str_detect(word, '[A-Z]')) %>% # commented out for the norvig data source, which is all caps
  mutate(word = tolower(word)) %>%
  mutate(chars_total = nchar(word)) %>%
  filter(chars_total > 3) %>%
  rowwise() %>%
  mutate(ano = ano_sort(word)) %>%
  mutate(ano_unique = ano_unique(ano)) %>%
  mutate(chars_unique = nchar(ano_unique)) %>%
  filter(chars_unique > 1) %>%
  mutate(vowels = str_remove_all(ano_unique, '[^[aeiouy]]')) %>%
  mutate(vowels_unique = ano_unique(vowels)) %>%
  #filter(nchar(vowels_unique) > 1) %>%
  ungroup()

head(df_words)

# find words that can be used for the base
df_words7 <-
df_words %>%
  filter(chars_total == 7 & chars_unique == 7) %>%
  filter(nchar(vowels_unique) == 2)


word7 <- df_words7$ano[sample(nrow(df_words7), 1)]

word_non_vowel <- str_remove_all(word7, '[[aeiouy]]')
word_non_vowel <- strsplit(word_non_vowel, '')[[1]]
base_letter <- word_non_vowel[sample(length(word_non_vowel), 1)]


# find words that can be derived from the base word
df_derivatives <-
df_words %>%
  rowwise() %>%
  mutate(sub_word = ano_subset(word7, ano_unique) == chars_unique) %>%
  filter(sub_word == TRUE) %>%
  filter(str_detect(ano_unique, base_letter))



### Game play ###

shuffle <- function(){
  #letter_base <- 'i'
  ano_shuffler(word7, base_letter)
}

wordgame <- function(x){
  #if (!is.character(x)) stop('You must enter a word')
  if (nchar(x) < 4){
    #stop('Words must contain at least 4 letters')
    print('Words must contain at least 4 letters')
  } else if (!str_detect(x, base_letter)) {
    print(paste0("Sorry, the word must contain the letter '", base_letter, "'."))
  } else if (x %in% df_score$accepted_word){
    print('Sorry, that word has alredy been used.')
  } else if (x %in% df_derivatives$word){
    pts <- ano_pointr(x)
    #word_list <<- create_word_list()
    #word_list$words(x)
    score_keeper(x)
    print(paste0('yes! +', pts))
  } else {
    print(paste0("Sorry, '", x, "' does not work."))
  }
}


#ano_shuffler(word7, base_letter)
wordgame('seekers')

shuffle()
results <- score_display()
results$pts()
results$words()
