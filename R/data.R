### Data ###


# Import data 
words_url <- 'http://norvig.com/ngrams/TWL06.txt'
words <- read.table(words_url)
df_words <- data.frame(words[,1], stringsAsFactors = FALSE) %>% tbl_df()
names(df_words) <- 'word'
head(df_words)

#df_words %>% write.csv('df_words.csv', row.names = FALSE)
#df_words <- readr::read_csv('/Users/stuartharty/Documents/data/df_words.csv')


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
  mutate(vowels = str_remove_all(ano, '[^[aeiouy]]')) %>%
  mutate(vowels_unique = ano_unique(vowels)) %>%
  #filter(nchar(vowels_unique) > 1) %>%
  ungroup()

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
  filter(str_detect(ano_unique, base_letter)) %>% 
  mutate(points = ano_pointer(word)) %>% 
  #mutate(bonus = chars_total == 7 & chars_unique == 7)
  mutate(bonus = chars_unique >= 7) %>% 
  mutate(points = if_else(isTRUE(bonus), points * 2, points)) %>% 
  mutate(guessed = FALSE) %>% 
  ungroup() 