### Functions ###

# functions to create the data frame
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
ano_pointer <- function(x){
  if (nchar(x) == 4) pts = 1
  else pts = nchar(x) - 3
  return(pts)
}


# functions for gameplay
# options for displaying current game play results
score_display <- function(){
  list(
    points = function(){
      sum(df_derivatives$points[df_derivatives$guessed == TRUE])
    },
    words = function(sort = 'length'){
      if (sort == 'length') cat(df_derivatives$word[df_derivatives$guessed == TRUE], sep = '\n')
      if (sort == 'alpha') cat(sort(df_derivatives$word[df_derivatives$guessed == TRUE]), sep = '\n')
    },
    word_count = function(){
      length(df_derivatives$word[df_derivatives$guessed == TRUE])
    },
    percent_words = function(){
      round(length(df_derivatives$word[df_derivatives$guessed == TRUE])/length(df_derivatives$word) * 100, 1)
    },
    percent_points = function(){
      round(sum(df_derivatives$points[df_derivatives$guessed == TRUE])/sum(df_derivatives$points) * 100, 1)
    }
  )
}

# start new game
start_game <- function(){
  results <<- score_display()
  shuffle() 
}

# shuffle letters in the base word
shuffle <- function(){
  ano_shuffler(word7, base_letter)
}

# print unguessed words
# prompt
give_up <- function(){
  df_derivatives %>% 
    filter(!word %in% df_derivatives$word[df_derivatives$guessed == TRUE]) %>% 
    mutate(word = ifelse(bonus == TRUE, paste0(word, '*'), word)) %>% 
    select(word) %>% 
    print(n = Inf)
  df_derivatives$guessed <<- TRUE
}

# Game play functions
wordgame <- function(x){
  #if (!is.character(x)) stop('You must enter a word')
  x <- tolower(x)
  if (nchar(x) < 4){
    print('Words must contain at least 4 letters')
  } else if (!str_detect(x, base_letter)) {
    print(paste0("Sorry, the word must contain the letter '", base_letter, "'."))
  } else if (x %in% df_derivatives$word[df_derivatives$guessed == TRUE]){
    print('Sorry, that word has already been used.')
  } else if (x %in% df_derivatives$word){
    pts <- df_derivatives$points[df_derivatives$word == x]
    df_derivatives$guessed[df_derivatives$word == x] <<- TRUE
    if (isTRUE(df_derivatives$bonus[df_derivatives$word == x])){
      print(paste0('Pangram! All seven letters - 2x bonus points! +', pts)) # panogram
    } else {
      print(paste0('yes! +', pts))
    }
  } else {
    print(paste0("Sorry, '", x, "' does not work."))
  }
}

restart <- function(){
  # prompt are you sure
  word7 <<- df_words7$ano[sample(nrow(df_words7), 1)]
  
  word_non_vowel <<- str_remove_all(word7, '[[aeiouy]]')
  word_non_vowel <<- strsplit(word_non_vowel, '')[[1]]
  base_letter <<- word_non_vowel[sample(length(word_non_vowel), 1)]
  
  df_derivatives <<-
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

  start_game()
}


