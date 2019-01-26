### Word Game ###

# library(tidyverse)
# source('R/functions.R')
# source('R/data.R') # tbl_df


### Game play ###
start_game()
#results <- score_display()
results$points()
results$word_count()
results$percent_words()
results$percent_points()
results$words()
results$words(sort = 'alpha')
shuffle()    

wordgame('robbers')
#give_up()
#restart()


# https://stackoverflow.com/questions/44702134/r-error-cannot-change-value-of-locked-binding-for-df
# https://stackoverflow.com/questions/6456501/how-to-include-source-r-script-in-other-scripts

