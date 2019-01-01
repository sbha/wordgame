# Word Game
## Overview
Find all words four letters or more from a group of seven letters

## Usage
```r
# Display the 7 letters available:
shuffle()
[1] "p" "s" "n" "i" "t" "e" "d"
# The first letter displayed will always be the same and must be used at least once in each guess.

# Refresh the order to help get new ideas:
shuffle()
[1] "p" "t" "i" "d" "n" "e" "s"

# When you have a word idea, test it to see if it works:
wordgame('insipid')
[1] "yes! +4"
# Four letter words are worth 1 point, and each additional letter is worth another point. 

# Non-words are not accepted:
wordgame('sipid')
[1] "Sorry, 'sipid' does not work."

# Words less than four letters are not accepted:
wordgame('pie')
[1] "Words must contain at least 4 letters"

# Words that contain letters not in the 7 are not accepted:
wordgame('present')
[1] "Sorry, 'present' does not work."

# Words that do not contain the first letter are not accepted:
wordgame('tied')
[1] "Sorry, the word must contain the letter 'p'."

# Succcessful entries are stored and can't be used again:
wordgame('insipid')
[1] "Sorry, that word has already been used."

# Bonus points are given to words that use all seven letters:
wordgame('stipend')
[1] "All seven letters - bonus! +8"

# Successful entries are stored and points are tallied:
results$points()
[1] 235
results$word_count()
[1] 75
results$percent_words()
[1] 26
results$percent_points()
[1] 21.1
results$words()
independents
independent
dependents
dependent
dispensed
...
...
pits
sped
tips

# The default is to sort the results by character count, but they can be sorted alphabetically too:
thresults$words(sort = 'alpha')

# Throw in the towel and print the words that haven't been guessed:
give_up()

# Start a new game:
restart()
```

