# You bet I will!

# inspired by:
# https://www.howtobet.com/calculator/hold-calculator/

# +130 means you get 130 for each 100 you bet. Underdog.
# -150 means you get 100 for each 150 you bet. Favorite.

# convert usa odds to eu odds
to_decimal_odds <- function(american_odds) {
  
  # When odss are positive.
  if(american_odds > 0) {
    return((american_odds + 100) / 100)

  # When odds are negative.
  } else {
    return((100 + -american_odds) / -american_odds)
  }
}


# example converting -300 to decimals odds
to_decimal_odds(-300)

# Calculate Break Even percentage for USA odds.
break_even_american_odds <- function(x) {
  
  # When odds are positive.
  if (x > 0) {
    return(100 / (100 +  x))
    
  # When odds are negative.
  } else if(x < 0) {
    return(-x / (100 + -x))
  }
}

# example +130
break_even_american_odds(130)

# Calculate Break Even Percentage for decimal odds.
break_even_decimal_odds <- function(decimal_odds){
  return(1 / decimal_odds)
}

# example 
break_even_decimal_odds(1.33)

# Convert American odds to probabilities.
american_odds_to_prob <- function(american_odds){

  # When odds are positive:
  if(american_odds > 0) {
    return(100 / (100 + american_odds))

  # When negative odds:
  } else {
    return(-american_odds / (-american_odds + 100))
  }
}

# Calculate the hold percentage.
hold_usa_percent <- function(...){
  
  probs <- sapply(c(...), american_odds_to_prob)
  total_hold <- sum(probs)
  
  return(total_hold - 1)
}

# example
hold_usa_percent(-125, 115)


# Calculate bookmakers probabilities for each betting options.
bookmaker_usa_probs <- function(...){
  
  probs <- sapply(c(...), american_odds_to_prob)
  total_hold <- sum(probs)
  
  result <- sapply(probs, function(prob) prob / total_hold)
  return(result)
}

# Example for -400 / +300.
# Can be generalized to > 2 beting options.
bookmaker_usa_probs(-400, 300)


