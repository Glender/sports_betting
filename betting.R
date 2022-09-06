# You bet I will!

# inspired by:
# https://www.howtobet.com/calculator/hold-calculator/

# +130 means you get 130 for each 100 you bet. Underdog.
# -150 means you get 100 for each 150 you bet. Favorite.

# convert usa odds to eu odds
american_to_decimal_odds <- function(american_odds) {
  
  # When odss are positive.
  if(american_odds > 0) {
    return((american_odds + 100) / 100)

  # When odds are negative.
  } else {
    return((100 + -american_odds) / -american_odds)
  }
}

# example converting -300 to decimals odds
american_to_decimal_odds(-300)

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
break_even_decimal_odds(2.3)

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




# Convert decimal odds to a probability.
decimal_odds_to_prop <- function(decimal_odds) {
  return(decimal_odds / (1 + decimal_odds))
}

# example
decimal_odds_to_prop(1/9)

# Calculate the Hold percentage based on the decimall odds.
hold_percent_decimal_odds <- function(...) {
  
  probs <- sapply(c(...), decimal_odds_to_prop)
  total_hold <- sum(probs)
  
  return(total_hold - 1)
}

# Calculate Hold Percentage for decimal odds.
hold_percent_decimal_odds(1.2, 3.4)


# Calculate bookmakers probabilities for decimal odds.
bookmaker_prob_eu <- function(...){
  
  probs <- sapply(c(...), decimal_odds_to_prop)
  total_hold <- sum(probs)
  
  result <- sapply(probs, function(prob) prob / total_hold)
  return(result)
}

# example
bookmaker_prob_eu(1.2, 2.3)

# Calculate bookmakers probabilities for each betting options.
bookmaker_probs_usa <- function(...){
  
  probs <- sapply(c(...), american_odds_to_prob)
  total_hold <- sum(probs)
  
  result <- sapply(probs, function(prob) prob / total_hold)
  return(result)
}

# Example for -400 / +300.
# Can be generalized to > 2 beting options.
bookmaker_probs_usa(-400, 300)



