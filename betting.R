# You bet I will!

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
  
  if (x > 0) {
    return(100 / (100 +  x))
  } else if(x < 0) {
    return(-x / (100 + -x))
  }
}

# example +130
break_even_american_odds(130)


# Convert American odds to probabilities.
american_odds_to_prob <- function(american_odds){

  if(american_odds > 0) {
    return(100 / (100 + american_odds))
  } else {
    return(-american_odds / (-american_odds + 100))
  }
}

hold_percent <- function(usa_odds1, usa_odds2){
  probs <- sapply(c(usa_odds1, usa_odds2), american_odds_to_prob)
  return(sum(probs) - 1)
}

# example +300 / -400
hold_percent(-400, 300)

# 
bookmaker_probs <- function(usa_odds1, usa_odds2){
  
  probs <- sapply(c(usa_odds1, usa_odds2), american_odds_to_prob)
  total_hold <- sum(probs)
  
  p1 <- probs[1] / total_hold
  p2 <- probs[2] / total_hold
  
  return(c(p1, p2))
}

#  example -400 / +300
bookmaker_probs(-400, 300)














