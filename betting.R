# You bet I will!

# inspired by:
# https://www.howtobet.com/calculator/hold-calculator/

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
break_even_american_odds <- function(usa_odds) {
  
  # When odds are positive.
  if (usa_odds > 0) {
    return(100 / (100 +  usa_odds))
    
  # When odds are negative.
  } else if(usa_odds < 0) {
    return(-usa_odds / (100 + -usa_odds))
  }
}

# example +130
break_even_american_odds(130)


# Calculate Break Even Percentage for decimal odds.
break_even_decimal_odds <- function(...){
  return(1 / c(...))
}

# example: decimall to break even percentage: 
break_even_decimal_odds(2.3, 1.3, 2, 1.18)

# But this also works the other way around:
break_even_decimal_odds(.4347, .7692, .5, .84745)

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
hold_percent_usa <- function(...){
  
  probs <- sapply(c(...), american_odds_to_prob)
  total_hold <- sum(probs)
  
  return(total_hold - 1)
}

# example
hold_percent_usa(150, -175)
hold_percent_usa(142, -140)

# Convert decimal odds to a probability.
decimal_odds_to_prop <- function(decimal_odds) {
  return(1 / decimal_odds)
}

# Example, 2 yields 0.5; 1.5 yields .667   
decimal_odds_to_prop(1.5)

# Calculate the Hold percentage based on the decimal odds.
hold_percent_decimal_odds <- function(...) {
  
  probs <- sapply(c(...), decimal_odds_to_prop)
  total_hold <- sum(probs)
  
  return(total_hold - 1)
}

# Calculate Hold Percentage for decimal odds.
hold_percent_decimal_odds(1.71, 2.55, 1.86)


# Calculate bookmakers probabilities for decimal odds.
bookmaker_prob_eu <- function(...){
  
  probs <- sapply(c(...), decimal_odds_to_prop)
  total_hold <- sum(probs)
  
  result <- sapply(probs, function(prob) prob / total_hold)
  return(result)
}

# example
bookmaker_prob_eu(1.08, 13, 35)


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


give_names <- function(var) {
  names(var) <- c("profit", "roi")
  return(var)
}


# +130 means you get 130 for each 100 you bet. Underdog.
# -150 means you get 100 for each 150 you bet. Favorite.
profit_calc_usa <- function(usa_odds, amnt){
  
  # When positive odds (e.g. +130).
  if(usa_odds > 0){
    
    profit <- (usa_odds / 100) * amnt
    return(
      give_names(list(profit, profit / (profit + amnt)))
    )
    
  # When negative odds (e.g. -130).
  } else if(usa_odds < 0){
    
    profit <- (amnt / -usa_odds) * 100
    return(
      give_names(list(profit, profit / (profit + amnt)))
    )
  }
}

# Some examples.
profit_calc_usa(200, 100) # +300 / -100
profit_calc_usa(-175, 100) # +157.14 / -100

# Calculate your profit for decimal odds.
profit_calc_eu <- function(decimal_odds, amnt){
  
  profit <- (decimal_odds*amnt) - amnt
  results <- c(profit, (profit/amnt))
  
  return(give_names(results))
}

# examples
profit_calc_eu(1.55, 10)

# Main function.
profit_calc <- function(odds, amnt, method = c("decimal", "american"))
{
  
  method <- match.arg(method)
  profit <- switch(method,
    "decimal" = profit_calc_eu(odds, amnt),
    "american" = profit_calc_usa(odds, amnt)
  )
  return(profit)
}

# Examples
profit_calc(1.55, 20, method = "decim")
profit_calc(-110, 20, method = "amer")
