value_to_coins <- function(value){
  
  gold <- as.integer(value/10000)
  
  value <- as.integer(c(value %% 10000))
  
  silver <- as.integer(value / 100)
  
  copper <- as.integer(value %% 100)
  
  return(list(gold = gold,
              silver = silver,
              copper = silver))
}

value_to_gold <- function(value){
  return(value / 10000)
}

format_coins <- function(coins){
  return(paste0(coins$gold,"g, ", coins$silver, "s, ", coins$copper, "c"))
}