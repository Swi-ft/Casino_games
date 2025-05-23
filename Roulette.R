#----------------------------------------------------------------------------------------------------------------------------

returns_roulette_straight <- function(){
    bet <- sample(1:38,1)
    outcome <- sample(1:38,1)
    if(bet == outcome){
      return(35)
    }
    else{
      return(-1)
    }
}

#----------------------------------------------------------------------------------------------------------------------------

returns_roulette_red <- function(){
  bet <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  outcome <- sample(1:38,1)
  if(outcome %in% bet){
    return(1)
  }
  else{
    return(-1)
  }
}


#----------------------------------------------------------------------------------------------------------------------------

# Using Martingale Strategy on 'Red'

martingale_red <- function(bet_amount, no_of_runs){
  count <- 0
  track <- 1
  final_amount <- bet_amount
  while((final_amount > 0) && (track <= no_of_runs)){
    track <- track + 1
    curr_bet <- min(2^(count), final_amount)
    count <- count + 1
    curr_bet <- returns_roulette_red() * curr_bet
    if(curr_bet > 0){
      count <- 0
      final_amount <- final_amount + curr_bet
    } else {
      final_amount <- final_amount + curr_bet
    }
  }
  return((final_amount-bet_amount))
}

#----------------------------------------------------------------------------------------------------------------------------

# Using Reverse Martingale Strategy on 'Red'

rev_martingale_red <- function(bet_amount, no_of_runs){
  count <- 0
  track <- 1
  final_amount <- bet_amount
  while((final_amount > 0) && (track <= no_of_runs)){
    track <- track + 1
    curr_bet <- min(2^(count), final_amount)
    count <- count + 1
    curr_bet <- returns_roulette_red() * curr_bet
    if(curr_bet < 0){
      count <- 0
      final_amount <- final_amount + curr_bet
    } else {
      final_amount <- final_amount + curr_bet
    }
  }
  return((final_amount-bet_amount))
}

#----------------------------------------------------------------------------------------------------------------------------


mean(replicate(1000000, returns_roulette_straight()))
mean(replicate(1000000, returns_roulette_red()))

for(i in 1:30){
  print(mean(replicate(10000, martingale_red(2000, i))))
}

for(i in 0:30){
  print(mean(replicate(25000, rev_martingale_red(2000, i))) - mean(replicate(25000, martingale_red(2000, i))))
}

# We see that both have a similar outcome values (both -ve), but reverse Martingale has a safer output