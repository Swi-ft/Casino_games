#---------------------------------------------------------------
# Simulate Straight Bet Return
returns_roulette_straight <- function(){
  bet <- sample(1:38, 1)  # American roulette: 1-36, 0 (37), 00 (38)
  outcome <- sample(1:38, 1)
  if(bet == outcome){
    return(35)
  } else {
    return(-1)
  }
}

#---------------------------------------------------------------
# Simulate Red Bet Return
returns_roulette_red <- function(){
  red_numbers <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  outcome <- sample(1:38, 1)
  if(outcome %in% red_numbers){
    return(1)
  } else {
    return(-1)
  }
}

#---------------------------------------------------------------
# Martingale Strategy: Double after loss
martingale_red <- function(initial_amount, max_rounds){
  money <- initial_amount
  bet_base <- 1
  bet <- bet_base
  for(i in 1:max_rounds){
    if(money <= 0) break
    result <- returns_roulette_red()
    win <- result * bet
    money <- money + win
    if(result == -1){
      bet <- min(2 * bet, money)  # double, capped at current money
    } else {
      bet <- bet_base  # reset
    }
  }
  return(money - initial_amount)
}

#---------------------------------------------------------------
# Reverse Martingale: Double after win
rev_martingale_red <- function(initial_amount, max_rounds){
  money <- initial_amount
  bet_base <- 1
  bet <- bet_base
  for(i in 1:max_rounds){
    if(money <= 0) break
    result <- returns_roulette_red()
    win <- result * bet
    money <- money + win
    if(result == 1){
      bet <- min(2 * bet, money)
    } else {
      bet <- bet_base
    }
  }
  return(money - initial_amount)
}

#---------------------------------------------------------------
# Run Simulations
cat("Expected return (Straight): ", mean(replicate(1e6, returns_roulette_straight())), "\n")
cat("Expected return (Red): ", mean(replicate(1e6, returns_roulette_red())), "\n")

cat("\nMartingale Returns:\n")
for(i in 1:30){
  cat(sprintf("Rounds = %2d: %.3f\n", i, mean(replicate(10000, martingale_red(2000, i)))))
}

cat("\nReverse Martingale Relative Advantage:\n")
for(i in 1:30){
  m <- mean(replicate(10000, martingale_red(2000, i)))
  r <- mean(replicate(10000, rev_martingale_red(2000, i)))
  cat(sprintf("Rounds = %2d: RevMart - Mart = %.3f\n", i, r - m))
}

#---------------------------------------------------------------
#Interactive Roulette Game
play_roulette_game <- function(){
  money <- as.numeric(readline("Enter initial money (e.g., 1000): "))
  repeat {
    cat("\n--- Roulette ---\n")
    cat("You have:", money, "money\n")
    bet_amount <- as.numeric(readline("Enter your bet amount: "))
    if(bet_amount > money){
      cat("You can't bet more than you have!\n")
      next
    }

    bet_type <- tolower(readline("Bet on 'red' or 'straight'? "))
    if(bet_type == "red"){
      result <- returns_roulette_red()
      if(result == 1){
        cat("You won!\n")
        money <- money + bet_amount
      } else {
        cat("You lost!\n")
        money <- money - bet_amount
      }
    } else if(bet_type == "straight"){
      your_number <- as.numeric(readline("Pick a number between 1 and 38: "))
      if(!(your_number %in% 1:38)){
        cat("Invalid number!\n")
        next
      }
      outcome <- sample(1:38, 1)
      if(outcome == your_number){
        cat("Jackpot! You hit", outcome, "\n")
        money <- money + 35 * bet_amount
      } else {
        cat("Ball landed on", outcome, ". You lost!\n")
        money <- money - bet_amount
      }
    } else {
      cat("Invalid bet type.\n")
      next
    }

    if(money <= 0){
      cat("You're out of Money!\n")
      break
    }

    cont <- tolower(readline("Do you want to play again? (yes/no): "))
    if(cont != "yes") break
  }
  cat("Thanks for playing! You leave with", money, "money.\n")
}

# Uncomment below to play the interactive game
# play_roulette_game()
