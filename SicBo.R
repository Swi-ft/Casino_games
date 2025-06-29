#----------------------------------------------------------------------------------------------------------------------------
# Small Bet: Wins if sum is 4-10 and not triple
small_bet <- function(main){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  if((d1 == d2) && (d2 == d3)){
    return(-main)
  } else if((d1+d2+d3) %in% 4:10){
    return(main)
  } else {
    return(-main)
  }
}

mean(replicate(100000, small_bet(1)))

#----------------------------------------------------------------------------------------------------------------------------
# Big Bet: Wins if sum is 11-17 and not triple
big_bet <- function(main){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  if((d1 == d2) && (d2 == d3)){
    return(-main)
  } else if((d1+d2+d3) %in% 11:17){
    return(main)
  } else {
    return(-main)
  }
}

mean(replicate(100000, big_bet(1)))

#----------------------------------------------------------------------------------------------------------------------------
# Bet on Specific Total (e.g., 8, 9, etc.)
other_sums <- function(bet, amount){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  if(d1+d2+d3 == bet){
    if(bet %in% c(4,17)){
      return(amount*60)
    }
    if(bet %in% c(5,16)){
      return(amount * 30)
    }
    if(bet %in% c(6,15)){
      return(17*amount)
    }
    if(bet %in% c(7,14)){
      return(12*amount)
    }
    if(bet %in% c(8,13)){
      return(8 * amount)
    }
    if(bet %in% 9:12){
      return(6*amount)
    }
  } else {
    return(-amount)
  }
}

#----------------------------------------------------------------------------------------------------------------------------
# Bet on a specific number appearing on any dice
single_dice_bet <- function(number, amount){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  keep <- 0
  if(d1 == number){
    keep <- keep + 1
  }
  if(d2 == number){
    keep <- keep + 1
  }
  if(d3 == number){
    keep <- keep + 1
  }
  if(keep == 0){
    return(-amount)
  } else{
    return(keep * amount)
  }
}

#----------------------------------------------------------------------------------------------------------------------------
# Bet that a number appears twice
double_bet <- function(bet, amount){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  if(((d1 == d2) && (bet == d1)) || ((d2 == d3) && (bet == d2)) || ((d3 == d1) && (bet == d3))){
    return(amount * 10)
  } else {
    return(-amount)
  }
}

#----------------------------------------------------------------------------------------------------------------------------
# Bet that all 3 dice show the same number
triple_bet <- function(amount){
  d1 <- sample(1:6,1)
  d2 <- sample(1:6,1)
  d3 <- sample(1:6,1)
  if((d1 == d2) && (d2 == d3)){
    return(amount * 30)
  } else {
    return(-amount)
  }
}

#----------------------------------------------------------------------------------------------------------------------------
# Interactive Game

play_sic_bo <- function(){
  cat("Welcome to Sic Bo!\n")
  balance <- as.numeric(readline("Enter your starting balance: "))
  
  while(balance > 0){
    cat("\nYour current balance:", balance, "\n")
    cat("Bet Types:\n")
    cat("1 - Small Bet\n")
    cat("2 - Big Bet\n")
    cat("3 - Specific Sum (4–17)\n")
    cat("4 - Single Dice Number (1–6)\n")
    cat("5 - Double Bet on a Number (1–6)\n")
    cat("6 - Triple Bet (any number)\n")
    
    choice <- as.integer(readline("Choose a bet type (1–6): "))
    amount <- as.numeric(readline("Enter bet amount: "))
    
    if(amount > balance || amount <= 0){
      cat("Invalid amount. Try again.\n")
      next
    }
    
    result <- 0
    
    if(choice == 1){
      result <- small_bet(amount)
    } else if(choice == 2){
      result <- big_bet(amount)
    } else if(choice == 3){
      sum_choice <- as.integer(readline("Enter the total sum to bet on (4–17): "))
      if(!(sum_choice %in% 4:17)){
        cat("Invalid sum choice.\n")
        next
      }
      result <- other_sums(sum_choice, amount)
    } else if(choice == 4){
      number <- as.integer(readline("Enter a number to bet on (1–6): "))
      if(!(number %in% 1:6)){
        cat("Invalid number.\n")
        next
      }
      result <- single_dice_bet(number, amount)
    } else if(choice == 5){
      number <- as.integer(readline("Enter a number to bet on (1–6): "))
      if(!(number %in% 1:6)){
        cat("Invalid number.\n")
        next
      }
      result <- double_bet(number, amount)
    } else if(choice == 6){
      result <- triple_bet(amount)
    } else {
      cat("Invalid choice.\n")
      next
    }
    
    balance <- balance + result
    if(result > 0){
      cat("You won", result, "!\n")
    } else {
      cat("You lost", abs(result), "!\n")
    }
    
    if(balance <= 0){
      cat("You're out of money! Game over.\n")
      break
    }
    
    again <- tolower(readline("Play again? (yes/no): "))
    if(again != "yes"){
      break
    }
  }
  
  cat("\nFinal Balance:", balance, "\n")
}

# Uncomment the line below to start the game
# play_sic_bo()
