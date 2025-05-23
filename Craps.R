#----------------------------------------------------------------------------------------------------------------------------
pass_line <- function(main){
  prob <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
  roll <- sample(2:12, 1, prob = prob)
  win <- F
  loss <- F
  target <- 1
  if(roll %in% c(7,11)){
    win <- T
  } else if(roll %in% c(2,3,12)) {
    loss <- T
  } else {
    target <- roll
  }
  while(!(win || loss)){
    roll <- sample(2:12, 1, prob = prob)
    if(roll == target){
      win <- T
    } else if(roll == 7){
      loss <- T
    }
  }s
  if(win){
    return(main)
  } else {
    return(-main)
  }
}

#----------------------------------------------------------------------------------------------------------------------------

dont_pass_line <- function(main){
  prob <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
  roll <- sample(2:12, 1, prob = prob)
  win <- F
  loss <- F
  target <- 1
  if(roll %in% c(7,11)){
    loss <- T
  } else if(roll %in% c(2,3)) {
    win <- T
  } else if(roll == 12){
    return(0)
  } else {
    target <- roll
  }
  while(!(win || loss)){
    roll <- sample(2:12, 1, prob = prob)
    if(roll == target){
      loss <- T
    } else if(roll == 7){
      win <- T
    }
  }
  if(win){
    return(main)
  } else {
    return(-main)
  }
}

#----------------------------------------------------------------------------------------------------------------------------

pass_vs_dont <- function(){
  prob <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
  roll <- sample(2:12, 1, prob = prob)
  pass <- 0
  dont <- 0
  target <- 1
  if(roll %in% c(7,11)){
    pass <- 1
    dont <- -1
  } else if(roll %in% c(2,3,12)) {
    pass <- -1
    if(roll == 12){
      dont <- 0
    } else {
      dont <- 1
    }
  } else {
    target <- roll
  }
  while(pass == 0){
    roll <- sample(2:12, 1, prob = prob)
    if(roll == target){
      pass <- 1
      dont <- -1
    } else if(roll == 7){
      pass <- -1
      dont <- 1
    }
  }
  return(pass-dont)
}

#----------------------------------------------------------------------------------------------------------------------------

# Come Bets and Don't come bets are 1 move delayed and have no difference in their expected value compared to
# Place and Don't Place respectively

#----------------------------------------------------------------------------------------------------------------------------

pass_and_odds <- function(main, side){
  prob <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
  roll <- sample(2:12, 1, prob = prob)
  if(roll %in% c(7,11)){
    return(main)
  } else if (roll %in% c(2,3,12)){
    return(-main)
  } else {
    target <- roll
  }
  while(1){
    roll <- sample(2:12, 1, prob = prob)
    if(roll == target){
      if(target %in% c(4,10)){
        return(main + 2*side)
      } else if(target %in% c(5,9)){
        return(main + 1.5*side)
      } else {
        return(main + 1.2 * side)
      }
    } else if(roll == 7){
      return(-(main+side))
    }
  }
}

#-----------------------------------------------------------------------------------------------------------------------------------

mean(replicate(1000000, pass_line()))        #House Edge is 1.41%
mean(replicate(1000000, dont_pass_line()))   #House Edge is 1.36%
sum(replicate(1000, 25*(pass_vs_dont())))    #Finding the better strat
mean(replicate(1000000, pass_and_odds(1,100)/101)) #Expected Return per dollar is improved heavily after taking odds (still -ve)

#-----------------------------------------------------------------------------------------------------------------------------------


#Sample Game

play_craps <- function() {
  total_money <- 1000
  cat("Welcome to the Craps simulator!\nStarting money: $", total_money, "\n")
  
  repeat {
    cat("\nChoose your bet type:\n1 = Pass Line\n2 = Don't Pass Line\n3 = Pass with Odds\n")
    bet_type <- readline(prompt = "Enter 1, 2, or 3 (or 'q' to quit): ")
    if(tolower(bet_type) == 'q') {
      cat("Thanks for playing! Final money: $", total_money, "\n")
      break
    }
    
    bet_type <- as.numeric(bet_type)
    if(is.na(bet_type) || !(bet_type %in% 1:3)) {
      cat("Invalid choice. Please select 1, 2, or 3.\n")
      next
    }
    if(bet_type %in% 1:2) {
      main_bet <- as.numeric(readline(prompt = "Enter your main bet amount: "))
      if(is.na(main_bet) || main_bet <= 0) {
        cat("Invalid bet amount.\n")
        next
      }
      if(main_bet > total_money) {
        cat("You don't have enough money for that bet.\n")
        next
      }
    } else if(bet_type == 3) {
      main_bet <- as.numeric(readline(prompt = "Enter your main bet amount: "))
      side_bet <- as.numeric(readline(prompt = "Enter your side (odds) bet amount: "))
      if(any(is.na(c(main_bet, side_bet))) || main_bet <= 0 || side_bet < 0) {
        cat("Invalid bet amounts.\n")
        next
      }
      if(main_bet + side_bet > total_money) {
        cat("You don't have enough money for those bets.\n")
        next
      }
    }
    if(bet_type == 1) {
      result <- pass_line(main_bet)
    } else if(bet_type == 2) {
      result <- dont_pass_line(main_bet)
    } else if(bet_type == 3) {
      result <- pass_and_odds(main_bet, side_bet)
    }
    total_money <- total_money + result
    if(result > 0) {
      cat("You won $", result, "!\n")
    } else if(result < 0) {
      cat("You lost $", -result, "...\n")
    } else {
      cat("Push (no win/loss).\n")
    }
    
    cat("Your current total money: $", total_money, "\n")
    
    if(total_money <= 0) {
      cat("You've run out of money! Game over.\n")
      break
    }
    
    cont <- readline(prompt = "Do you want to play another round? (y/n): ")
    if(tolower(cont) != "y") {
      cat("Thanks for playing! Final money: $", total_money, "\n")
      break
    }
  }
}

play_craps()

#----------------------------------------------------------------------------------------------------------------------------

