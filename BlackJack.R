#-----------------------------------------------------------------------------------
# Create & shuffle a deck (no jokers)
get_shuffled_deck <- function(){
  deck <- rep(c(2:10, "J", "Q", "K", "A"), times = 4)
  return(sample(deck))
}

#-----------------------------------------------------------------------------------
# Calculate hand total
hand_total <- function(hand){
  values <- sapply(hand, function(card){
    if(card %in% c("J", "Q", "K")) return(10)
    if(card == "A") return(11)
    return(as.numeric(card))
  })
  total <- sum(values)
  # Adjust for Aces if total > 21
  aces <- sum(hand == "A")
  while(total > 21 && aces > 0){
    total <- total - 10
    aces <- aces - 1
  }
  return(total)
}

#-----------------------------------------------------------------------------------
# Print hand nicely
print_hand <- function(hand, owner){
  cat(owner, "hand:", paste(hand, collapse = ", "), "| Total:", hand_total(hand), "\n")
}

#-----------------------------------------------------------------------------------
#Blackjack Game
play_blackjack <- function(){
  cat("Welcome to Blackjack!\n")
  balance <- as.numeric(readline("Enter your starting balance: "))
  
  while(balance > 0){
    cat("\nYour balance:", balance, "\n")
    bet <- as.numeric(readline("Enter your bet: "))
    if(bet > balance || bet <= 0){
      cat("Invalid bet.\n")
      next
    }
    
    deck <- get_shuffled_deck()
    player_hand <- deck[1:2]
    dealer_hand <- deck[3:4]
    deck <- deck[-(1:4)]
    
    print_hand(player_hand, "Your")
    cat("Dealer shows:", dealer_hand[1], "+ [hidden]\n")
    
    # Player Turn
    while(TRUE){
      if(hand_total(player_hand) == 21){
        cat("Blackjack!\n")
        break
      }
      move <- tolower(readline("Do you want to hit or stand? "))
      if(move == "hit"){
        player_hand <- c(player_hand, deck[1])
        deck <- deck[-1]
        print_hand(player_hand, "Your")
        if(hand_total(player_hand) > 21){
          cat("You busted!\n")
          balance <- balance - bet
          break
        }
      } else if(move == "stand"){
        break
      } else {
        cat("Invalid move. Type 'hit' or 'stand'.\n")
      }
    }
    
    if(hand_total(player_hand) <= 21){
      # Dealer Turn
      print_hand(dealer_hand, "Dealer's")
      while(hand_total(dealer_hand) < 17){
        cat("Dealer hits.\n")
        dealer_hand <- c(dealer_hand, deck[1])
        deck <- deck[-1]
        print_hand(dealer_hand, "Dealer's")
      }
      
      player_total <- hand_total(player_hand)
      dealer_total <- hand_total(dealer_hand)
      
      if(dealer_total > 21){
        cat("Dealer busted! You win!\n")
        balance <- balance + bet
      } else if(player_total > dealer_total){
        cat("You win!\n")
        balance <- balance + bet
      } else if(player_total < dealer_total){
        cat("Dealer wins.\n")
        balance <- balance - bet
      } else {
        cat("It's a tie!\n")
      }
    }
    
    if(balance <= 0){
      cat("You're out of chips! Game over.\n")
      break
    }
    
    again <- tolower(readline("Do you want to play again? (yes/no): "))
    if(again != "yes") break
  }
  
  cat("\n You leave the table with", balance, "chips. Thanks for playing!\n")
}

play_blackjack()
