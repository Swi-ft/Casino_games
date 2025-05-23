#----------------------------------------------------------------------------------------------------------------------------

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
