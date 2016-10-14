#### Song, Jessica,  UID 904854175
#### 10/14/2016
#### Tic Tac Toe

triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

play <- function(){
  # determine game conditons: 1 or 2 players. If computer plays, is it player 1 or 2.
  userNum <- readline(prompt='Press 1 for one player, 2 for two players: ')
  # if(userNum ==1){
  #   whoFirst <- readline(promtp='Decide who goes first. Press 3 for computer, press 4 for user (you)')
  # }
  # initialize game board
  board <- c("1","2","3","4","5","6","7","8","9")
  # while( no winner ){
  while( is.na(check_winner(board))  ){
    # x's turn
    display(board) # display board 
    # x chooses where to play. prompt user or computer_turn()
    xInput <- readline(prompt='Enter the number for player X: ')
    board <- update(board,'x',xInput) # update board
    # print(board)
    if( !is.na(check_winner(board) ) ){
      break; # if x wins - quit loop
    }
    
    # o's turn
    display(board) # display board 
    # o chooses where to play. prompt user or computer_turn()
    if(userNum == 1){
      oInput <-computer_turn(board)
    }else{
      oInput <- readline(prompt='Enter the number for player O: ')
    }
    board <- update(board,'o',oInput) # update board
    check_winner(board) # if o wins - quit loop
    if( !is.na(check_winner(board) ) ){
      break; # if x wins - quit loop
    }
  }
  # display final board state and who the winner is
  display(board)
  cat('Congrats! ', check_winner(board) ,' win!')
}


display <- function (defalutVector= c(1:9)){
  output <- '\n'
  for(i in 1:9){
    if( i%%3 == 1 || i%%3 == 2){
      output <- cat(output,defalutVector[i],'|')
    } else if (i == 9){
      output <- cat(output,defalutVector[i])
    }else {
      output <- cat(output,defalutVector[i],'\n---+---+---\n' )
    }
  }
  cat(output,'\n')
}

update <- function (state, who, pos){
  pos <- as.numeric(pos)
  if( is.na(state[pos]) | is.numeric(state[pos]) ){
    state[pos] <- who
  } else if(  state[pos] != 'x' & state[pos] != 'o'){
    state[pos] <- who
  } else{
    print("You cannot play that location")
    xInput <- readline(prompt='Enter the different number: ')
    state <- update(state,who,xInput)
  }
  state
}
#AI
computer_turn <- function(state){
  playIndex <- NA
  xCount <- currentCount(state,'x')
  oCount <- currentCount(state,'o')
  available <- rep(0,9)
  
  #find available spot
  for(i in 1:length(state)){
    if(state[i] =='x' & !is.na(state[i]) & state[i]=='o'){
      
    }else{
      available[i] = state[i]
    }
  }
  if(min(xCount,oCount) == xCount){
    thisWho <- 'x'
    opponent <- 'o'
  }else{
    thisWho <- 'o'
    opponent <-'x'
  }
  
  playIndex <-findLastSpot(state,thisWho)
  if(!is.na(playIndex)){
    return (playIndex)
  }
  
  playIndex<-findLastSpot(state,opponent)
  if(!is.na(playIndex)){
    return (playIndex)
  }
  
  randomIndex <- sample(seq(1:9),1)
  if( is.na( available[randomIndex]  )){
    playIndex <- randomIndex
  }else{
    while(  available[randomIndex] == 0 ){
      randomIndex <- sample(seq(1:9),1)
    }
  }
  playIndex <- randomIndex
  playIndex
}

check_winner <- function(state) {
  winner <- NA
  for (i in 1: length(triples)){
    count <- 0
    temp <- c(NA,3)
    for ( j in 1:3){
      currentI <- triples[[i]][j]
      temp[j] <- state[currentI]
      if(!is.na(temp[j])){
        count = count+1
      }
    }
    
    if(count==3){
      if(temp[1]== temp[2] & temp[1]==temp[3]){
        winner <- temp[1]
        # cat(winner,'won!')
        break
      }
    }
  }
  
  xCount <- currentCount(state,'x')
  oCount <- currentCount(state,'o')
  if(xCount+oCount == 9){
    cat('it was a tie!')
  }
  winner
}

currentCount <- function (state,symbol){
  symbolCount <- 0
  for ( j in 1: length(state)){
    if(!is.na(state[j]) & state[j]==symbol){
      symbolCount = symbolCount +1
    }
  }
  symbolCount
}

findLastSpot <- function(state, symbol){
  playIndex <- NA
  for(j in 1:length(triples)){
    count <- 0
    emptyIndex <- NA
    for(k in 1:3){
      if(state[triples[[j]][k]] == symbol & !is.na(state[triples[[j]][k]]) ){
        count = count +1
      }else if (!is.na(state[triples[[j]][k]]) & is.numeric(as.numeric(state[triples[[j]][k]]) )| is.na(state[triples[[j]][k]])){
        emptyIndex <- triples[[j]][k]
      }
      if(k ==3 & count ==2){
        playIndex <-emptyIndex
        break;
      }
    }
    if(!is.na(playIndex)){
      break;
    }
  }
  return (playIndex)
}


cat("Enter 'play()' to play tic tec toe")