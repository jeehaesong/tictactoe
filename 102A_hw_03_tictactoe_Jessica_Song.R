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
  whoFirst <- 0
  userNum <- readline(prompt='Press 1 for one player, 2 for two players: ')
  cat('line 33')
  if(userNum == "1"){
    whoFirst <- readline(prompt='Decide who goes first. Press 3 for user (you), press 4 for computer ')
  }
  # initialize game board
  board <- c("1","2","3","4","5","6","7","8","9")
  # while( no winner ){
  if(whoFirst == 4){
    while( is.na(check_winner(board))  ){
      # x's turn
      board <- alternativePlayer(board,userNum,'x',TRUE)
      if( !is.na(check_winner(board) ) ){
        break; # if x wins - quit loop
      }
      
      # o's turn
      board <- alternativePlayer(board,userNum,'o',FALSE)
      # if o wins - quit loop
      if( !is.na(check_winner(board) ) ){
        break; # if x wins - quit loop
      }
    }
  }else if(whoFirst == 3){
    while( is.na(check_winner(board))  ){
      # x's turn
      board <- alternativePlayer(board,userNum,'x',FALSE)
      if( !is.na(check_winner(board) ) ){
        break; # if x wins - quit loop
      }
      
      # o's turn
      board <- alternativePlayer(board,userNum,'o',TRUE)
      # if o wins - quit loop
      if( !is.na(check_winner(board) ) ){
        break; # if x wins - quit loop
      }
    }
  }else{
    while( is.na(check_winner(board))  ){
      # x's turn
      board <- alternativePlayer(board,userNum,'x',FALSE)
      if( !is.na(check_winner(board) ) ){
        break; # if x wins - quit loop
      }
      
      # o's turn
      board <- alternativePlayer(board,userNum,'o',FALSE)
      # if o wins - quit loop
      if( !is.na(check_winner(board) ) ){
        break; # if x wins - quit loop
      }
    }
  }
  # display final board state and who the winner is
  display(board)
  if(check_winner(board) != 'tie'){
    cat('Congrats! ', check_winner(board) ,' wins!')
  }else{
    cat('It was a ', check_winner(board) ,'!')
  }
}

display <- function (defalutVector= c(1:9)){
  output <- '\n'
  for(i in 1:9){
    if( i%%3 == 1 || i%%3 == 2){
      if(!is.na(defalutVector[i])& (defalutVector[i]=='x' | defalutVector[i] == 'o')){
        output <- cat(output,defalutVector[i],'|')
      }else{
        output <- cat(output,i,'|')
      }
    } else if (i == 9){
      if(!is.na(defalutVector[i])& (defalutVector[i]=='x' | defalutVector[i] == 'o')){
        output <- cat(output,defalutVector[i])
      }else{
        output <- cat(output,i)
      }
    }else {
      if(!is.na(defalutVector[i])& (defalutVector[i]=='x' | defalutVector[i] == 'o')){
        output <- cat(output,defalutVector[i],'\n---+---+---\n' )
      }else{
        output <- cat(output,i,'\n---+---+---\n' )
      }
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
    cat("You cannot play that location",pos,'\n')
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
    while(  available[randomIndex] == 'x' | available[randomIndex] == 'o'){
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
        break
      }
    }
  }
  
  xCount <- currentCount(state,'x')
  oCount <- currentCount(state,'o')
  if(xCount+oCount == 9){
    return ('tie')
  }
  if(is.na(winner)){
    cat('no winner yet!')
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
      }
      else if (!is.na(state[triples[[j]][k]]) & is.numeric(as.numeric(state[triples[[j]][k]]) )| is.na(state[triples[[j]][k]])){
        emptyIndex <- triples[[j]][k]
      }
      if(k ==3 & count ==2){
        if(!is.na(state[emptyIndex]) & (state[emptyIndex] == 'o'| state[emptyIndex] =='x')){
          
        }else{
          playIndex <-emptyIndex
          break;
        }
      }
    }
    if(!is.na(playIndex)){
      break;
    }
  }
  return (playIndex)
}

alternativePlayer <- function (state,userNum,currentSymbol,isCom){
  display(state) # display board 
  if(isCom){
    input <-computer_turn(state)
    cat('Computer played on',input)
  }else{
    messagge <- cat('Player "',currentSymbol,'": Select the coordinate you want to play ')
    input <- readline(prompt=messagge)
  }
  state <- update(state,currentSymbol,input)
  return(state)
}

cat("Enter 'play()' to play tic tec toe")