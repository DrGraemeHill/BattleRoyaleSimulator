#these are the basic functions to run the battle royale sim
#match results and encounter results


encounter_result<-function(x){
  r<-x[1]-x[2]
  
  if(runif(1)<1/(1+exp(-r))){
    return(c(1,2))
  }else{return(c(2,1))}
}


match_result<-function(players){
  
  for(j in 1:99){
    
    #select two players
    selected_players<-sample(c(1:100)[players$alive==1],2,
                             prob=players$aggression[players$alive==1],
                             replace = F)
    
    #get the result of each encounter
    result<-encounter_result(players$skill[selected_players])
    
    winner<-selected_players[result[1]]
    loser<-selected_players[result[2]]
    
    players$kills[winner]<-players$kills[winner]+1
    players$alive[loser]<-0
    
    
  }
  
  return(players)
  
}