#attempt to simulate a battle royale
#players are specified by aggression and skill
#skill determines ability to win fights
#aggression determines how often they get into fight


#skill is a value from -inf to inf, which is plugged into a logit equation
#along with opponent to determine result

#aggression is a value from 0.1 0.9 where it is used as a proportional 
#selection criteria for next encounters

#more aggressive players are probably better, so adding in relationship between the two


encounter_result<-function(x){
  r<-x[1]-x[2]
  
  if(runif(1)<1/(1+exp(-r))){
    return(c(1,2))
  }else{return(c(2,1))}
}


match_result<-function(players){

for(j in 1:99){
  
  #select two players
  selected_players<-sample(players$id[players$alive==1],2,
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


results<-c()
for(k in 1:2000){

players<-data.frame(id=1:100,skill=rnorm(100),aggression=runif(100,min=0.1,max=0.9),
                    alive=rep(1,100),kills=rep(0,100))
players$skill<-2*(players$aggression-0.5)+0.5*rnorm(100)

r<-match_result(players)
results<-rbind(results,r[r$alive==1,])
}














  




  














