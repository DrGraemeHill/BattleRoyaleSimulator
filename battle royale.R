#attempt to simulate a battle royale
#players are specified by aggression and skill
#skill determines ability to win fights
#aggression determines how often they get into fight


#skill is a value from -inf to inf, which is plugged into a logit equation
#along with opponent to determine result

#aggression is a value from 0.5 0.9 where it is used as a proportional 
#selection criteria for next encounters

#more aggressive players are probably better, so adding in relationship between the two




#bots<-players<-data.frame(id=1:50,skill=rep(-3,50),aggression=rep(0.5,50),
 #                         alive=rep(1,100),kills=rep(0,100))
source("battle royale functions.R")

results<-c()

#create player pool
player_number<-100000
all_players<-data.frame(id=1:player_number,skill=rnorm(player_number),
                    aggression=runif(player_number,min=0.5,max=1),
                    matches_played=rep(0,player_number),
                    matches_won=rep(0,player_number),
                    kills=rep(0,player_number),
                    deaths=rep(0,player_number))
all_players$skill<-2*(all_players$aggression-0.5)+0.5*rnorm(player_number)

s<-sample(1:length(all_players[,1]),length(all_players[,1])/2)
all_players$platform<-"PC"
all_players$platform[s]<-"Console"


all_players$skill[all_players$platform=="PC"]<-all_players$skill[all_players$platform=="PC"]+0.5

#run through all players a few times so there are statistics

#everyone plays 100 matches to initialise the numbers
for(l in 1:100){
  print(l)
for(k in 1:1000){
  #if(k%%100==0){print(k)}
#select the players
  sel<-(1+(k-1)*100):(100+(k-1)*100)

  current_players<-all_players[sel,]
  current_players$alive<-1
  


r<-match_result(current_players)
r$deaths<-r$deaths+1-r$alive
r$matches_won<-r$matches_won+r$alive
r$matches_played<-r$matches_played+1

#dump the results into the player data
s<-which(all_players$id==r$id)


all_players[s,]<-r[,1:8]
}}
save(all_players,file="all_players_platform.RData")






