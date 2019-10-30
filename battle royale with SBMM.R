#this runs the data battle royale with a basic attempt at SBMM

library(dplyr)
library(ggplot2)

source("battle royale functions.R")


load("all_players_platform.RData")
#all_players<-all_players%>%select(-round_aggression,-round_skill)%>%
#  mutate(platform="PC")


#can't calc the quantiles initially as there are too many players with no wins
#will just order the players by matches_won
#this won't rank the equal placed but this will change eventually
all_players$ranking<-0
o<-order(all_players$matches_won,decreasing = T)
all_players$ranking[o[1:25000]]<-4
all_players$ranking[o[25001:50000]]<-3
all_players$ranking[o[50001:75000]]<-2
all_players$ranking[o[75001:100000]]<-1

#play 100 matches, rerank
result<-c()
for(k in 1:200){
for(rank in unique(all_players$ranking)){
 # print(rank)
  #do 250 random matches
  player_subset<-all_players%>%filter(ranking==rank)
  
  if(rank!=1){
  for(j in 1:250){
  #now just pick random players rather than cycle
  sel<-sample(1:length(player_subset[,1]),100,replace=F)
  current_players<-player_subset[sel,]
  current_players$alive<-1
  r<-match_result(current_players)
  r$deaths<-r$deaths+1-r$alive
  r$matches_won<-r$matches_won+r$alive
  r$matches_played<-r$matches_played+1
  #dump the results into the player data
    player_subset[sel,c("id","skill","aggression","matches_played",
                        "matches_won","kills","deaths")]<-r[,c("id","skill","aggression",
                                                               "matches_played","matches_won","kills","deaths")]
  }}
  
  #for the bottom quantile give them 50% bots
  
  
  if(rank==1){
    for(j in 1:500){
    #now just pick random players rather than cycle
    sel<-sample(1:length(player_subset[,1]),50,replace=F)
    current_players<-player_subset[sel,]
    #bots are just normal players with medium aggression and bad skill
    bots<-current_players%>%mutate(skill=-5,aggression=0.5,id=-1)
    current_players<-rbind(current_players,bots)
    current_players$alive<-1
    r<-match_result(current_players)
    r$deaths<-r$deaths+1-r$alive
    r$matches_won<-r$matches_won+r$alive
    r$matches_played<-r$matches_played+1
    #remove the bots
    r<-r[r$id!= -1,]
    #dump the results into the player data
    player_subset[sel,1:7]<-r[,1:7]
    }
  }
  
  
  #dump the subset back into the main section
all_players[all_players$ranking==rank,]<-player_subset

}



#hist(all_players$matches_won/all_players$matches_played,breaks=seq(0,0.4,by=0.01),xlim=c(0,0.2),
#     main=paste("iteration number",k))

r<-all_players%>%group_by(ranking)%>%
  summarise(win_percentage=100*mean(matches_won/matches_played))
#result<-rbind(result,c(signif( mean(all_players$matches_played),3),r$win_percentage))
players_with_no_wins<-length(which(all_players$matches_won==0))
#print(paste(c(signif( mean(all_players$matches_played),3),
#              players_with_no_wins,
#              signif(r$win_percentage,3))))
r<-all_players%>%group_by(platform)%>%
  summarise(wins=100*mean(matches_won/matches_played),
            kd=mean(kills/matches_played))%>%
  mutate(iterations=k)
print(r)
result<-rbind(result,r)
 #rerank
  o<-order(all_players$matches_won/all_players$matches_played,decreasing = T)
all_players$ranking[o[1:25000]]<-4
all_players$ranking[o[25001:50000]]<-3
all_players$ranking[o[50001:75000]]<-2
all_players$ranking[o[75001:100000]]<-1
  
}

