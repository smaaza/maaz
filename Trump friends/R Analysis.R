library("igraph")
library("rtweet")

PUSA= lookup_users("realDonaldTrump")
names(PUSA)
PUSA$friends_count

#finding friends of Trump

P_friends = get_friends("realDonaldTrump") #46 friends

View(P_friends)

p_fr_data <- lookup_users(P_friends$user_id)

View(p_fr_data)

#top 20 friends of Trump

tmp <- (p_fr_data[,c('user_id',"screen_name",'friends_count',"followers_count")])

top = tmp[order(-tmp$friends_count),]
head(top)
top20_fr = top[1:20,]
View(top20_fr)

write.csv(top20_fr,file = "top20fr.csv")
trumpfr20 = read.csv("top20fr.csv")

#finding followers of Trump

PUSA$followers_count
P_followers = get_followers(
  "realDonaldTrump", n= 90000, retryonratelimit = TRUE
)
  ?get_followers

View(P_followers)
p_fol_data <- lookup_users(P_followers$user_id)
View(p_fol_data)

#top 20 random followers of Trump

tmp2 <- (p_fol_data[,c('user_id',"screen_name",'friends_count',"followers_count")])
top2 = tmp2[order(-tmp2$followers_count),]
head(top2)
top20_fol= top2[1:20,]
View(top20_fol)

write.csv(top20_fol,file="top20fol.csv")  
trumpfol20 = read.csv("top20fol.csv")



pfrIDs= P_friends$user_id
pfolIDs= P_followers$user_id

commonpeople = intersect(pfrIDs,pfolIDs)
View(commonpeople)


top20_frN=top20_fr$screen_name
top20_folN=top20_fol$screen_name



#graph1


all = as.data.frame(rbind(c("P_friends","P_followers", "black")))
names(all) = c('Type', 'Friend', 'Colour')
View(all)
relations1 = merge(data.frame(Type='P_friends',Friend=top20_fr$screen_name,
                              Colour ="blue"),
                             data.frame(Type= "P_followers", Friend=top20_fol$screen_name,
                                        Colour = "red"), all=T)

g1 = graph.data.frame(relations1, directed = FALSE)
V(g1)$label = V(g1)$name

par(mar = c(0,0,0,0))
V(g1)$label.cex = 0.5

plot(g1)
plot(g1, layout = layout.fruchterman.reingold, vertex.size = 20, edge.color = E(g1)$Colour)

#finding friends who are followinf each other


frnfol=lookup_friendships(top20_fr$screen_name,top20_fol$screen_name)
View(frnfol)

f=lookup_friendships("TrumpChicago","TrumpDoral")
View(f)
#performed for all friends of trump against other 19 friends, one at a time.

#plotting th friends graph
g2 = graph.formula("IvankaTrump"+-+"DonaldJTrumpJr","IvankaTrump"+-+"EricTrump","IvankaTrump"+-+"Jim_Jordan",
                   "IvankaTrump"+-+"JesseBWatters","IvankaTrump"+-+"KellyannePolls","IvankaTrump"+-+"foxandfriends"
                   ,"IvankaTrump"+-+"greta","IvankaTrump"+-+"ericbolling","IvankaTrump"+-+"MariaBartiromo",
                   "IvankaTrump"+-+"GOPChairwoman","IvankaTrump"+-+"Scavino45","IvankaTrump"+-+"KatrinaPierson",
                   "IvankaTrump"+-+"garyplayer","IvankaTrump"+-+"GeraldoRivera","IvankaTrump"+--"RealRomaDowney",
                   "IvankaTrump"+--"KatrinaCampins","IvankaTrump"+-+"TrumpLasVegas","IvankaTrump"+-+"TrumpChicago",
                   "IvankaTrump"+-+"TrumpDoral"
                   ,"DonaldJTrumpJr"+-+"EricTrump"
                   ,"DonaldJTrumpJr"+-+"KellyannePolls"
                   ,"DonaldJTrumpJr"+-+"Jim_Jordan"
                   ,"DonaldJTrumpJr"+-+"foxandfriends"
                   ,"DonaldJTrumpJr"+-+"JesseBWatters"
                   ,"DonaldJTrumpJr"+-+"greta"
                   ,"DonaldJTrumpJr"+-+"ericbolling"
                   ,"DonaldJTrumpJr"+-+"MariaBartiromo"
                   ,"DonaldJTrumpJr"+-+"GOPChairwoman"
                   ,"DonaldJTrumpJr"+-+"Scavino45"
                   ,"DonaldJTrumpJr"+-+"KatrinaPierson"
                   ,"DonaldJTrumpJr"+--"garyplayer"
                   ,"DonaldJTrumpJr"+-+"GeraldoRivera"
                   ,"DonaldJTrumpJr"+--"KatrinaCampins"
                   ,"DonaldJTrumpJr"+-+"TrumpLasVegas"
                   ,"DonaldJTrumpJr"+-+"TrumpChicago"
                   ,"DonaldJTrumpJr"+-+"TrumpDoral"
                   ,"EricTrump"+-+"KellyannePolls"
                   ,"EricTrump"+-+"	Jim_Jordan"
                   ,"EricTrump"+-+""
                   ,"EricTrump"+-+""
                   ,"EricTrump"+-+"foxandfriends"
                   ,"EricTrump"+-+"	JesseBWatters"
                   ,"EricTrump"+-+"greta"
                   ,"EricTrump"+-+"ericbolling"
                   ,"EricTrump"+-+"MariaBartiromo"
                   ,"EricTrump"+-+"GOPChairwoman"
                   ,"EricTrump"+-+"Scavino45"
                   ,"EricTrump"+-+"KatrinaPierson"
                   ,"EricTrump"+-+"garyplayer"
                   ,"EricTrump"+-+"GeraldoRivera"
                   ,"EricTrump"+-+"KatrinaCampins"
                   ,"EricTrump"+-+"TrumpLasVegas"
                   ,"EricTrump"+-+"TrumpChicago"
                   ,"EricTrump"+-+"TrumpDoral"
                   ,"KellyannePolls"+-+"Jim_Jordan"
                   ,"KellyannePolls"+-+"foxandfriends"
                   ,"KellyannePolls"+-+"JesseBWatters"
                   ,"KellyannePolls"+-+"greta"
                   ,"KellyannePolls"+-+"ericbolling"
                   ,"KellyannePolls"+-+"MariaBartiromo"
                   ,"KellyannePolls"+-+"GOPChairwoman"
                   ,"KellyannePolls"+-+"Scavino45"
                   ,"KellyannePolls"+-+"KatrinaPierson"
                   ,"KellyannePolls"+--"GeraldoRivera"
                   ,"KellyannePolls"+--"KatrinaCampins"
                   ,"Jim_Jordan"+-+"foxandfriends"
                   ,"Jim_Jordan"+-+"JesseBWatters"
                   ,"Jim_Jordan"+-+"greta"
                   ,"Jim_Jordan"+-+"ericbolling"
                   ,"Jim_Jordan"+-+"MariaBartiromo"
                   ,"Jim_Jordan"+-+"GOPChairwoman"
                   ,"Jim_Jordan"+--"Scavino45"
                   ,"Jim_Jordan"+--"KatrinaPierson"
                   ,"Jim_Jordan"+--"	KatrinaCampins"
                   ,"foxandfriends"+-+"JesseBWatters"
                   ,"foxandfriends"--+"greta"
                   ,"foxandfriends"+--"ericbolling"
                   ,"foxandfriends"+-+"MariaBartiromo"
                   ,"foxandfriends"+--"GOPChairwoman"
                   ,"foxandfriends"+--"Scavino45"
                   ,"foxandfriends"+-+"KatrinaPierson"
                   ,"foxandfriends"+-+"GeraldoRivera"
                   ,"foxandfriends"+--"RealRomaDowney"
                   ,"foxandfriends"+--"KatrinaCampins"
                   ,"JesseBWatters"--+"greta"
                   ,"JesseBWatters"+-+"ericbolling"
                   ,"JesseBWatters"+-+"MariaBartiromo"
                   ,"JesseBWatters"+--"GOPChairwoman"
                   ,"JesseBWatters"+-+"Scavino45"
                   ,"JesseBWatters"+-+"KatrinaPierson"
                   ,"JesseBWatters"+-+"GeraldoRivera"
                   ,"JesseBWatters"+--"KatrinaCampins"
                   ,"greta"+-+"MariaBartiromo"
                   ,"greta"+-+"GOPChairwoman"
                   ,"greta"+-+"Scavino45"
                   ,"greta"+--"KatrinaPierson"
                   ,"greta"+-+"GeraldoRivera"
                   ,"greta"+-+"RealRomaDowney"
                   ,"ericbolling"+-+"MariaBartiromo"
                   ,"ericbolling"+-+"GOPChairwoman"
                   ,"ericbolling"+-+"Scavino45"
                   ,"ericbolling"+-+"KatrinaPierson"
                   ,"ericbolling"+-+"GeraldoRivera"
                   ,"ericbolling"--+"RealRomaDowney"
                   ,"ericbolling"+-+"KatrinaCampins"
                   ,"MariaBartiromo"+-+"GOPChairwoman"
                   ,"MariaBartiromo"+-+"Scavino45"
                   ,"MariaBartiromo"+-+"KatrinaPierson"
                   ,"MariaBartiromo"--+"GeraldoRivera"
                   ,"MariaBartiromo"+--"	KatrinaCampins"
                   ,"GOPChairwoman"+-+"Scavino45"
                   ,"GOPChairwoman"+-+"KatrinaPierson"
                   ,"GOPChairwoman"+--"KatrinaCampins"
                   ,"Scavino45"+-+"GeraldoRivera"
                   ,"Scavino45"+--"KatrinaCampins"
                   ,"KatrinaPierson"+-+"	KatrinaCampins"
                   ,"GeraldoRivera"--+"RealRomaDowney"
                   ,"TrumpLasVegas"+-+"TrumpChicago"
                   ,"TrumpLasVegas"+-+"	TrumpDoral"
                   ,"TrumpChicago"+-+"	TrumpDoral")


plot(g2,layout = layout.fruchterman.reingold, vertex.size = 20, edge.color = E(g2)$Colour,edge.arrow.size=0.4)


#graph density and diameter

graph.density(g1)

diameter(g1)
?diameter

## Computing the neighborhood overlap of each edge

## get the neighborhood graph of all nodes.
gn = neighborhood(g1, order = 1)

## get pair of nodes that are at the end of each edge.
g.ends = ends(g1, E(g1))

# number of edges

N = nrow(g.ends)
# make space for neighborhood overlap score

NO = rep(0, N)

for (a in 1:N) {
  ## for every edge
  
  x = g.ends[a,1] # x is the node at one end of the edge
  y = g.ends[a,2] # y is the node at the other end of the edge
  
  ## compute the intersection of the neighbourhoods of x and y
  i = length(intersect(gn[[x]], gn[[y]])) - 2
  ## compute the union of the neighbourhoods of x and y
  u = length(union(gn[[x]], gn[[y]])) - 2
  
  ## Note that we subtract 2 since each neighbourhood includes x and y.
  ## we don't want to include x and y in the counts.
  
  ## the neighbourhood overlap is the intersection/union
  NO[a] = i/u
}
NO[a]

#Homophily

par(mar = c(0, 0, 0, 0))
V(g1)$label.cex = 0.5
V(g1)$label = c("S", "S", "S", "S", "S", "S", "S", 
                "NS", "S", "NS", "S", "NS", "S", "S", "S", "S", "S",
                "S", "S", "S", "S", "S", "NS", "S", 
                "NS", "S", "NS", "NS", "NS", "S", "S", "S", "S", "S",
                "NS", "NS", "S", "NS", "S", "NS", "S", "S")
plot(g1, layout = layout.fruchterman.reingold, vertex.size = 20)

V(g1)


class = c("S", "S", "S", "S", "S", "S", "S", 
          "NS", "S", "NS", "S", "NS", "S", "S", "S", "S", "S",
          "S", "S", "S", "S", "S", "NS", "S", 
          "NS", "S", "NS", "NS", "NS", "S", "S", "S", "S", "S",
          "NS", "NS", "S", "NS", "S", "NS", "S", "S")

## extract the adjacency matrix
A = get.adjacency(g1)

## repeat 1000 times
R = 1000


###----- Method 1: Using a for loop
crossEdgeCount = rep(0,R)

for (a in 1:R) {
  ### permute the row and column labels
  permutedClass = sample(class)
  ### record the number of cross S-NS edges
  xPos = which(permutedClass == "S")
  yPos = which(permutedClass == "NS")
  crossEdgeCount[a] = sum(A[xPos, yPos])
}


## examine the cross edge distribution
par(mar = c(2, 2, 3, 2))
hist(crossEdgeCount,40)

## compare the number of cross S-NS edges from the data to the 
## random distribution
xPos = which(class == "S")
yPos = which(class == "NS")
dataCrossEdgeCount = sum(A[xPos, yPos])

## If the they look the same we cannot reject H0
## If they look different, we can reject, meaning there is homophily
pVal = mean(crossEdgeCount < dataCrossEdgeCount)
pVal
## p value is very small, so reject H0: there is no homophily,
## therefore there must be homophily


