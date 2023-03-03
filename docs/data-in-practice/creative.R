library(maps)
library(geosphere)

#data = read.csv(file="survey.csv")

plot_creative_map = function(data){
  # No margin
  par(mar=c(0,0,0,0))
  
  # World Map
  map('world',
      main = "Around the world",
      col="#f2f2f2", 
      fill=TRUE, 
      bg="white", 
      lwd=0.05,
      mar=rep(0,4),
      border=0, 
      xlim = c(-120,120), 
      ylim = c(-80,80)
  )
  
  #Countries origin + gone
  points(x=c(data$origin_long,data$gone_long), y=c(data$origin_lat, data$gone_lat), col="slateblue", cex=2, pch=20)
  #Countries wish
  points(x=data$wish_long, y=data$wish_lat, col="pink", cex=2, pch=20)
  
  text(data$"Where do you come from?", x=data$origin_long, y=data$origin_lat,  col="slateblue", cex=1, pos=4)
  
  
  #compute connection
  for (i in 1:nrow(data)){
    origin = c(data[i, "origin_long"],data[i, "origin_lat"])
    arrival = c(data[i, "gone_long"],data[i, "gone_lat"])
    arrival_2 = c(data[i, "wish_long"],data[i, "wish_lat"])
    
    places_ive_been = gcIntermediate(origin, arrival, n=150, addStartEnd=TRUE, breakAtDateLine=F)
    
    places_to_go = gcIntermediate(origin, arrival_2, n=150, addStartEnd=TRUE, breakAtDateLine=F)
    
    # Show this connection
    lines(places_ive_been, col="slateblue", lwd=0.5)
    
    lines(places_to_go , col="red", lwd=0.5)
  } 
}