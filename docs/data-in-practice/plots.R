library(dplyr)
library(wesanderson)

pallet = wes_palette("Royal2", 5, type = c("continuous"))

create_pie_chart = function(df){
  
  res = df %>% group_by(origin) %>% summarise(Freq=n())
  
  slices = list(res$Freq)[[1]]
  percentages = c(11.1, 11.1, 55.5, 11.11, 11.1)
  labels = list(res$origin)[[1]]

  pie(slices
  , labels=paste0(percentages, "%")
  , col = pallet
  , border = pallet
  , main="Origin of students")
  
  
  legend("topleft"
  , legend = labels
  , fill =  pallet
  , border = pallet
  , bg= "white")

  return ()
}


create_bar_chart = function(df){
  
  counts = table(df$background)
  
  mycols = wes_palette("Royal2", 2, type = c("discret"))
  
  par(mar=c(5,15,4,1)+.1)
  barplot(counts
          , main="Students Education Background"
          , horiz=TRUE
          , names.arg=c(df$background)
          , las = 1
          , col = mycols[df$group]
          , legend = c("Technical", "Humanities"))
}