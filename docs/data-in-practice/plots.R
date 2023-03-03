library(dplyr)

#df = read.csv("origin_answers.csv", sep=";")

create_pie_chart = function(df){
  
  res = df %>% group_by(origin) %>% summarise(Freq=n())
  
  slices = list(res$Freq)[[1]]
  labels = list(res$origin)[[1]]
  
  chart = pie(slices, labels=labels, main="Origin of students")
  

  return (chart)
}

create_bar_chart = function(df){
  
  counts = table(df$background)
  
  mycols = c("red","blue")
  
  par(mar=c(5,15,4,1)+.1)
  barplot(counts
          , main="Students Education Background"
          , horiz=TRUE
          , names.arg=c(df$background)
          , las = 1
          , col = mycols[df$group]
          , legend = c("Technical", "Humanities")
        )
}