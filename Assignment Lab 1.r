source("http://www.openintro.org/stat/data/arbuthnot.R")
#What command would you use to extract just the counts of girls baptized? Try it!
arbuthnot$year
arbuthnot$girls
arbuthnot$boys

#Display the baptism records of 
#a) girls,
summary(arbuthnot$girls)
arbuthnot[arbuthnot$girls == max(arbuthnot$girls) , ]
arbuthnot[arbuthnot$girls == min(arbuthnot$girls) , ]
#b) boys,
summary(arbuthnot$boys)
arbuthnot[arbuthnot$boys == max(arbuthnot$boys) , ]
arbuthnot[arbuthnot$boys == min(arbuthnot$boys) , ]
#c) all children, 
arbuthnot$children<-arbuthnot$boys + arbuthnot$girls
summary(arbuthnot$children)
arbuthnot[arbuthnot$children == max(arbuthnot$children) , ]
arbuthnot[arbuthnot$children == min(arbuthnot$children) , ]
#d) the ratio of boys and girls and 
arbuthnot$ratio<-arbuthnot$boys / arbuthnot$girls
summary(arbuthnot$ratio)
arbuthnot[arbuthnot$ratio == max(arbuthnot$ratio) , ]
arbuthnot[arbuthnot$ratio == min(arbuthnot$ratio) , ]
#e) the proportion of boys of all children over the observation years.
arbuthnot$proportion<-arbuthnot$boys / arbuthnot$children
summary(arbuthnot$proportion)
arbuthnot[arbuthnot$proportion == max(arbuthnot$proportion) , ]
arbuthnot[arbuthnot$proportion == min(arbuthnot$proportion) , ]

source("http://www.openintro.org/stat/data/present.R")
present$year
present$girls
present$boys
#Display the records of 
#a) girls,
summary(present$girls)
present[present$girls == max(present$girls) , ]
present[present$girls == min(present$girls) , ]
#b) boys,
summary(present$boys)
present[present$boys == max(present$boys) , ]
present[present$boys == min(present$boys) , ]
#c) all children, 
present$children<-present$boys + present$girls
summary(present$children)
present[present$children == max(present$children) , ]
present[present$children == min(present$children) , ]
#d) the ratio of boys and girls and 
present$ratio<-present$boys / present$girls
summary(present$ratio)
present[present$ratio == max(present$ratio) , ]
present[present$ratio == min(present$ratio) , ]
#e) the proportion of boys of all children over the observation years.
present$proportion<-present$boys / present$children
summary(present$proportion)
present[present$proportion == max(present$proportion) , ]
present[present$proportion == min(present$proportion) , ]
#f) highest number of births in US.
present[present$children == max(present$children) , ]

