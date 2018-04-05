library(ggplot2)
library(reshape2)

setwd('/home/robinson/Data/Tef/paper')

t<-read.csv('PlantHeight.csv', header = FALSE)
t<-t[-1,]
t<-t[-1,]

m<-melt(t, id.vars='V1')


m$Line<-'Quncho'

write.csv(m,'plantheight_Quncho.csv')


