
library(ggplot2)

setwd('/home/robinson/Data/Tef/paper')

t<-read.csv('lodgingAngle.csv')


mean <- aggregate(cbind(lodging.angle) ~ Line, data=t, function (x) mean(x)) 
sd<- aggregate(cbind(lodging.angle) ~ Line, data=t, function (x) sd(x)) 
mean$sd<-sd$lodging.angle



limits <- aes(ymax = mean$lodging.angle + mean$sd, ymin=mean$lodging.angle - mean$sd)



ggplot(data=mean,aes(x=Line, y=lodging.angle))+geom_bar(stat='identity') +
  geom_errorbar(limits, width=0.1)+
  scale_y_continuous(expand = c(0,0), limit = c(0,100)) +
  theme_bw()+
  theme(
    plot.background = element_blank()
    #,panel.grid.major = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color = 'black', size=1)
    ,axis.line.y = element_line(color = 'black', size=1)
    ,plot.title= element_text(lineheight=.8, face="bold")
    ,text = element_text(size=20)
    # ,legend.position=c(0.85,0.8)    
    #,legend.background = element_rect(size=.5, element_rect(), colour='black')
    ,legend.text.align = 1
    ,legend.background = element_blank()
    ,legend.text = element_text(colour="black", size = 20)
    ,axis.text.x = element_text(angle = 90, hjust = 1)
    ,legend.title=element_blank()
  )



fit<-aov(lodging.angle~ Line, data=t)


wilcox.test(x, y, alternative = "two.sided")  # tests significance 


t.test(x,y)
