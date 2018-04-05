library(ggplot2)
library(reshape2)

setwd('/home/robinson/Data/Tef/paper')

t<-read.csv('PlantHeight.csv')

t$length<-as.numeric(as.character(t$length))

h<-t[t$internode=='complete stem including P',]



ggplot(data=h,aes(x=Line, y=length))+geom_point() +
  #geom_errorbar(limits, width=0.1)+
  #scale_y_continuous(expand = c(0,0), limit = c(0,500)) +
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


mean <- aggregate(cbind(length) ~ Line, data=h, function (x) mean(x)) 
sd<- aggregate(cbind(length) ~ Line, data=h, function (x) sd(x)) 
mean$sd<-sd$length



limits <- aes(ymax = mean$length + mean$sd, ymin=mean$length - mean$sd)



ggplot(data=mean,aes(x=Line, y=length))+geom_bar(stat='identity') +
  geom_errorbar(limits, width=0.1)+
  #scale_y_continuous(expand = c(0,0), limit = c(0,500)) +
  ylab('Plant height (cm)')+
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



fit<-aov(length~ Line, data=h)
TukeyHSD(fit)


