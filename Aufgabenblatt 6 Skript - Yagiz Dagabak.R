##AUFGABENBLATT 6

install.packages("rio")
install.packages("tidyverse")
install.packages("forcats")
library(tidyverse)
library(forcats)
library(rio)


#AUFGABE 1

adf <- import("a18_s800_neu.rds")


length(na.omit(adf$partei))  

ggplot(data=subset(adf, !is.na(partei)), aes(x=fct_infreq(subset(partei,!is.na(partei)))))+labs(x="Partei (N = 641)",y="Prozent(%)")+
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="black",fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","cadetblue1","hotpink1"))


#AUFGABE 2.1

ggplot(na.omit(adf),aes(hheink)) + 
  geom_density(aes(y=550*..count..), colour="black",adjust=1) + 
  labs(x="Haushaltseinkommen",y=expression(bold("count")))+
  scale_x_continuous( expand = c(0, 0)) +
  geom_histogram(colour="black",fill=rgb(1,0,0,alpha=0.2),binwidth =400)+
  scale_y_continuous(expand = c(0, 0)) 


#AUFGABE 2.2

ggplot(na.omit(adf),aes(x= sex, y=eink,fill=sex))  + geom_violin() + geom_boxplot(width=0.1) +labs(x="Sex",y="Einkommen") 

#AUFGABE 3

ggplot(na.omit(adf),aes(eink, fill=partei)) + geom_histogram(binwidth=300,color= "black", boundary=40) + facet_grid(sex~partei)+labs(x="Einkommen",y="Count")

#AUFGABE 4
  
ggplot(na.omit(adf), aes(y = eink, x = alter, colour = sex)) + 
   geom_point() +
  geom_smooth(aes(linetype = sex), method = "loess", se = TRUE)  + 
   scale_x_continuous(limits=c(20,80),expand = c(0,0)) + 
   scale_y_continuous(limits=c(50,6000),expand = c(0,0)) + 
   theme(panel.background = element_rect(fill="white"), 
  panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray")) + 
  labs(x="Alter" ,y="Einkommen") +  theme(axis.line = element_line(colour = "black"))
