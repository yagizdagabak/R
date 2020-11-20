##AUFGABENBLATT 5

install.packages("rio")
install.packages("tidyverse")
install.packages("forcats")
install.packages("psych")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("fitdistrplus")
install.packages("lubridate")
library(lubridate)
library(fitdistrplus)
library(pastecs)
library(Hmisc)
library(psych)
library(forcats)
library(tidyverse)
library(rio)

##AUFGABE 1

#1.1

adf <- import("a18_s800_neu.rds")
str(adf)
str(adf$famst)
##factor variable --> bar plot
ggplot(data=subset(adf, !is.na(famst)), aes(fct_infreq(subset(famst,!is.na(famst)))))+labs(x="Familienstatus",y="count")+
geom_bar(colour="black",fill=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+coord_flip()

#1.2

str(adf$alter)
is.na(adf$alter)
ggplot(na.omit(adf),aes(alter)) + geom_histogram(binwidth=1,colour="black")
##alternative
ggplot(na.omit(adf),aes(alter.K)) + geom_bar(colour="black")

#1.3

MODE <- function(x){
  a <- table(x)
  return(a[which.max(a)]) 
  
}

##alter##numeric variable
mean(na.omit(adf$alter))
median(na.omit(adf$alter))
MODE(na.omit(adf$alter))
quantile(na.omit(adf$alter))
geometric.mean(na.omit(adf$alter))
harmonic.mean(na.omit(adf$alter))
range(na.omit(adf$alter))

##lzuf#ordinal variable
mean(as.numeric(na.omit(adf$lzuf))-1) ##as.numeric adds 1 to each value, solution --> -1
median(as.integer(adf$lzuf)-1,na.rm=TRUE)
MODE(na.omit(adf$lzuf))
quantile(as.integer(adf$lzuf)-1,na.rm=TRUE)
range(na.omit(adf$lzuf))

##konf##categorical variable
MODE(adf$konf)
describe(adf$konf)

#1.4
mean(adf$hheink,na.rm=TRUE)
median(adf$hheink,na.rm=TRUE)
ggplot(na.omit(adf),aes(hheink)) +  geom_density(bw=1000,color="yellow",fill=rgb(1,1,0,alpha=0.3)) +
  labs(x="Haushaltseinkommen",y=expression(bold("density")))+
  scale_x_continuous(limits = c(1,10000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,0.000219), expand = c(0, 0)) +
  geom_vline(aes(xintercept = mean(adf$hheink,na.rm=TRUE), color="mean"),
             linetype = "dashed", size = 0.6) +
  geom_vline(aes(xintercept = median(adf$hheink,na.rm=TRUE),color="median"),
             linetype = "dashed", size = 0.6) +
scale_color_manual(name = "mean&median",values = c(median = "blue", mean = "red"))

#1.5

mean(adf$eink,na.rm=TRUE)
median(adf$eink,na.rm=TRUE)
sd(adf$eink,na.rm=TRUE)
min(adf$eink,na.rm=TRUE)
max(adf$eink,na.rm=TRUE)
range(adf$eink,na.rm=TRUE)
IQR(adf$eink,na.rm=TRUE)


ggplot(na.omit(adf),aes(eink)) + 
  geom_density(aes(y=400*..count..), colour="black") + 
  labs(x="Einkommen",y=expression(bold("count")))+
  scale_x_continuous( expand = c(0, 0)) +
  geom_histogram(colour="black",fill=rgb(0,0.5,1,alpha=0.2),binwidth =400)+
  scale_y_continuous(expand = c(0, 0)) 

#1.6

#a
mean(subset(adf,adf$sex=="Mann"&!is.na(eink))$eink)
mean(subset(adf,adf$sex=="Frau"&!is.na(eink))$eink)
barplot(c(mean(subset(adf,adf$sex=="Mann"&!is.na(eink))$eink),mean(subset(adf,adf$sex=="Frau"&!is.na(eink))$eink)),names.arg=c("Mann","Frau"),main="Einkommensdifferenz bei Geschlecht",ylim=range(pretty(c(0,2500))),col=c("red","blue"))
#b
mean(subset(adf,adf$ew=="West"&!is.na(eink))$eink)
mean(subset(adf,adf$ew=="Ost"&!is.na(eink))$eink)
barplot(c(mean(subset(adf,adf$ew=="West"&!is.na(eink))$eink),mean(subset(adf,adf$ew=="Ost"&!is.na(eink))$eink)),names.arg=c("West","Ost"),main="Einkommensdifferenz bei Land",ylim=range(pretty(c(0,2000))),col=c("green","yellow"))
#c
ausbeink <- c()
for ( i in 1:length(levels(adf$ausb))){
ausbeink[i] <- mean(subset(adf,adf$ausb==levels(adf$ausb)[i]&!is.na(eink))$eink)}

df <- data.frame(Einkommen=ausbeink,Ausbildungsniveau=levels(adf$ausb))

ggplot(data=df, aes(x=reorder(Ausbildungsniveau, -Einkommen),y=Einkommen))+
  geom_bar(stat="identity",colour="black",fill=c("#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00"))+coord_flip()+
  labs(x="Ausbildungsniveau",y="Einkommen")

#1.7

##alter is a numeric, cardinal, continuous variable, so all these are appropriate
summary(adf$alter)
psych::describe(adf$alter)
Hmisc::describe(adf$alter)
stat.desc(adf$alter)

##lzuf, is an ordinal variable harmonic and geometric mean are not appropriate, also min/max somewhat meaningless 
summary(adf$lzuf)
psych::describe(as.numeric(adf$lzuf)-1)
Hmisc::describe(as.numeric(adf$lzuf)-1)
stat.desc(as.numeric(adf$lzuf)-1)

##konf is a categorical variable so only mode and relative frequencies are appropriate

Hmisc::describe(adf$konf)


#1.8

##boxplot meaningful for factored continous variables

boxplot(adf$eink,horizontal =TRUE)
abline(v = min(adf$eink,na.rm=TRUE), col = "Blue")
abline(v = max(adf$eink,na.rm=TRUE), col = "Yellow")
abline(v = median(adf$eink,na.rm=TRUE), col = "Green")
abline(v = quantile(adf$eink, c(0.25, 0.75),na.rm=TRUE), col = "Red")

#1.9

##alle
length(na.omit(adf$alter[which(adf$alter>60)]))/length(na.omit(adf$alter))
##maenner
length(subset(adf$alter,adf$alter>60&!is.na(adf$alter)&adf$sex=="Mann"))/length(subset(adf$alter,!is.na(adf$alter)&adf$sex=="Mann"))
##frauen
length(subset(adf$alter,adf$alter>60&!is.na(adf$alter)&adf$sex=="Frau"))/length(subset(adf$alter,!is.na(adf$alter)&adf$sex=="Frau"))

#1.10

quantile(adf$alter,p=seq(0,1,0.1),na.rm=TRUE)[2]
quantile(adf$alter,p=seq(0,1,0.1),na.rm=TRUE)[10]



#1.11

quantile(subset(adf$alter,adf$sex=="Mann"),p=seq(0,1,0.1),na.rm=TRUE)[2]
quantile(subset(adf$alter,adf$sex=="Mann"),p=seq(0,1,0.1),na.rm=TRUE)[10]


#1.12
summary(adf$eink)
psych::describe(adf$eink)
Hmisc::describe(adf$eink)
stat.desc(adf$eink)


ggplot(na.omit(adf),aes(eink)) +  geom_density(color="red",fill=rgb(1,0,0,alpha=0.3),bw=50,adjust=5) +
  labs(x="Einkommen",y=expression(bold("density")))+
  scale_x_continuous(limits = c(1,10000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,0.0005), expand = c(0, 0)) 

##right-tailed beta distribution (α < β) or perhaps a gamma   

##code to check which one better fits

normalized <-  (na.omit(adf$alter)-min(na.omit(adf$alter))+0.0001)/(max(na.omit(adf$alter))-min(na.omit(adf$alter))+0.001)##dirty trick, fitdist() gives error when data contains 0's or 1's
normalized
summary(normalized)

fit <- fitdist(as.numeric(normalized), "beta",lower=c(0,0))
summary(fit)
plot(fit, las = 1)
  
  
fit2 <- fitdist(as.numeric(normalized), "gamma",lower=c(0,0))
summary(fit2)
plot(fit2,las=1)

##beta dist is a much better fit

##AUFGABE 2

dax <- import("dax30.sav")

## 3 Options

#1.Option

#a

mean(dax$daxdelta,na.rm=TRUE)

#b
daxl <- subset(dax,year(dax$datum)>1996&year(dax$datum)<2000)

mean(daxl$daxdelta,na.rm=TRUE)



#2.Option

#a
fitdax1 <- lm(na.omit(dax$dax30)~c(1:length(na.omit(dax$dax30))))
summary(fitdax1)
plot(na.omit(dax$dax30)~c(1:length(na.omit(dax$dax30))))
abline(fitdax1,col="red")
coefficients(fitdax1)[2]##slope

#b
fitdax1 <- lm(na.omit(daxl$dax30)~c(1:length(na.omit(daxl$dax30))))
summary(fitdax1)
plot(na.omit(daxl$dax30)~c(1:length(na.omit(daxl$dax30))))
abline(fitdax1,col="red")
coefficients(fitdax1)[2]##slope
daxl

#3.Option

#a

(na.omit(dax$dax30)[length(na.omit(dax$dax30))]-na.omit(dax$dax30)[1])/(length(na.omit(dax$dax30))-1)

#b

(na.omit(daxl$dax30)[length(na.omit(daxl$dax30))]-na.omit(daxl$dax30)[1])/(length(na.omit(daxl$dax30))-1)


## to me, the 2. option is the healthiest, the 1. option is correct if you meant average delta instead of slope