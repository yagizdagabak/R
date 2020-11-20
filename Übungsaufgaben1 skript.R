install.packages("rio")
install.packages("tidyverse")
##AUFGABE 1

## i pasted the table at kino.docx to an excel document and replaced the missing value with a 0

str(kino)  #here, kino$b is encoded as a chr variable possibly due to the fact that i replaced the missing value manually on the excel file


kino$b <- as.integer(kino$b) #produced NA for the missing value

##replace the NA with 0
kino$b[8] <- 0

##??factor n??

##command to factor g
kino$g <- factor(kino$g,levels=0:2,labels=c("keine Angabe","männlich","weiblich"))

##command to factor a
kino$a <- cut(kino$a, breaks = c(-2,-0.5,25,50,75,100), labels = c("keine Angabe","-25","25-50","50-75","75+"))

##command to factor b
kino$b <- factor(kino$b,levels=0:6,labels=c("Keine Angabe","Schüler","Student","Selbständiger","Arbeiter","Angestellter","kein Beruf"))

##command to factor f
kino$f <- factor(kino$f,levels=0:5,labels=c("keine Angabe","überhaupt nicht","nicht","teils ja/teils nein","gut","sehr gut"))

##command to factor k

kino$k <- cut(kinok$k, breaks = c(-2,-0.5,1,3,6,10,100000), labels = c("keine Angabe","0-1","2-3","4-6","7-10","10+"))

##save data frame
save(kino,file="kino_1.Rdata")

##AUFGABE 2

## create sequence named nr
nr <- c(1:10)

## create and factor categorical g
g_num <- rep(1:2,5)

g <- factor(g_num,1:2,labels=c("männlich","weiblich"))str

## create and factor categorical variable l
l_num <- rep(1:5,2)
l <- factor(l_num,1:5,labels=c("Kaufland","Lidl","Aldi","Netto","Andere"))


##create and factor ordinal variable a
a <- rep(1:4,2)
a[9] <- 2
a[10] <- 3
a <- factor(a,1:4,labels=c("Voralkolische Phase","Anfangsphase","Kritische Phase","Chronische Phase"))

## create metric variable s
s <- c(36,40,41,42,44,43,41.5,40.5,38,45)

## create metric variable m
m <- c(3000,3250,1790,2600,2450,8000,11000,17000,1500,24000)

## create and factor nominal variable st

st <- c("VWL","BWl","Anglistik","Kunstgeschichte","Soziologie","Physik","Biologie","Informatik","Mathematik","Medizin")

st <- factor(st)

##create and factor ordinal variable ord

ord <- c(11:20)

ord <- ordered(ord,levels=11:20,labels=c("11","12","13","14","15","16","17","18","19","20"))

##create d

m <- Sys.Date()

d <- seq(110,200, by=10)

d <- d+m

##create data frame neun_variablen

neun_variablen <- data.frame(nr=nr,d=d,g=g,a=a,m=m,l=l,s=s,st=st,ord=ord)

##save data_frame neunvariablen.rds

save(neun_variablen,file="neun_variablen.rds")



               