##ÜBUNGSAUFGABEN 3
install.packages("stringr")
install.packages("rio")
library(stringr)

##AUFGABE 1
cat(paste(shQuote(append(paste0("s-",1:15),paste0("n-",1:15)), type="cmd"), collapse=","))

##AUFGABE 2


geburtstag <- rio::import("geburtstag.sav")
str(geburtstag)

##min length
min_length <- geburtstag$beschrei[which(str_length(geburtstag$beschrei)==min(str_length(geburtstag$beschrei)))]## this gives one "" as answer but this is not a word; Anzahl personen mit keine bescrei = 1

geburtstag$beschrei[which.min(str_length(geburtstag$beschrei))] <- "something relatively long temporarily" ## i will alter this to NA afterwards

min_length <- geburtstag$beschrei[which(str_length(geburtstag$beschrei)==min(str_length(geburtstag$beschrei)))]## now it gives the correct answer

index_min <- which(str_length(geburtstag$beschrei)==min(str_length(geburtstag$beschrei)))## index numbers min
anzahl_min <- length(min_length)##anzahl min

##max length
max_length <- geburtstag$beschrei[which(str_length(geburtstag$beschrei)==max(str_length(geburtstag$beschrei)))]

index_max <- which(str_length(geburtstag$beschrei)==max(str_length(geburtstag$beschrei)))## index numbers max
anzahl_max <- length(max_length)##anzahl max

##change temp string to NA
geburtstag$beschrei[which(geburtstag$beschrei=="something relatively long temporarily")] <- NA

##AUFGABE 3

##names
namen_max_beschrei <- geburtstag$name[index_max]

##sortierung
namen_max_beschrei <- sort(namen_max_beschrei)

##AUFGABE 4

##names that start with letter M
m <- geburtstag$name[startsWith(geburtstag$name,"M")]

##names that start with letter M or A
ma <- geburtstag$name[startsWith(geburtstag$name,c("M","A"))]

##names that end with us
us <- geburtstag$name[endsWith(geburtstag$name,"us")]

##names that start with m and end with u
mu <- m[endsWith(m,"u")]
mu

#names that contain the letter j
j <- geburtstag$name[grepl("J",geburtstag$name,fixed=TRUE)]

##frequency of albert
length(geburtstag$name[grepl("Albert",geburtstag$name,fixed=TRUE)])

##AUFGABE 4

##declare empty vector
ersteteil <- c()

## get first names
for( i in 1: length(geburtstag$name)){

  ersteteil <- append(ersteteil,strsplit(geburtstag$name, "\\s+")[[i]][1])
}

##frequency of albert

length(ersteteil[grepl("Albert",ersteteil,fixed=TRUE)])







