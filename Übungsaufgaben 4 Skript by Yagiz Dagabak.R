##ÜBUNGSBLATT 4

install.packages("lubridate")
install.packages("rvest")
install.packages("WikipediR")
install.packages("gt")
install.packages("webshot")

library(gt)
library(WikipediR)
library(lubridate)
library(rvest)
library(webshot)

##AUFGABE 1

heute <- Sys.Date() -2
heute1 <- format(heute,"%Y/%B")
heute1
heute2 <- format(heute,"%Y%m%d")
heute2
heute3 <- format(heute,"KW: %W")
heute3

##AUFGABE 2-5

origin <- as.Date("01.01.0000",format="%d.%m.%Y")
d1 <- as.Date("01.15.2020",format="%m.%d.%Y")
str(d1)
leap_year(d1)##TRUE
month(d1,label=TRUE)##January
wday(d1,label=TRUE,week_start=1)##Wed
wday(d1+3,label=TRUE,week_start=1)##Sat
d1+3
day(d1)%%2 ## != 0

sincejesusbirth <- d1-origin #Time difference of 737804 days

##BONUS
##code to create a table of significant events that took place on jan 15
d1wiki <- read_html("https://en.wikipedia.org/wiki/January_15") ##extract from wiki

text <- html_nodes(d1wiki, "ul > li") %>% html_text(trim=TRUE)#save nodes

events_text <- as.vector(text[7:55])#isolate "event" nodes

##seperate year
events_year <-c()
  for(i in 1:49) {
 events_year <- append(events_year, strsplit(events_text[i],split=" – ")[[1]][1])
  }

events_year <- as.numeric(events_year)

##seperate desc
events_desc <-c()
for(i in 1:49) {
  events_desc <- append(events_desc, strsplit(events_text[i],split=" – ")[[1]][2])
}

events_desc

##create data frame
events_df <- data.frame(events_year,events_desc)
events_df

names(events_df) <- c("Year","Description")

##create table
events_tbl <- gt(data=events_df)
events_tbl


  events_tbl <- 
  events_tbl %>%
  tab_header(
    title = "Events of Jan. 15",
    
  )
  events_tbl <- 
    cols_align(events_tbl, align =  "center", columns = TRUE)

 
  events_tbl <- events_tbl %>%
    data_color(
    columns = vars(Year),
    colors="gray"
  )
## save
gtsave(events_tbl,file="Jan 15(1).png")
  
##AUFGABE 6
  
 d2 <- ydm_hms("2020-15-01 12:30:45",tz="Europe/London")
  str(d2)
  hour(d2)
  minute(d2)
  second(d2)
  
##AUFGABE 7
  
now <- now()
  
nowinlnd <- now(tz="Europe/London")
  
## AUFGABE 8
  
OlsonNames()
##New york time
nowny <- now(tz="US/Eastern")
nowny
## Buenos Aires time
nowba <- now(tz="America/Argentina/Buenos_Aires" )
nowba

##AUFGABE 9

##9.1.
a <- dmy("01 September 2020")
b <- dmy_hms("11-07-2020 07:20:30")
dif1 <- difftime(a,b,units="h")
dif1

##9.2.
dif2 <- difftime(Sys.time(),b,units="h")
dif2


##AUFGABE 10

heute4 <- dmy_hm("25. August 2019 12:30")

##einen Tag hinfügen

heute4 + days(1)

##zwei wochen hinfügen

heute4 + weeks(2)

## datum - 3 jahre und 4 monaten

heute4 - years(3) - months(4)

##AUFGABE 11

##a)

seq(from = Sys.Date(), to = Sys.Date()+5, by = 1) 

##b)

seq(from = Sys.Date(), to = Sys.Date()-weeks(6), by =-7) 

##c)

seq(from = Sys.Date(), to = Sys.Date()+years(4), by="year") 

##AUFGABE 12



dvec <- dmy(c("13.06.2020", "15.06.2020","17.06.2020"))

floor_date(dvec,unit="months")

ceiling_date(dvec,unit="months")

round_date(dvec,unit="months")

##AUFGABE 13

dty <- seq(from=dmy("01.01.20"),to=dmy("31.12.20"),by=1)
dty

dty13 <- dty[which(day(dty)==13)]
is_weekend <- append(dty13[which((wday(dty13,label=TRUE))=="Sun")],dty13[which((wday(dty13,label=TRUE))=="Sat")])

str(is_weekend)
wday(is_weekend,label=TRUE)




##AUFGABE 14


dtym <- seq(from=dmy("01.05.20"),to=dmy("31.05.20"),by=1)

mothersday2020 <- dtym[which((wday(dtym,label=TRUE))=="Sun")][2]

