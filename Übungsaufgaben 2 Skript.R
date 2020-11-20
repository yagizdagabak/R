##Übungsaufgaben 2

install.packages("rio")

##AUFGABE 1

##1.1 erste Sequenz erstellen

noquote(paste(noquote(rep(letters[1:2],4)),collapse=","))
        
##1.2 zweite Sequenz erstellen

noquote(paste(noquote(rep(letters[1:2],each=4)),collapse=","))

##1.3 dritte Sequenz erstellen

noquote(paste(noquote(rep(letters[1:2],each=2, times=2)),collapse=","))

## AUFGABE 2

##2.1 Erzeugung von k

k <- seq(1:9)
k

##2.2.1 Erzeugung von l

l <- noquote(rep(letters[1:3],each=3))
l

##2.2.2 Erzeugung von m

m <- noquote(rep(letters[1:3],times =3))
m

##2.2.3 Erzeugung von n
n <- append(noquote(rep(letters[1:3],1:3)),rep(NA,3))
n

##2.3.1 Erzeugung von matrix a

a <-  noquote(matrix(data=c(k,l,m,n),ncol=4))
a

##2.3.2 Erzeugung von data frame b
b <- data.frame(k,l,m,n)
b

##2.3.3 Aenderung von Namen
names(b) <- c("eins","zwei","drei","vier")
b

##AUFGABE 3

## 3.1 Erzeugung von v, b & z

v = "Vorwärts"
b = "Berlin"
z = "!"

## 3.2.1

paste0(paste(v,b),z)

##3.2.2

rep(paste0(paste(v,b),z),3)

##3.2.3

rep(c(paste0(v,z),paste0(b,z)),2)

##3.2.4

rep(c(paste0(v,z),paste0(b,z)),each=2)


#AUFGABE 4

## create ortsteile string

ortsteile <- "Gesundbrunnen, Hansaviertel, Mitte, Moabit, Tiergarten, Wedding, Frederichshain, Kreuzberg, Blankenburg, Blankenfelde, Buch, Französisch Buchholz, Heinersdorf, Karow, Pankow, Niederschönhausen, Prenzlauer Berg, Rosenthal, Stadtrandsiedlung Malchow, Weißensee, Wilhelmsruh, Charlottenburg, Charlottenburg-Nord, Grunewald, Halensee, Schmargendorf, Westend, Wilmersdorf, Falkenhagener Feld, Gatow, Hakenfelde, Haselhorst, Kladow, Siemensstadt, Spandau, Staaken, Wilhelmstadt, Dahlem, Lankwitz, Lichterfelde, Nikolassee, Steglitz, Wannsee, Zehlendorf, Friedenau, Lichtenrade, Mariendorf, Marienfelde, Schöneberg, Tempelhof, Buckow, Britz, Gropiusstadt, Neukölln, Rudow, Adlershof, Altglienicke, Alt-Treptow, Baumschulenweg, Bohnsdorf, Friedrichshagen, Grünau, Johannisthal, Köpenick, Müggelheim, Niederschöneweide, Oberschöneweide, Plänterwald, Rahnsdorf, Schmöckwitz, Biesdorf, Hellersdorf, Kaulsdorf, Mahlsdorf, Marzahn, Alt-Hohenschönhausen, Falkenberg, Fennpfuhl, Friedrichsfelde, Karlshorst, Lichtenberg, Malchow, Neu-Hohenschönhausen, Rummelsburg, Wartenberg, Borsigwalde, Frohnau, Heiligensee, Hermsdorf, Konradshöhe, Lübars, Märkisches Viertel, Reinickendorf, Tegel, Waidmannslust, Wittenau"

## code to signify each ortsteile as "ortsteile_x"

cat(gsub("(\\w+)", '"\\1"', ortsteile))

## copy and paste the output and change german-only letters to create a vector of ortsteilen, also correct for errors bc the code miscorrects for "-" and " "

ortsteile_vec <- c("Gesundbrunnen", "Hansaviertel", "Mitte", "Moabit", "Tiergarten", "Wedding","Frederichshain","Kreuzberg", "Blankenburg", "Blankenfelde", "Buch", "Franzoesisch Buchholz", "Heinersdorf", "Karow", "Pankow", "Niederschoenhausen", "Prenzlauer Berg", "Rosenthal", "Stadtrandsiedlung Malchow", "Weissensee", "Wilhelmsruh", "Charlottenburg", "Charlottenburg-Nord", "Grunewald", "Halensee", "Schmargendorf", "Westend", "Wilmersdorf", "Falkenhagener Feld", "Gatow", "Hakenfelde", "Haselhorst", "Kladow", "Siemensstadt", "Spandau", "Staaken", "Wilhelmstadt", "Dahlem", "Lankwitz", "Lichterfelde", "Nikolassee", "Steglitz", "Wannsee", "Zehlendorf", "Friedenau", "Lichtenrade", "Mariendorf", "Marienfelde", "Schoeneberg", "Tempelhof", "Buckow", "Britz", "Gropiusstadt", "Neukoelln", "Rudow", "Adlershof", "Altglienicke", "Alt-Treptow", "Baumschulenweg", "Bohnsdorf", "Friedrichshagen", "Gruenau", "Johannisthal", "Koepenick", "Mueggelheim", "Niederschoeneweide", "Oberschoeneweide", "Plaenterwald", "Rahnsdorf", "Schmoeckwitz", "Biesdorf", "Hellersdorf", "Kaulsdorf", "Mahlsdorf", "Marzahn", "Alt-Hohenschoenhausen", "Falkenberg", "Fennpfuhl", "Friedrichsfelde", "Karlshorst", "Lichtenberg", "Malchow", "Neu-Hohenschoenhausen", "Rummelsburg", "Wartenberg", "Borsigwalde", "Frohnau", "Heiligensee", "Hermsdorf", "Konradshoehe", "Luebars", "Maerkisches Viertel", "Reinickendorf", "Tegel", "Waidmannslust", "Wittenau")

## create abkürzungen

abk_ort <- abbreviate(ortsteile_vec, minlength = 3, use.classes = TRUE,
                      dot = FALSE, strict = TRUE,
                      method = c("left.kept", "both.sides"), named = TRUE)


##AUFGABE 5

##the datafile had an .xls extension, i converted it to an .xlsx extension

dataDE <- rio::import("dataDE1.xlsx")

str(dataDE)

## code for pasting

paste(dataDE$land[1:16],paste0('(',dataDE$abk[1:16],')',";"),dataDE$hauptstadt[1:16])


