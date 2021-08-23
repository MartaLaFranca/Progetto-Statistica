RCFL_Microdati_2020_Secondo_trimestre<-read.csv(file.choose(), sep = '\t')

#Librerie
library(car)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(GridExtra)

# Funzioni 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#rm(list = ls(all = TRUE))
RCFL_Microdati_2020_Secondo_trimestre<-read.csv(file.choose(), sep = '\t')

# Funzioni 
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#

#Librerie
library(car)
library(dplyr)
library(ggplot2)
library(Hmisc)

detach(RCFL_Microdati_2020_Secondo_trimestre)
attach(RCFL_Microdati_2020_Secondo_trimestre)

#Parte A

ncol(RCFL_Microdati_2020_Secondo_trimestre) # 343
nrow(RCFL_Microdati_2020_Secondo_trimestre) # 101600


summary(RCFL_Microdati_2020_Secondo_trimestre$IDNO)
#Length  Class   Mode 
#0   NULL   NULL 

summary(RCFL_Microdati_2020_Secondo_trimestre$TITL)
#Length  Class   Mode 
#0   NULL   NULL 

summary(RCFL_Microdati_2020_Secondo_trimestre$VERSION)
#Length  Class   Mode 
#0   NULL   NULL 

summary(RCFL_Microdati_2020_Secondo_trimestre$RELEASE)
#Length  Class   Mode 
#0   NULL   NULL 



# Anno e mese di riferimento dell'indagine
summary(RCFL_Microdati_2020_Secondo_trimestre$ANNO) # 2020
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2020    2020    2020    2020    2020    2020 

summary(RCFL_Microdati_2020_Secondo_trimestre$TRIM) # 2
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2       2       2       2       2       2 
# Wave di quartina o Gruppo di rotazione

summary(RCFL_Microdati_2020_Secondo_trimestre$WAVQUA)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   3.000   2.483   4.000   4.000 

# RIP5    A. Ripartizione geografica in 5 classi
RCFL_Microdati_2020_Secondo_trimestre$RIP5 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$RIP5)

levels(RCFL_Microdati_2020_Secondo_trimestre$RIP5) <- c("Nord Ovest", "Nord Est", "Centro", "Sud", "Isole")

table(RCFL_Microdati_2020_Secondo_trimestre$RIP5, useNA = "ifany")
#Nord Ovest   Nord Est     Centro        Sud      Isole 
#30967      21010      21637      18352       9634 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$RIP5, useNA = "ifany")),2)
#Nord Ovest   Nord Est     Centro        Sud      Isole 
#30.48      20.68      21.30      18.06       9.48 

# RIP 3   A. Ripartizione geografica in 3 classi
RCFL_Microdati_2020_Secondo_trimestre$RIP3 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$RIP3)
levels(RCFL_Microdati_2020_Secondo_trimestre$RIP3) <- c("Nord", "Centro", "Mezzogiorno")

table(RCFL_Microdati_2020_Secondo_trimestre$RIP3, useNA = "ifany")
#Nord      Centro Mezzogiorno 
#51977       21637       27986 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$RIP3, useNA = "ifany")),2)
#Nord      Centro Mezzogiorno 
#51.16       21.30       27.55 

# Tabella incrociata RIP3 + RIP5
table(RCFL_Microdati_2020_Secondo_trimestre$RIP3, RCFL_Microdati_2020_Secondo_trimestre$RIP5)
#Nord Ovest Nord Est Centro   Sud Isole
#Nord             30967    21010      0     0     0
#Centro               0        0  21637     0     0
#Mezzogiorno          0        0      0 18352  9634

risp_per_area <- RCFL_Microdati_2020_Secondo_trimestre %>%
  group_by(RCFL_Microdati_2020_Secondo_trimestre$RIP3) %>%
  summarise(Freq = n())
risp_per_area$perc <-
  round(100 * risp_per_area$Freq / sum(risp_per_area$Freq), 2)
risp_per_area$label <-
  paste(paste(risp_per_area$Freq, risp_per_area$perc, sep = " ("),
        "%)",
        sep = "")


# Rispondenti per area geografica ####
par(mar = c(5, 6, 1, 1),
    oma = c(0, 0, 0, 0),
    las = 1)

posbar <- barplot(
  risp_per_area$Freq,
  names.arg = c("Nord", "Centro","Mezzogiorno"),
  main = "N° di rispondenti per area geografica",
  col = c("#56B4E9"),
  horiz = TRUE,
  xlim = c(0, 60000)
)
text(
  y = posbar,
  x = risp_per_area$Freq,
  pos = 2,
  labels = risp_per_area$label
)
minor.tick(nx=5, ny = 1)



# REG     A. Regione di residenza
RCFL_Microdati_2020_Secondo_trimestre$REG <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$REG)
levels(RCFL_Microdati_2020_Secondo_trimestre$REG) <-
  c(
    "Piemonte",
    "Valle d'Aosta",
    "Lombardia",
    "Trentino Alto-Adige",
    "Veneto",
    "Friuli Venezia Giulia",
    "Liguria",
    "Emilia Romagna",
    "Toscana",
    "Umbria",
    "Marche",
    "Lazio",
    "Abruzzo",
    "Molise",
    "Campania",
    "Puglia",
    "Basilicata",
    "Calabria",
    "Sicilia",
    "Sardegna"
  )


table(RCFL_Microdati_2020_Secondo_trimestre$REG, useNA = "ifany")
#Piemonte         Valle d'Aosta             Lombardia   Trentino Alto-Adige                Veneto 
#                 9011                  2857                 15253                  5579                  6344 
#Friuli Venezia Giulia               Liguria        Emilia Romagna               Toscana                Umbria 
#                 4238                  3846                  4849                  7798                  3186 
#               Marche                 Lazio               Abruzzo                Molise              Campania 
#                 3671                  6982                  2995                  1758                  4374 
#               Puglia            Basilicata              Calabria               Sicilia              Sardegna 
#                 2301                  3186                  3738                  5889                  3745 
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$REG, useNA = "ifany")),2)
#Piemonte         Valle d'Aosta             Lombardia   Trentino Alto-Adige                Veneto 
#                 8.87                  2.81                 15.01                  5.49                  6.24 
#Friuli Venezia Giulia               Liguria        Emilia Romagna               Toscana                Umbria 
#                 4.17                  3.79                  4.77                  7.68                  3.14 
#               Marche                 Lazio               Abruzzo                Molise              Campania 
#                 3.61                  6.87                  2.95                  1.73                  4.31 
#               Puglia            Basilicata              Calabria               Sicilia              Sardegna 
#                 2.26                  3.14                  3.68                  5.80                  3.69 
RCFL_Microdati_2020_Secondo_trimestre$PROVCM <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$PROVCM)



# GRACOM  A. Codice grandi comuni
RCFL_Microdati_2020_Secondo_trimestre$GRACOM <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$GRACOM)
levels(RCFL_Microdati_2020_Secondo_trimestre$GRACOM) <-
  c(
    "Torino",
    "Genova",
    "Milano",
    "Verona",
    "Venezia",
    "Bologna",
    "Firenze",
    "Roma",
    "Napoli",
    "Bari",
    "Palermo",
    "Messina",
    "Catania"
  )


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$GRACOM, useNA = "ifany")),2)
#Torino  Genova  Milano  Verona Venezia Bologna Firenze    Roma  Napoli    Bari Palermo Messina Catania    <NA> 
#  0.77    1.09    0.91    0.27    0.26    0.47    0.44    1.29    1.31    0.49    1.56    0.46    0.61   90.08

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$GRACOM, useNA = "no")),2)
#Torino  Genova  Milano  Verona Venezia Bologna Firenze    Roma  Napoli    Bari Palermo Messina Catania 
#7.76   10.94    9.14    2.69    2.67    4.75    4.40   12.97   13.25    4.89   15.76    4.59    6.18 

# MFRAM   A. Codice famiglia
summary(RCFL_Microdati_2020_Secondo_trimestre$MFRFAM)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1   15182   30226   30229   45233   60457 

# Conteggio famiglie intervistate
length(summary(as.factor(RCFL_Microdati_2020_Secondo_trimestre$MFRFAM), maxsum = 50000)) #  46413


# RPN2    A. Relazione di parentela nel nucleo 2
RCFL_Microdati_2020_Secondo_trimestre$RPN2 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$RPN2)
levels(RCFL_Microdati_2020_Secondo_trimestre$RPN2) <-
  c("Persona singola",
    "Capo nucleo",
    "Coniuge o convivente",
    "Figlio")

table(RCFL_Microdati_2020_Secondo_trimestre$RPN2, useNA = "ifany")
#Persona singola          Capo nucleo Coniuge o convivente               Figlio 
#17949                30508                26689                26454 
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$RPN2, useNA = "ifany")),2)
#Persona singola          Capo nucleo Coniuge o convivente               Figlio 
#17.67                30.03                26.27                26.04 
RCFL_Microdati_2020_Secondo_trimestre$TFM <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$TFM)


# TN2     A. Tipo nucleo 2
RCFL_Microdati_2020_Secondo_trimestre$TN2 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$TN2)
levels(RCFL_Microdati_2020_Secondo_trimestre$TN2) <-
  c(
    "Persona isolata",
    "Coppia con figli",
    "Coppia senza figli",
    "Monogenitore maschio",
    "Monogenitore femmina"
  )


# Attenzione! Questa non è la distribuzione delle tipologie di famiglie,
# ma la distribuzione degli individui! Verosimilmente, per ogni coppia 
# ci saranno almeno due individui che rispondono
table(RCFL_Microdati_2020_Secondo_trimestre$TN2, useNA = "ifany")
## 
##       Persona isolata     Coppia con figli   Coppia senza figli Monogenitore maschio Monogenitore femmina 
##       17949                48031                26760                 1357                 7503  

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$TN2, useNA = "ifany")),2)
## 
##      Persona isolata     Coppia con figli   Coppia senza figli Monogenitore maschio Monogenitore femmina 
##      17.67                47.27                26.34                 1.34                 7.38 


# SG4     A. Numero persone che vivono in casa
# Come sopra! L'unità elementare nel RCFL_Microdati_2020_Secondo_trimestre non è la famiglia, ma l'individuo
summary(RCFL_Microdati_2020_Secondo_trimestre$SG4)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.000   2.000   3.000   2.801   4.000  11.000 

sd(RCFL_Microdati_2020_Secondo_trimestre$SG4)
## [1] 1.266771
par(las = 1)
par( mar=c(3.1, 4.7, 2.3, 0)) 
hist(RCFL_Microdati_2020_Secondo_trimestre$SG4,
     main = "",
     xlab = "", ylab = "",
     ylim =c(0,35000),
     col = c("#56B4E9"),
     breaks = c(0:max(RCFL_Microdati_2020_Secondo_trimestre$SG4)))
minor.tick(ny=5)
mtext(side=1, text="N° persone",  line=1.8)
mtext(side=2, text="Frequenza", line=0.5)
mtext(side=3, text="N° di persone che vivono in casa",  line=0.1)
#histogram <- ggplot(RCFL_Microdati_2020_Secondo_trimestre, aes=(RCFL_Microdati_2020_Secondo_trimestre$SG4)+geom_histogram(stat = "count", color='black', fill='white'))
#histogram


# SG11    A. Genere
RCFL_Microdati_2020_Secondo_trimestre$SG11 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG11)
levels(RCFL_Microdati_2020_Secondo_trimestre$SG11) <- c("Maschio", "Femmina")

table(RCFL_Microdati_2020_Secondo_trimestre$SG11, useNA = "ifany")
## 
## Maschio Femmina 
## 48224   53376 
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG11, useNA = "ifany")),2)
## 
## Maschio Femmina 
## 47.46   52.54 
# ETAM    A. Età in anni compiuti
summary(RCFL_Microdati_2020_Secondo_trimestre$ETAM)
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.00   32.00   53.00   48.85   70.00   75.00 
sd(RCFL_Microdati_2020_Secondo_trimestre$ETAM)
## [1] 22.8972
par( mar=c(3.1, 4.7, 2.3, 0))
hist(RCFL_Microdati_2020_Secondo_trimestre$ETAM,
     main = "",
     xlab = "", ylab = "", 
     xlim=c(0,80),
     ylim =c(0,25000),
     col=("#56B4E9"),
)
minor.tick(nx = 4, ny=5)
mtext(side=1, text="Età",  line=1.8)
mtext(side=2, text="Frequenza", line=0.5)
mtext(side=3, text="Distribuzione individui per età",  line=0.1)

# La distribuzione dell'età è asimmetrica positiva! 
# La mediana è superiore alla media


# CLETAD  A. Classi di età decennali
RCFL_Microdati_2020_Secondo_trimestre$CLETAD <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$CLETAD)
levels(RCFL_Microdati_2020_Secondo_trimestre$CLETAD) <-
  c(
    "0-14 anni",
    "15-24 anni",
    "25-34 anni",
    "35-44 anni",
    "45-54 anni",
    "55-64 anni",
    "65-74 anni",
    "75 anni e più"
  )

table(RCFL_Microdati_2020_Secondo_trimestre$CLETAD, useNA = "ifany")
## 
##      0-14 anni    15-24 anni    25-34 anni    35-44 anni    45-54 anni    55-64 anni    65-74 anni 75 anni e più 
##        11072          8591          7802         10722         14512         14877         15343         18681 
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CLETAD, useNA = "ifany")),2)
## 
##      0-14 anni    15-24 anni    25-34 anni    35-44 anni    45-54 anni    55-64 anni    65-74 anni 75 anni e più 
##        10.90          8.46          7.68         10.55         14.28         14.64         15.10         18.39 
# Il 16,64% della popolazione ha più di 75 anni

risp_per_eta <- RCFL_Microdati_2020_Secondo_trimestre %>%
  group_by(CLETAD) %>%
  summarise(Freq = n())
risp_per_eta$perc <-
  round(100 * risp_per_eta$Freq / sum(risp_per_eta$Freq), 2)
risp_per_eta$label <-
  paste(paste(risp_per_eta$Freq, risp_per_eta$perc, sep = " ("),
        "%)",
        sep = "")


# Rispondenti per classe di età ####
barplot(
  risp_per_eta$Freq,
  names.arg = risp_per_eta$CLETAD,
  main = "N° Individui per classe di età",
  xlab = "Classe di età", ylab = "Frequenza",
  ylim =c(0,20000),
  col=("#56B4E9")
)
minor.tick(nx = 0,ny=5)


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CLETAD, useNA = "ifany")),2)
## 
##     0-14 anni    15-24 anni    25-34 anni    35-44 anni    45-54 anni    55-64 anni    65-74 anni 75 anni e più 
##       10.90          8.46          7.68         10.55         14.28         14.64         15.10         18.39 
# Tabella incrociata: Classe di età vs Area Geografica
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CLETAD,RCFL_Microdati_2020_Secondo_trimestre$RIP3, useNA = "ifany"), margin = 1),2)
##                
##                   Nord Centro Mezzogiorno
##    0-14 anni     52.14  21.33       26.53
##    15-24 anni    49.41  20.85       29.74
##    25-34 anni    49.65  21.26       29.08
##    35-44 anni    49.53  22.86       27.61
##    45-54 anni    51.84  21.54       26.62
##    55-64 anni    50.89  21.38       27.73
##    65-74 anni    50.87  21.40       27.73
##    75 anni e più 52.86  20.25       26.89

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CLETAD,RCFL_Microdati_2020_Secondo_trimestre$RIP3, useNA = "ifany"), margin = 2),2)
##                
##                  Nord Centro Mezzogiorno
##   0-14 anni     11.59  11.87       11.66
##   15-24 anni     8.37   8.53        9.19
##   25-34 anni     8.32   8.38        9.05
##   35-44 anni    11.43  12.09       11.58
##   45-54 anni    15.17  15.05       14.40
##   55-64 anni    14.23  14.57       13.87
##   65-74 anni    13.95  13.40       13.76
##   75 anni e più 16.95  16.11       16.49
# CLETAQ  A. Classi di età quinquennali
# CLETAS  A. Età in 17 classi



# STACIM  A. Stato civile
RCFL_Microdati_2020_Secondo_trimestre$STACIM <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$STACIM)
levels(RCFL_Microdati_2020_Secondo_trimestre$STACIM) <-
  c("Celibe/nubile",
    "Coniugato/a",
    "Separato/a o divorziato/a",
    "Vedovo/a")

table(RCFL_Microdati_2020_Secondo_trimestre$STACIM, useNA = "ifany")
## 
##    Celibe/nubile               Coniugato/a      Separato/a o divorziato/a          Vedovo/a 
##       36273                     48918                      6478                      9931 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$STACIM, useNA = "ifany")),2)
## 
##              Celibe/nubile               Coniugato/a        Separato/a o divorziato/a          Vedovo/a 
#                   35.70                     48.15                      6.38                      9.77 
# AMATRI  A. Anno di matrimonio


# RELPAR  A. Relazione di parentela con la P.R
RCFL_Microdati_2020_Secondo_trimestre$RELPAR <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$RELPAR)


# SG13    A. Luodo di nascita
RCFL_Microdati_2020_Secondo_trimestre$SG13 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG13)
levels(RCFL_Microdati_2020_Secondo_trimestre$SG13) <- c("Italia", "Estero")

table(RCFL_Microdati_2020_Secondo_trimestre$SG13, useNA = "ifany")
## 
## Italia Estero 
## 93227   8373 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG13, useNA = "ifany")),2)
## 
## Italia Estero 
##  91.76   8.24 
# NASSES  A. Stato estero di nascita
ds$NASSES <- as.factor(ds$NASSES)


# CITTAD  A. Cittadinanza
RCFL_Microdati_2020_Secondo_trimestre$CITTAD <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$CITTAD)
levels(RCFL_Microdati_2020_Secondo_trimestre$CITTAD) <- c("Italiana", "UE", "Fuori UE")

table(RCFL_Microdati_2020_Secondo_trimestre$CITTAD, useNA = "ifany")
## 
## Italiana     UE    Fuori UE 
##  94663     2190     4747 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CITTAD, useNA = "ifany")),2)
## 
## Italiana     UE    Fuori UE 
##  93.17     2.16     4.67 

# SG16    A. Cittadinanza italiana
RCFL_Microdati_2020_Secondo_trimestre$SG16 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG16)
levels(RCFL_Microdati_2020_Secondo_trimestre$SG16) <- c("Si", "No")

table(RCFL_Microdati_2020_Secondo_trimestre$SG16, useNA = "ifany")
## 
##     Si    No 
##  94663  6937 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG16, useNA = "ifany")),2)
## 
##    
##   Si     No 
##  93.17  6.83 


# CITSES  A. Cittadinanza straniera
RCFL_Microdati_2020_Secondo_trimestre$CITSES <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$CITSES)

# SG18B   A. Anno in cui è venuto a vivere in Italia
# SG18D   A. Mese in cui è venuto a vivere in Italia

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$SG18B) & (RCFL_Microdati_2020_Secondo_trimestre$SG18B == 997 | RCFL_Microdati_2020_Secondo_trimestre$SG18B == 998), 'SG18B'] <- NA
summary(RCFL_Microdati_2020_Secondo_trimestre$SG18B)
##    M Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     1928    1996    2003    1999    2008    2020   93344 
# SG18F   A. Anno dal quale vive in Italia senza allontanarsi
# SG18G   A. Mese dal quale vive in Italia senza allontanarsi

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$SG18F) & (RCFL_Microdati_2020_Secondo_trimestre$SG18F == 997 | RCFL_Microdati_2020_Secondo_trimestre$SG18F == 998), 'SG18F'] <- NA
summary(RCFL_Microdati_2020_Secondo_trimestre$SG18F)
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     1953    1997    2007    2003    2014    2020  101443


# SG18E   A. Dalla prima volta in Italia, ha sempre vissuto qui
RCFL_Microdati_2020_Secondo_trimestre$SG18E <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG18E)
levels(RCFL_Microdati_2020_Secondo_trimestre$SG18E) <- c("Si", "No")

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG18E, useNA = "ifany")),2)
## 
##    Si    No  <NA> 
##   8.08  0.16 91.76 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG18E, useNA = "no")),2)
## 
##     Si    No 
##   98.05  1.95 


# TITSTUD A. Titolo di studio (10 modalità)
RCFL_Microdati_2020_Secondo_trimestre$TISTUD <- as.ordered(RCFL_Microdati_2020_Secondo_trimestre$TISTUD)
levels(RCFL_Microdati_2020_Secondo_trimestre$TISTUD) <-
  c(
    "Nessun titolo",
    "Licenza elementare",
    "Licenza media",
    "Diploma di qualifica professionale (2-3 anni)",
    "Diploma di maturità/Istr. secondaria di 4-5 anni",
    "Diploma di Accademia, Conservatorio, simili",
    "Diploma universitario di 2-3 anni",
    "Laurea di primo livello (triennale)",
    "Laurea specialistica/magistrale (biennale)",
    "Laurea di 4-6 anni"
  )

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$TISTUD, useNA = "ifany")),2)
## 
##          Nessun titolo                               Licenza elementare 
##            3.08                                            15.19 
##          Licenza media    Diploma di qualifica professionale (2-3 anni) 
##            27.86                                             5.23 
##          Diploma di maturità/Istr. secondaria di 4-5 anni      Diploma di Accademia, Conservatorio, simili 
##                  25.46                                             0.26 
##          Diploma universitario di 2-3 anni              Laurea di primo livello (triennale) 
##                0.62                                             2.09 
##          Laurea specialistica/magistrale (biennale)                               Laurea di 4-6 anni 
##                                      1.16                                             8.14 
##          <NA> 
##          10.90 


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$TISTUD, useNA = "no")),2)
## 
##                                     Nessun titolo                               Licenza elementare 
##                                        3.46                                            17.05 
##                                    Licenza media    Diploma di qualifica professionale (2-3 anni) 
##                                        31.27                                             5.87 
##                                    Diploma di maturità/Istr. secondaria di 4-5 anni      Diploma di Accademia, Conservatorio, simili 
##                                                28.58                                             0.29 
##                                    Diploma universitario di 2-3 anni              Laurea di primo livello (triennale) 
##                                            0.69                                             2.34 
##                                    Laurea specialistica/magistrale (biennale)          Laurea di 4-6 anni 
##                                                  1.30                                             9.13 


# EDULEV A. Titolo di studio (6 modalità)
RCFL_Microdati_2020_Secondo_trimestre$EDULEV <- as.ordered(RCFL_Microdati_2020_Secondo_trimestre$EDULEV)
levels(RCFL_Microdati_2020_Secondo_trimestre$EDULEV) <- c(
  "Nessun titolo",
  "Licenza elementare",
  "Licenza media",
  "Diploma 2-3",
  "Diploma 4-5",
  "Laurea"
)

table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV, useNA = "ifany")
## 
##       Nessun titolo Licenza elementare      Licenza media        Diploma 2-3        Diploma 4-5             Laurea 
##            3133              15436              28308               5318              25871              12462 
##        <NA> 
##        11072 


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV, useNA = "ifany")),2)
## 
##      Nessun titolo Licenza elementare      Licenza media        Diploma 2-3        Diploma 4-5             Laurea 
##            3.08              15.19              27.86               5.23              25.46              12.27 
##            <NA> 
##            10.90 


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV, useNA = "no")),2)
## 
##       Nessun titolo   Licenza elementare      Licenza media        Diploma 2-3        Diploma 4-5          Laurea 
##              3.46              17.05              31.27               5.87              28.58              13.77
# La percentuale di laureati è solo dell'11,09%!

risp_per_edulev <- RCFL_Microdati_2020_Secondo_trimestre %>%
  group_by(EDULEV) %>%
  summarise(Freq = n())
## Warning: Factor `EDULEV` contains implicit NA, consider using
## `forcats::fct_explicit_na`
risp_per_edulev$EDULEV <- as.character(risp_per_edulev$EDULEV)
risp_per_edulev$EDULEV[is.na(risp_per_edulev$EDULEV)] <- "Unknown"
risp_per_edulev$perc <-
  round(100 * risp_per_edulev$Freq / sum(risp_per_edulev$Freq), 2)
risp_per_edulev$label <-
  paste(paste(risp_per_edulev$Freq, risp_per_edulev$perc, sep = " ("),
        "%)",
        sep = "")

barplot(
  risp_per_edulev$Freq,
  names.arg = risp_per_edulev$EDULEV,
  main = "N° di individui per titolo di studio",
  xlab = "Titolo di studio", ylab = "Frequenza", 
  cex.names = 0.8,
  col=("#56B4E9")
)
minor.tick(nx = 0,ny=5)

# risp_per_edulev

# Tabella incrociata: Titolo di studio vs Genere
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV,RCFL_Microdati_2020_Secondo_trimestre$SG11, useNA = "ifany"), margin = 1),2)
##                    Maschio Femmina
##Nessun titolo        29.88   70.12
##Licenza elementare   38.84   61.16
##Licenza media        51.76   48.24
#Diploma 2-3          51.18   48.82
##Diploma 4-5          49.42   50.58
##Laurea               43.77   56.23
##<NA>                 51.29   48.71
# Considerando tutti gli individui la maggior parte dei laureati sono donne
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV,RCFL_Microdati_2020_Secondo_trimestre$SG11,RCFL_Microdati_2020_Secondo_trimestre$CLETAD, useNA = "ifany"), margin = 3),2)


##                    Maschio Femmina
##Nessun titolo         0.00    0.00
##Licenza elementare    0.00    0.00
##Licenza media         0.00    0.00
##Diploma 2-3           0.00    0.00
##Diploma 4-5           0.00    0.00
##Laurea                0.00    0.00
##<NA>                 51.29   48.71

##, ,  = 15-24 anni


##                    Maschio Femmina
##Nessun titolo         0.08    0.08
##Licenza elementare    0.42    0.31
##Licenza media        27.40   23.80
##Diploma 2-3           2.33    1.30
##Diploma 4-5          19.46   19.40
##Laurea                2.07    3.33
##<NA>                  0.00    0.00

##                  , ,  = 25-34 anni


##                    Maschio Femmina
##Nessun titolo         0.28    0.36
#Licenza elementare    0.45    0.50
##Licenza media        10.46    9.14
##Diploma 2-3           3.90    2.50
##Diploma 4-5          22.53   19.99
##Laurea               11.93   17.96
##<NA>                  0.00    0.00

##, ,  = 35-44 anni


##                  Maschio Femmina
##Nessun titolo         0.30    0.33
##Licenza elementare    0.91    0.90
##Licenza media        14.94   12.12
##Diploma 2-3           3.78    2.87
##Diploma 4-5          18.85   19.93
##Laurea                9.70   15.37
##<NA>                  0.00    0.00

##, ,  = 45-54 anni


##                  Maschio Femmina
##Nessun titolo         0.30    0.39
##Licenza elementare    1.44    1.48
##Licenza media        19.36   16.69
##Diploma 2-3           3.97    4.00
##Diploma 4-5          16.32   18.82
##Laurea                7.07   10.16
##<NA>                  0.00    0.00

##, ,  = 55-64 anni


##                    Maschio Femmina
##Nessun titolo         0.45    0.50
##Licenza elementare    2.75    4.52
##Licenza media        19.71   20.35
##Diploma 2-3           3.37    4.67
##Diploma 4-5          14.38   16.76
##Laurea                5.87    6.65
##<NA>                  0.00    0.00

##, ,  = 65-74 anni


##                  Maschio Femmina
##Nessun titolo         0.79    1.68
##Licenza elementare   10.58   17.68
##Licenza media        15.42   15.72
##Diploma 2-3           3.22    3.10
##Diploma 4-5          11.24    9.71
##Laurea                5.57    5.30
##<NA>                  0.00    0.00

##, ,  = 75 anni e più


##                  Maschio Femmina
##Nessun titolo         3.44    9.31
##Licenza elementare   19.19   30.40
##Licenza media         9.49    9.29
##Diploma 2-3           1.29    1.23
##Diploma 4-5           5.90    5.39
##Laurea                2.94    2.11
##<NA>                  0.00    0.00

# Tabella incrociata: Titolo di studio vs Area Geografica
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV,RCFL_Microdati_2020_Secondo_trimestre$RIP3, useNA = "ifany"), margin = 1),2)
##                     
##                   Nord   Centro    Mezzogiorno
##Nessun titolo      21.67  21.93       56.40
##Licenza elementare 51.24  20.37       28.39
##Licenza media      52.59  20.15       27.26
##Diploma 2-3        74.52  14.89       10.59
##Diploma 4-5        48.29  23.51       28.20
##Laurea             50.33  23.00       26.67
##<NA>               52.14  21.33       26.53
# Considerando tutti gli individui la maggior parte dei laureati risiedono al nord
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$EDULEV,RCFL_Microdati_2020_Secondo_trimestre$RIP3,RCFL_Microdati_2020_Secondo_trimestre$CLETAD, useNA = "ifany"), margin = 3),2)
## , ,  = 0-14 anni


##                  Nord Centro Mezzogiorno
##Nessun titolo       0.00   0.00        0.00
##Licenza elementare  0.00   0.00        0.00
##Licenza media       0.00   0.00        0.00
##Diploma 2-3         0.00   0.00        0.00
##Diploma 4-5         0.00   0.00        0.00
##Laurea              0.00   0.00        0.00
##<NA>               52.14  21.33       26.53

##, ,  = 15-24 anni


##                  Nord Centro Mezzogiorno
##Nessun titolo       0.05   0.08        0.03
##Licenza elementare  0.27   0.12        0.35
##Licenza media      24.97  10.88       15.35
##Diploma 2-3         2.93   0.41        0.29
##Diploma 4-5        18.36   8.22       12.29
##Laurea              2.84   1.14        1.42
##<NA>                0.00   0.00        0.00

##, ,  = 25-34 anni


##                Nord Centro Mezzogiorno
##Nessun titolo       0.15   0.26        0.23
##Licenza elementare  0.44   0.13        0.38
##Licenza media       9.69   4.04        5.87
##Diploma 2-3         4.82   0.79        0.78
##Diploma 4-5        19.12   9.27       14.14
##Laurea             15.43   6.78        7.68
##<NA>                0.00   0.00        0.00

##, ,  = 35-44 anni


##                  Nord Centro Mezzogiorno
##Nessun titolo       0.21   0.21        0.21
##Licenza elementare  0.70   0.32        0.79
##Licenza media      13.11   5.59        8.37
##Diploma 2-3         5.01   0.99        0.65
##Diploma 4-5        17.52   9.91       11.34
##Laurea             12.97   5.85        6.25
##<NA>                0.00   0.00        0.00

##, ,  = 45-54 anni


##                Nord Centro Mezzogiorno
##Nessun titolo       0.19   0.17        0.33
##Licenza elementare  0.97   0.40        1.55
##Licenza media      18.08   7.11       10.86
##Diploma 2-3         6.10   1.18        0.69
##Diploma 4-5        17.63   8.75        8.75
##Laurea              8.86   3.93        4.44
##<NA>                0.00   0.00        0.00

##, ,  = 55-64 anni


##                  Nord Centro Mezzogiorno
##Nessun titolo       0.22   0.12        0.62
##Licenza elementare  2.64   1.39        3.24
##Licenza media      20.66   8.17       11.23
##Diploma 2-3         5.83   1.25        0.96
##Diploma 4-5        15.57   7.43        8.15
##Laurea              5.98   3.02        3.54
##<NA>                0.00   0.00        0.00

##, ,  = 65-74 anni


##                Nord Centro Mezzogiorno
##Nessun titolo       0.48   0.33        1.66
##Licenza elementare 13.50   6.07        8.67
##Licenza media      17.37   6.25        7.52
##Diploma 2-3         4.41   1.06        0.85
##Diploma 4-5         9.97   5.13        5.85
##Laurea              5.14   2.56        3.17
##<NA>                0.00   0.00        0.00

##, ,  = 75 anni e più


##                  Nord Centro Mezzogiorno
##Nessun titolo       2.71   2.92        7.11
##Licenza elementare 27.68  10.13       11.78
##Licenza media      11.88   3.46        3.44
##Diploma 2-3         1.97   0.37        0.18
##Diploma 4-5         6.11   2.30        2.89
##Laurea              2.51   1.07        1.48
##<NA>                0.00   0.00        0.00
RCFL_Microdati_2020_Secondo_trimestre$HATLEV <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$HATLEV)



# SG24A   A. Tipo di diploma conseguito
RCFL_Microdati_2020_Secondo_trimestre$SG24A <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG24A)
levels(RCFL_Microdati_2020_Secondo_trimestre$SG24A) <-
  c(
    "Diploma vecchio ordinamento",
    "Diploma AFAM I livello",
    "Diploma AFAM II livello",
    "Non sa"
  )

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG24A, useNA = "ifany")),2)
## 
## Diploma vecchio ordinamento      Diploma AFAM I livello     Diploma AFAM II livello         Non sa 
##          0.17                        0.05                        0.04                        0.00 
##  <NA> 
##  99.74 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG24A, useNA = "no")),2)
## 
## Diploma vecchio ordinamento      Diploma AFAM I livello     Diploma AFAM II livello          Non sa 
##           65.15                       18.94                       15.53                      0.38 


# SG24B   A. Titolo di studio post-laurea, post-diploma
RCFL_Microdati_2020_Secondo_trimestre$SG24B <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG24B)
levels(RCFL_Microdati_2020_Secondo_trimestre$SG24B) <-
  c(
    "Master I livello",
    "Master II livello",
    "Diploma specializzazione universitaria",
    "Dottorato di ricerca",
    "Nessuno di questi"
  )


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG24B, useNA = "ifany")),2)
## 
##                      Master I livello                      Master II livello         Diploma specializzazione universitaria 
##                          0.46                                   0.16                                   0.55 
##                    Dottorato di ricerca                      Nessuno di questi                          <NA> 
##                            0.25                                  10.68                                  87.90 


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$SG24B, useNA = "no")),2)
## 
##                        Master I livello                      Master II livello          Diploma specializzazione universitaria 
##                            3.78                                   1.29                                   4.55 
##                        Dottorato di ricerca                      Nessuno di questi 
##                              2.09                                  88.30 


# SG25    A. Tipo di titolo di studio
RCFL_Microdati_2020_Secondo_trimestre$SG25 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$SG25)



# SG26    A. Anno di conseguimento del titolo di studio
summary(RCFL_Microdati_2020_Secondo_trimestre$SG26) # 14398 NA
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      997    1950    1975    1783    1996    2020   14205 
# 997 Non sa
# 998 Non risponde
RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$SG26) & (RCFL_Microdati_2020_Secondo_trimestre$SG26 == 997 | RCFL_Microdati_2020_Secondo_trimestre$SG26 == 998), 'SG26'] <- NA
summary(RCFL_Microdati_2020_Secondo_trimestre$SG26) #31987
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1927    1966    1983    1984    2002    2020   31987 


# SG27A    A. Mese di conseguimento del titolo di studio
summary(RCFL_Microdati_2020_Secondo_trimestre$SG27A) # 85271 NA
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     1.00    6.00    7.00   33.64    7.00  997.00   85271
# 997 Non sa
# 998 Non risponde
RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$SG27A) & (RCFL_Microdati_2020_Secondo_trimestre$SG27A == 997 | RCFL_Microdati_2020_Secondo_trimestre$SG27A == 998), 'SG27A'] <- NA
summary(RCFL_Microdati_2020_Secondo_trimestre$SG27A) # 85718  NA
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  1.00    6.00    7.00    6.53    7.00   12.00   85718 
# SG27B    A. Età di conseguimento del titolo di studio
summary(RCFL_Microdati_2020_Secondo_trimestre$SG27) # 83818 NA
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     8.00   13.00   14.00   15.86   19.00   70.00   83818


# COND3   A. Condizione professionale a 3 modalità
RCFL_Microdati_2020_Secondo_trimestre$COND3 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$COND3)
levels(RCFL_Microdati_2020_Secondo_trimestre$COND3) <- c("Occupati", "Persone in cerca di lavoro", "Inattivi")

table(RCFL_Microdati_2020_Secondo_trimestre$COND3, useNA = "ifany")
## 
##                    Occupati              Persone in cerca di lavoro                   Inattivi 
##                     34028                       2474                                   65098 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$COND3, useNA = "ifany")),2)
## 
##                    Occupati            Persone in cerca di lavoro                   Inattivi 
##                      33.49                       2.44                                64.07


# Condizione professionale vs Area Geografica
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$COND3, RCFL_Microdati_2020_Secondo_trimestre$RIP3), margin = 1),2)
##                             
##                            Nord Centro    Mezzogiorno
##Occupati                   55.55  22.70       21.75
##Persone in cerca di lavoro 40.66  19.89       39.45
##Inattivi                   49.26  20.61       30.12
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$COND3, RCFL_Microdati_2020_Secondo_trimestre$RIP3), margin = 2),2)
##                             
##                           Nord   Centro     Mezzogiorno
##Occupati                   36.36  35.71       26.45
##Persone in cerca di lavoro  1.94   2.27        3.49
##Inattivi                   61.70  62.02       70.07
RCFL_Microdati_2020_Secondo_trimestre$COND10 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$COND10)
levels(RCFL_Microdati_2020_Secondo_trimestre$COND10) <-
  c(
    "Occupati",
    "Persone in cerca, con precedenti esperienze, ex-occupati",
    "Persone in cerca, con precedenti esperienze, ex-inattivi",
    "Persone in cerca, senza precedenti esperienze",
    "Inattivi in età lav., cercano non attivamente ma disponibili",
    "Inattivi in età lav., cercano ma non disponibili",
    "Inattivi in età lav., non cercano ma disponibili",
    "Inattivi in età lav., non cercano e non disponibili",
    "Inattivi in età non lav., meno di 15 anni",
    "Inattivi in età non lav., più di 64 anni"
  )

table(RCFL_Microdati_2020_Secondo_trimestre$COND10, useNA = "ifany") # Occupati 34028
## 
##                                                     Occupati 
##                                                      34028 
##Persone in cerca, con precedenti esperienze, ex-occupati 
##1275 
##Persone in cerca, con precedenti esperienze, ex-inattivi 
##652 
##Persone in cerca, senza precedenti esperienze 
##547 
##Inattivi in età lav., cercano non attivamente ma disponibili 
##2564 
##Inattivi in età lav., cercano ma non disponibili 
##559 
##Inattivi in età lav., non cercano ma disponibili 
##1954 
##Inattivi in età lav., non cercano e non disponibili 
##16285 
##Inattivi in età non lav., meno di 15 anni 
##11072 
##Inattivi in età non lav., più di 64 anni 
##32664 


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$COND10, useNA = "ifany")),2)
## 
##                                                     Occupati 
##                                                      33.49 
##Persone in cerca, con precedenti esperienze, ex-occupati 
##1.25 
##Persone in cerca, con precedenti esperienze, ex-inattivi 
##0.64 
##Persone in cerca, senza precedenti esperienze 
##0.54 
##Inattivi in età lav., cercano non attivamente ma disponibili 
##2.52 
##Inattivi in età lav., cercano ma non disponibili 
##0.55 
##Inattivi in età lav., non cercano ma disponibili 
##1.92 
##Inattivi in età lav., non cercano e non disponibili 
##16.03 
##Inattivi in età non lav., meno di 15 anni 
##10.90 
##Inattivi in età non lav., più di 64 anni 
##32.15 


# Considerando solo gli individui in età lavorativa, tabella incrociata COND3 vs COND10
cond_prof_15_64_anni <- RCFL_Microdati_2020_Secondo_trimestre[RCFL_Microdati_2020_Secondo_trimestre$ETAM >= 15 & RCFL_Microdati_2020_Secondo_trimestre$ETAM <= 64,c('COND3','COND10')]

table(cond_prof_15_64_anni$COND10, cond_prof_15_64_anni$COND3)
##                                                               
##                                                                Occupati
##Occupati                                                        32685
##Persone in cerca, con precedenti esperienze, ex-occupati            0
##Persone in cerca, con precedenti esperienze, ex-inattivi            0
##Persone in cerca, senza precedenti esperienze                       0
##Inattivi in età lav., cercano non attivamente ma disponibili        0
##Inattivi in età lav., cercano ma non disponibili                    0
##Inattivi in età lav., non cercano ma disponibili                    0
##Inattivi in età lav., non cercano e non disponibili                 0
##Inattivi in età non lav., meno di 15 anni                           0
##Inattivi in età non lav., più di 64 anni                            0

##Persone in cerca di lavoro
##Occupati                                                                              0
##Persone in cerca, con precedenti esperienze, ex-occupati                           1263
##Persone in cerca, con precedenti esperienze, ex-inattivi                            647
##Persone in cerca, senza precedenti esperienze                                       547
##Inattivi in età lav., cercano non attivamente ma disponibili                          0
##Inattivi in età lav., cercano ma non disponibili                                      0
##Inattivi in età lav., non cercano ma disponibili                                      0
##Inattivi in età lav., non cercano e non disponibili                                   0
##Inattivi in età non lav., meno di 15 anni                                             0
##Inattivi in età non lav., più di 64 anni                                              0

##Inattivi
##Occupati                                                            0
##Persone in cerca, con precedenti esperienze, ex-occupati            0
##Persone in cerca, con precedenti esperienze, ex-inattivi            0
##Persone in cerca, senza precedenti esperienze                       0
##Inattivi in età lav., cercano non attivamente ma disponibili     2564
##Inattivi in età lav., cercano ma non disponibili                  559
##Inattivi in età lav., non cercano ma disponibili                 1954
##Inattivi in età lav., non cercano e non disponibili             16285
##Inattivi in età non lav., meno di 15 anni                           0
##Inattivi in età non lav., più di 64 anni                            0
# ISCO3D  A. Professione
RCFL_Microdati_2020_Secondo_trimestre$ISCO3D <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$ISCO3D)
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$ISCO3D)) # 34028 NA
## [1] 34028

# B1      B. Svolgimento di almeno un'ora di lavoro nella SR
RCFL_Microdati_2020_Secondo_trimestre$B1<- as.factor(RCFL_Microdati_2020_Secondo_trimestre$B1)
levels(RCFL_Microdati_2020_Secondo_trimestre$B1) <- c("Si", "No", "Permanentemente inabile al lavoro","NA")

table(RCFL_Microdati_2020_Secondo_trimestre$B1, useNA = "ifany")
#Si                                No               Permanentemente inabile al lavoro 
#27007                             62177                              1344 
#NA                              <NA> 
#  0                             11072 



round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$B1, useNA = "ifany")),2)
#Si                                No             Permanentemente inabile al lavoro 
#26.58                             61.20                              1.32 
#                                 <NA> 
#                                 10.90 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$B1, useNA = "no")),2)
#Si                                No               Permanentemente inabile al lavoro 
#29.83                             68.68                              1.48 



# B2      B. Ha un lavoro ma non l'ha svolto nella SR   #NOTA:Noi non l'abbiamo importato nei new data
RCFL_Microdati_2020_Secondo_trimestre$B2 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$B2)
levels(RCFL_Microdati_2020_Secondo_trimestre$B2) <- c("Si", "No")

table(RCFL_Microdati_2020_Secondo_trimestre$B2, useNA = "ifany")
#Si       No         <NA> 
#  7231   54946     39423 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$B2, useNA = "ifany")),2)
#Si      No      <NA> 
#7.12   54.08    38.80

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$B2, useNA = "no")),2)
#Si     No 
#11.63  88.37



# B3      B. Motivo per cui non ha lavorato nella SR
RCFL_Microdati_2020_Secondo_trimestre$B3 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$B3)
levels(RCFL_Microdati_2020_Secondo_trimestre$B3) <-
  c(
    "Cassa integrazione guadagni",
    "Ridotta attività dell'impresa",
    "Controversia dil lavoro",
    "Maltempo",
    "Malattia, infortunio",
    "Ferie",
    "Festività nella settimana",
    "Orario variabile o flessibile",
    "Part-time verticale",
    "Studio non riconosciuto",
    "Studio riconosciuto nell'orario lavorativo",
    "Maternità obbligatoria",
    "Maternità facoltativa",
    "Motivi familiari",
    "Mancanza/scarsità di lavoro",
    "Lavoro occasionale",
    "Lavoro stagionale alle dipendenze",
    "Altro"
  )
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$B3, useNA = "ifany")),2)
#             Cassa integrazione guadagni              Ridotta attività dell'impresa 
#                                      2.54                                     0.87 
#                   Controversia dil lavoro                                   Maltempo 
#                                      0.00                                       0.00 
#                      Malattia, infortunio                                      Ferie 
#                                      0.18                                       0.46 
#                 Festività nella settimana              Orario variabile o flessibile 
#                                      0.23                                       0.00 
#                       Part-time verticale                    Studio non riconosciuto 
#                                      0.00                                       0.15 
#Studio riconosciuto nell'orario lavorativo                     Maternità obbligatoria 
#                                     0.06                                       0.06 
#                     Maternità facoltativa                           Motivi familiari 
#                                     0.19                                       0.00 
#               Mancanza/scarsità di lavoro                         Lavoro occasionale 
#                                     0.01                                       0.02 
#         Lavoro stagionale alle dipendenze                                      Altro 
#                                     2.34                                       0.00 
#                                     <NA> 
#                                   92.86 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$B3, useNA = "no")),2)
#Cassa integrazione guadagni              Ridotta attività dell'impresa 
#                                     35.56                                      12.22 
#                   Controversia dil lavoro                                   Maltempo 
#                                      0.01                                       0.03 
#                      Malattia, infortunio                                      Ferie 
#                                      2.55                                       6.49 
#                 Festività nella settimana              Orario variabile o flessibile 
#                                      3.27                                       0.07 
#                       Part-time verticale                    Studio non riconosciuto 
#                                      0.01                                       2.04 
#Studio riconosciuto nell'orario lavorativo                     Maternità obbligatoria 
#                                       0.91                                       0.91 
#                     Maternità facoltativa                           Motivi familiari 
#                                       2.70                                       0.04 
#                 Mancanza/scarsità di lavoro                         Lavoro occasionale 
#                                       0.08                                       0.29 
#           Lavoro stagionale alle dipendenze                                      Altro 
#                                       32.82                                       0.00

#Sezione C - Attività lavorativa principale (per gli occupati)

# C1      C. Tipo di lavoro: dipendente o indipendente
RCFL_Microdati_2020_Secondo_trimestre$C1 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$C1)
levels(RCFL_Microdati_2020_Secondo_trimestre$C1) <-
  c(
    "Lavoro dipendente",
    "Collaborazione coordinata e continuativa",
    "Prestazione d'opera occasionale",
    "Autonomo: imprenditore",
    "Autonomo: libero professionista",
    "Autonomo: lavoratore in proprio",
    "Autonomo: coadiuvante in azienda familiare",
    "Autonomo: socio di cooperativa"
  )

table(RCFL_Microdati_2020_Secondo_trimestre$C1, useNA = "ifany")
#tipologia_lav_svolto
#Lavoro dipendente                 Collaborazione coordinata e continuativa 
#25792                                        162 
#Prestazione d'opera occasionale                     Autonomo: imprenditore #
#                                       96                                        406 
#           Autonomo: libero professionista            Autonomo: lavoratore in proprio 
#                                      2124                                       4772 
#Autonomo: coadiuvante in azienda familiare             Autonomo: socio di cooperativa 
#                                       533                                        143 
#                                      <NA> 
#                                     67572 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C1, useNA = "ifany")),2)
#Lavoro dipendente   Collaborazione coordinata e continuativa 
#25.39                                       0.16 
#Prestazione d'opera occasionale                     Autonomo: imprenditore 
#                                      0.09                                       0.40 
#           Autonomo: libero professionista            Autonomo: lavoratore in proprio 
#                                      2.09                                       4.70 
#Autonomo: coadiuvante in azienda familiare             Autonomo: socio di cooperativa 
#                                      0.52                                       0.14 
#                                      <NA> 
#                                    66.51 


round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C1, useNA = "no")),2)
#Lavoro dipendente   Collaborazione coordinata e continuativa 
#75.80                                       0.48 
#Prestazione d'opera occasionale                     Autonomo: imprenditore 
#                                      0.28                                       1.19 
#           Autonomo: libero professionista            Autonomo: lavoratore in proprio 
#                                      6.24                                      14.02 
#Autonomo: coadiuvante in azienda familiare             Autonomo: socio di cooperativa 
#                                      1.57                                       0.42 


# C9      C. Livello
RCFL_Microdati_2020_Secondo_trimestre$C9 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$C9)
levels(RCFL_Microdati_2020_Secondo_trimestre$C9) <-
  c(
    "Dirigente",
    "Quadro",
    "Impiegato",
    "Operaio",
    "Apprendista",
    "Proprio domicilio"
  )

sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C9)) # 25993 non NA

table(RCFL_Microdati_2020_Secondo_trimestre$C9, useNA = "ifany")
#Dirigente            Quadro         Impiegato           Operaio       Apprendista 
#555              1763             11725             11762               181 
#Proprio domicilio              <NA> 
#  7             75607 

table(RCFL_Microdati_2020_Secondo_trimestre$C9,RCFL_Microdati_2020_Secondo_trimestre$COND3,useNA = "ifany")
#                     1     2     3
#Dirigente           555     0     0
#Quadro             1763     0     0
#Impiegato         11725     0     0
#Operaio           11762     0     0
#Apprendista         181     0     0
#Proprio domicilio     7     0     0
#<NA>               8035  2474 65098

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C9, useNA = "ifany")),2)
#Dirigente            Quadro         Impiegato           Operaio       Apprendista 
#0.55              1.74             11.54               11.58              0.18 
#Proprio domicilio              <NA> 
#  0.01                        74.42  

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C9, useNA = "no")),2)
#Dirigente            Quadro         Impiegato           Operaio       Apprendista 
#2.14                   6.78            45.11             45.25              0.70 
#Proprio domicilio 
#0.03 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C1, RCFL_Microdati_2020_Secondo_trimestre$C9, useNA = "ifany"), margin = 1),2)
#Dirigente Quadro Impiegato Operaio Apprendista
#Lavoro dipendente                               2.13   6.83     45.26   45.06        0.70
#Collaborazione coordinata e continuativa        0.00   0.00      0.00    0.00        0.00
#Prestazione d'opera occasionale                 0.00   0.00      0.00    0.00        0.00
#  Autonomo: imprenditore                          0.00   0.00      0.00    0.00        0.00
#  Autonomo: libero professionista                 0.00   0.00      0.00    0.00        0.00
#  Autonomo: lavoratore in proprio                 0.00   0.00      0.00    0.00        0.00
#  Autonomo: coadiuvante in azienda familiare      0.38   0.00      5.82   12.01        0.19
#  Autonomo: socio di cooperativa                  2.80   1.40     14.69   52.45        0.00
#  <NA>                                            0.00   0.00      0.00    0.00        0.00

# CAT12   C. Attitivà economica 12 classi
RCFL_Microdati_2020_Secondo_trimestre$CAT12 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$CAT12)
levels(RCFL_Microdati_2020_Secondo_trimestre$CAT12) <-
  c(
    "Agricoltura, silvicoltura, pesca",
    "Industria in senso stretto",
    "Costruzioni",
    "Commercio",
    "Alberghi e ristoranti",
    "Trasporto e immagazzinaggio",
    "Servizio di informazione e comunicazione",
    "Attività finanziarie e assicurative",
    "Attivitià immobiliari, servizi alle imprese, alte attività professionali e imprenditoriali",
    "Amministrazione pubblica e difesa",
    "Istruzione, sanità e altri servizi sociali",
    "Altri servizi collettivi e personali"
  )

sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$CAT12)) # 34028 non NA

table(RCFL_Microdati_2020_Secondo_trimestre$CAT12, useNA = "no")
#Agricoltura, silvicoltura, pesca 
#1368 
#Industria in senso stretto 
#6962 
#Costruzioni 
#2024 
#Commercio 
#4596 
#Alberghi e ristoranti 
#1909 
#Trasporto e immagazzinaggio 
#1607 
#Servizio di informazione e comunicazione 
#767 
#Attività finanziarie e assicurative 
#911 
#Attivitià immobiliari, servizi alle imprese, alte attività professionali e imprenditoriali 
#3777 
#Amministrazione pubblica e difesa 
#1980 
#Istruzione, sanità e altri servizi sociali 
#5743 
#Altri servizi collettivi e personali 
#2384

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CAT12, useNA = "ifany")),2)
#Agricoltura, silvicoltura, pesca 
#1.35 
#Industria in senso stretto 
#6.85 
#Costruzioni 
#1.99 
#Commercio 
#4.52 
#Alberghi e ristoranti 
#1.88 
#Trasporto e immagazzinaggio 
#1.58 
#Servizio di informazione e comunicazione 
#0.75 
#Attività finanziarie e assicurative 
#0.90 
#Attivitià immobiliari, servizi alle imprese, alte attività professionali e imprenditoriali 
#3.72 
#Amministrazione pubblica e difesa 
#1.95 
#Istruzione, sanità e altri servizi sociali 
#5.65 
#Altri servizi collettivi e personali 
#2.35 
#<NA> 
#  66.51 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$CAT12, useNA = "no")),2)
#Agricoltura, silvicoltura, pesca 
#4.02 
#Industria in senso stretto 
#20.46 
#Costruzioni 
#5.95 
#Commercio 
#13.51 
#Alberghi e ristoranti 
#5.61 
#Trasporto e immagazzinaggio 
#4.72 
#Servizio di informazione e comunicazione 
#2.25 
#Attività finanziarie e assicurative 
#2.68 
#Attivitià immobiliari, servizi alle imprese, alte attività professionali e imprenditoriali 
#11.10 
#Amministrazione pubblica e difesa 
#5.82 
#Istruzione, sanità e altri servizi sociali 
#16.88 
#Altri servizi collettivi e personali 
#7.01



# C18     C. Numero lavoratori della propria sede
RCFL_Microdati_2020_Secondo_trimestre$C18 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$C18)
levels(RCFL_Microdati_2020_Secondo_trimestre$C18) <-
  c(
    "Fino a 10 persone",
    "Da 11 a 15 persone",
    "Da 16 a 19 persone",
    "Da 20 a 49 persone",
    "Da 50 a 249 persone",
    "250 persone o più",
    "Non sa ma fino a 10 persone",
    "Non sa ma più di 10 persone"
  )

sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C18)) # 28579 non NA

# C20     C. Tipologia di contratto
RCFL_Microdati_2020_Secondo_trimestre$C20 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$C20)
levels(RCFL_Microdati_2020_Secondo_trimestre$C20) <- c("Tempo determinato", "Tempo indeterminato")

sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C20)) # 25993 non NA
round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C20, useNA = "no")),2)
#Tempo determinato Tempo indeterminato 
#13.28               86.72 



# DURATT  C. Durata del lavoro attuale in mesi
summary(RCFL_Microdati_2020_Secondo_trimestre$DURATT)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.0    48.0   144.0   173.9   276.0   876.0   67572 

sd(RCFL_Microdati_2020_Secondo_trimestre$DURATT, na.rm = TRUE)
#143.9966

sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$DURATT)) # 34028 non NA

hist(RCFL_Microdati_2020_Secondo_trimestre$DURATT,
     main = "Distribuzione Occupati per durata lavoro attuale",
     xlab = "Durata (mesi)", ylab = "Frequenza",col = c("#56B4E9"))


# C27     C. Lavoro a tempo parziale
RCFL_Microdati_2020_Secondo_trimestre$C27 <- as.factor(RCFL_Microdati_2020_Secondo_trimestre$C27)
levels(RCFL_Microdati_2020_Secondo_trimestre$C27) <- c("A tempo pieno", "Part-time")

sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C27)) # 34028 non NA

table(RCFL_Microdati_2020_Secondo_trimestre$C27, useNA = "ifany")
#A tempo pieno     Part-time          <NA> 
#  27754          6274                67572

round(100*prop.table(table(C27, useNA = "ifany")),2)
#A tempo pieno     Part-time          <NA> 
#  27.32          6.18                66.51 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$C27, useNA = "no")),2)
#A tempo pieno     Part-time 
#81.56         18.44


# ORELAV    C. Numero di ore effettivamente lavorate a settimana
RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$ORELAV) & (RCFL_Microdati_2020_Secondo_trimestre$ORELAV == 997 | RCFL_Microdati_2020_Secondo_trimestre$ORELAV == 998 | RCFL_Microdati_2020_Secondo_trimestre$ORELAV == 999), 'ORELAV'] <- NA
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$ORELAV)) # 33871 non NA

summary(RCFL_Microdati_2020_Secondo_trimestre$ORELAV)
# Min 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.00   12.00   36.00   32.11   40.00  105.00   67572

sd(RCFL_Microdati_2020_Secondo_trimestre$ORELAV, na.rm = TRUE)
#17.84088



# RETRIC  C. Retribuzione netta del mese scorso
summary(RCFL_Microdati_2020_Secondo_trimestre$RETRIC)
#     Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    NA's 
#    250    1000    1330      1351    1600       3000     75607 

sd(RCFL_Microdati_2020_Secondo_trimestre$RETRIC, na.rm = TRUE)
#536.8678

hist(RCFL_Microdati_2020_Secondo_trimestre$RETRIC,col = c("#56B4E9"))

# il valore 3000 corrisponde a: 3000 o più
sum(RCFL_Microdati_2020_Secondo_trimestre$RETRIC == 3000 & !is.na(RCFL_Microdati_2020_Secondo_trimestre$RETRIC))
#545 casi


# C73     C. Grado di soddisfazione - Lavoro attuale
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C73)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C73) & (RCFL_Microdati_2020_Secondo_trimestre$C73 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C73 == 998), 'C73'] <- NA
sodd_lavoro <- summary(RCFL_Microdati_2020_Secondo_trimestre$C73)


# C74     C. Grado di soddisfazione - Guadagno
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C74)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C74) & (RCFL_Microdati_2020_Secondo_trimestre$C74 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C74 == 998), 'C74'] <- NA
sodd_guadagno <- summary(RCFL_Microdati_2020_Secondo_trimestre$C74)


# C75     C. Grado di soddisfazione - Clima e relazioni di lavoro
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C75)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C75) & (RCFL_Microdati_2020_Secondo_trimestre$C75 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C75 == 998), 'C75'] <- NA
sodd_clima <- summary(RCFL_Microdati_2020_Secondo_trimestre$C75)


# C76     C. Grado di soddisfazione - Carriera / Giro d'affari
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C76)) # 34028

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C76) & (RCFL_Microdati_2020_Secondo_trimestre$C76 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C76 == 998), 'C76'] <- NA
sodd_carriera <- summary(RCFL_Microdati_2020_Secondo_trimestre$C76)


# C77     C. Grado di soddisfazione - Numero di ore lavorate 
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C77)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C77) & (RCFL_Microdati_2020_Secondo_trimestre$C77 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C77 == 998), 'C77'] <- NA
sodd_ore_lav <- summary(RCFL_Microdati_2020_Secondo_trimestre$C77)


# C78     C. Grado di soddisfazione - Stabilità del lavoro
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C78)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C78) & (RCFL_Microdati_2020_Secondo_trimestre$C78 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C78 == 998), 'C78'] <- NA
sodd_stabilita <-summary(RCFL_Microdati_2020_Secondo_trimestre$C78)


# C81A     C. Grado di soddisfazione - Lavoro interessante
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C81A)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C81A) & (RCFL_Microdati_2020_Secondo_trimestre$C81A == 997 | RCFL_Microdati_2020_Secondo_trimestre$C81A == 998), 'C81A'] <- NA
sodd_interesse <- summary(RCFL_Microdati_2020_Secondo_trimestre$C81A)


# C83     C. Ritiene facile trovare un lavoro simile
sum(!is.na(RCFL_Microdati_2020_Secondo_trimestre$C83)) # 34028 non NA

RCFL_Microdati_2020_Secondo_trimestre[!is.na(RCFL_Microdati_2020_Secondo_trimestre$C83) & (RCFL_Microdati_2020_Secondo_trimestre$C83 == 997 | RCFL_Microdati_2020_Secondo_trimestre$C83 == 998), 'C83'] <- NA
facilita_nuovo_lav <- summary(RCFL_Microdati_2020_Secondo_trimestre$C83)

livello_soddisfazione <- t(cbind(sodd_lavoro,
                                 sodd_guadagno,
                                 sodd_clima,
                                 sodd_carriera,
                                 sodd_ore_lav,
                                 sodd_stabilita,
                                 sodd_interesse,
                                 facilita_nuovo_lav))

livello_soddisfazione


#variabile D1
RCFL_Microdati_2020_Secondo_trimestre$D1<- as.factor(RCFL_Microdati_2020_Secondo_trimestre$D1)
levels(RCFL_Microdati_2020_Secondo_trimestre$D1) <- c("Si,uno", "Si,più di uno","No")

table(RCFL_Microdati_2020_Secondo_trimestre$D1, useNA = "ifany")
#Si,uno    Si,più di uno            No            <NA> 
#  475            10               33543         67572 

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$D1, useNA = "ifany")),2)
# Si,uno      Si,più di uno        No           <NA> 
#  0.47          0.01             33.01         66.51

round(100*prop.table(table(RCFL_Microdati_2020_Secondo_trimestre$D1, useNA = "no")),2)
#Si,uno   Si,più di uno            No 
#1.40          0.03               98.57 



#Tasso di attività, occupazione, inattività, disoccupazione

# Salvo una copia del dataset originale per poter applicare nuovi filtri
ds_completo <- ds 

# Nuovo dataset: individui in età da lavoro #### 
ds <- ds[ds$ETAM >= 15 & ds$ETAM <= 64,]
# 53382

round(100*prop.table(table(ds$COND3)),2)
## 
##                   Occupati Persone in cerca di lavoro 
##                      57.95                       6.17 
##                   Inattivi 
##                      35.88
# Forza di lavoro = Occupati + In cerca di lavoro
# Forza di lavoro vs Inattivi

# Tasso di attività: rapporto tra le persone appartenenti alle forze di lavoro 
# e la corrispondente popolazione di riferimento. .
tasso_attivita_campione = round(100*sum(ds$COND3=='Occupati' | ds$COND3=='Persone in cerca di lavoro')/nrow(ds),2)
# 64,12%

i1 <- binom.test(x = sum(ds$COND3=='Occupati' | ds$COND3=='Persone in cerca di lavoro'), n = nrow(ds), p = 0.5,
                 alternative = 'two.sided', conf.level = 0.95)
i1
## 
##  Exact binomial test
## 
## data:  sum(ds$COND3 == "Occupati" | ds$COND3 == "Persone in cerca di lavoro") and nrow(ds)
## number of successes = 34227, number of trials = 53382, p-value <
## 2.2e-16
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.6370853 0.6452417
## sample estimates:
## probability of success 
##              0.6411712
# 95 percent confidence interval: 0.6370853 0.6452417

# Tasso di occupazione: rapporto tra gli occupati e la corrispondente popolazione di riferimento.
tasso_occup_campione = round(100*sum(ds$COND3=='Occupati')/nrow(ds),2)
# 57,95%

i2 <- binom.test(x = sum(ds$COND3=='Occupati'), n = nrow(ds), p = 0.5,
                 alternative = 'two.sided', conf.level = 0.95)
i2
## 
##  Exact binomial test
## 
## data:  sum(ds$COND3 == "Occupati") and nrow(ds)
## number of successes = 30935, number of trials = 53382, p-value <
## 2.2e-16
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.5753013 0.5836950
## sample estimates:
## probability of success 
##              0.5795025
# 95 percent confidence interval: 0.5753013 0.5836950

# Tasso di disoccupazione: rapporto tra le persone in cerca di occupazione e 
# le corrispondenti forze di lavoro.
tasso_disoccup_campione = round(100*sum(ds$COND3=='Persone in cerca di lavoro')/sum(ds$COND3=='Occupati' | ds$COND3=='Persone in cerca di lavoro'),2)
# 9,62%

i3 <- binom.test(x = sum(ds$COND3=='Persone in cerca di lavoro'), n = sum(ds$COND3=='Occupati' | ds$COND3=='Persone in cerca di lavoro'), p = 0.5,
                 alternative = 'two.sided', conf.level = 0.95)
i3
## 
##  Exact binomial test
## 
## data:  sum(ds$COND3 == "Persone in cerca di lavoro") and sum(ds$COND3 == "Occupati" | ds$COND3 == "Persone in cerca di lavoro")
## number of successes = 3292, number of trials = 34227, p-value <
## 2.2e-16
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.09307757 0.09935390
## sample estimates:
## probability of success 
##             0.09618138
# 95 percent confidence interval: 0.09307757 0.09935390

# Tasso di inattività: rapporto tra le persone non appartenenti alle forze di lavoro e la
# corrispondente popolazione di riferimento. 
tasso_inattivita_campione = round(100*sum(ds$COND3=='Inattivi')/nrow(ds),2)
# 35,88%

i4 <- binom.test(x = sum(ds$COND3=='Inattivi'), n = nrow(ds), p = 0.5,
                 alternative = 'two.sided', conf.level = 0.95)
i4
## 
##  Exact binomial test
## 
## data:  sum(ds$COND3 == "Inattivi") and nrow(ds)
## number of successes = 19155, number of trials = 53382, p-value <
## 2.2e-16
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.3547583 0.3629147
## sample estimates:
## probability of success 
##              0.3588288
# 95 percent confidence interval: 0.3547583 0.3629147

# Confronto tra i tassi tra Uomini vs Donne
# Ci sono differenze significative tra i vari tassi calcolati tra uomini e donne? E tra nord vs Centro+Sud? 

# Tasso di attività: uomini vs donne
i5<- prop.test(x = c(sum((ds$COND3=='Occupati' | ds$COND3=='Persone in cerca di lavoro') & ds$SG11 == 'Maschio'), 
                     sum((ds$COND3=='Occupati' | ds$COND3=='Persone in cerca di lavoro') & ds$SG11 == 'Femmina')), 
               n = c(sum(ds$SG11 == 'Maschio'), 
                     sum(ds$SG11 == 'Femmina')))
i5
## 
##  2-sample test for equality of proportions with continuity
##  correction
## 
## data:  c(sum((ds$COND3 == "Occupati" | ds$COND3 == "Persone in cerca di lavoro") &  out of c(sum(ds$SG11 == "Maschio"), sum(ds$SG11 == "Femmina"))    ds$SG11 == "Maschio"), sum((ds$COND3 == "Occupati" | ds$COND3 ==  out of c(sum(ds$SG11 == "Maschio"), sum(ds$SG11 == "Femmina"))    "Persone in cerca di lavoro") & ds$SG11 == "Femmina")) out of c(sum(ds$SG11 == "Maschio"), sum(ds$SG11 == "Femmina"))
## X-squared = 1983.6, df = 1, p-value < 2.2e-16
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  0.1770285 0.1930279
## sample estimates:
##    prop 1    prop 2 
## 0.7361774 0.5511492
# 95 percent confidence interval: 0.1770285 0.1930279


# Tasso di occupazione: uomini vs donne
i7 <- prop.test(x = c(sum(ds$COND3=='Occupati' & ds$SG11 == 'Maschio'),
                      sum(ds$COND3=='Occupati' & ds$SG11 == 'Femmina')), 
                n = c(sum(ds$SG11 == 'Maschio'), 
                      sum(ds$SG11 == 'Femmina')))
i7
## 
##  2-sample test for equality of proportions with continuity
##  correction
## 
## data:  c(sum(ds$COND3 == "Occupati" & ds$SG11 == "Maschio"), sum(ds$COND3 ==  out of c(sum(ds$SG11 == "Maschio"), sum(ds$SG11 == "Femmina"))    "Occupati" & ds$SG11 == "Femmina")) out of c(sum(ds$SG11 == "Maschio"), sum(ds$SG11 == "Femmina"))
## X-squared = 1665.8, df = 1, p-value < 2.2e-16
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  0.1662343 0.1827750
## sample estimates:
##    prop 1    prop 2 
## 0.6691052 0.4946005
# 95 percent confidence interval: 0.1662343 0.1827750



#Analisi della retribuzione mensile
# Nuovo dataset - Lavoratori dipendenti #####
ds <- RCFL_Microdati_2020_Secondo_trimestre[RCFL_Microdati_2020_Secondo_trimestre$COND3 =='Occupati' & RCFL_Microdati_2020_Secondo_trimestre$C1 == 'Lavoro dipendente', ]

# Retribuzione mensile ####
# Analisi Univariata ###

sum(!is.na(ds$RETRIC))  # 25792 valori NA

sum(is.na(ds$RETRIC))   # 0 valori NA

sum(ds$RETRIC == 3000)  # 545 valori pari a 3000

summary(ds$RETRIC) # Mediana 1330, Media 1352 (o superiore)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#250    1000    1330    1352    1600    3000 


getmode(ds$RETRIC) # 1500

sd(ds$RETRIC)
#537.2084

par(mfrow=c(1,2))
par(las=1)
par(mar=c(4.7, 4.7, 2.3, 0))
hist(ds$RETRIC,
     main = "Distribuzione retribuzione mese precedente",
     xlab = "Retribuzione netta mese precendente",
     ylab = "Frequenza",
     freq = TRUE, col = "#56B4E9")
minor.tick(nx = 4,ny=5)

library(Hmisc)
boxplot(ds$RETRIC, notch=T, horizontal=T, 
        main="Boxplot variabile target RETRIC",
        outline = TRUE,col = "white")
minor.tick(nx = 5,ny=0)

par(mfrow=c(1,1))
qqnorm(ds$RETRIC);qqline(ds$RETRIC)



# Boxplot bivariati #####

# Boxplot Retribuzione ####
p1 <- ggplot(data = ds, aes(x = SG11, y = RETRIC)) +
  geom_boxplot() +
  # geom_col(aes( x = Species, y = Petal.Width)) +
  ggtitle("Confronto reddito lavoratori dipendenti per Genere") +
  xlab("Genere") +
  ylab("Retrib. netta mese precedente") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p1


# Medie condizionate per genere
aggregate(ds$RETRIC, by=list(ds$SG11), mean)    # medie condizionate
#        Group.1     x
#1       Maschio 1476.336
#2       Femmina 1214.164

aggregate(ds$RETRIC, by=list(ds$SG11), median)  # mediane condizionate
#      Group.1    x
#1       Maschi 1400
#2       Femmina 1200

aggregate(ds$RETRIC, by=list(ds$SG11), var)     # varianze condizionate
#    Group.1        x
#1       Maschio 284112.2
#2       Femmina 257353.3

aggregate(ds$RETRIC, by=list(ds$SG11), sd)      # SD condizionate
#       Group.1        x
#1       Maschio 533.0218
#2       Femmina 507.3000

#RIP5
levels(ds$RIP5) <- c("Nord Ovest", "Nord Est", "Centro","Sud","Isole")
ds$RIP5 <- as.factor(ds$RIP5)
p2 <- ggplot(data = ds, aes(x = RIP5, y = RETRIC)) +
  geom_boxplot() +
  # geom_col(aes( x = Species, y = Petal.Width)) +
  ggtitle("Confronto reddito lavoratori dipendenti per Area Geografica") +
  xlab("Area Geografica") +
  ylab("Retrib. netta mese precedente")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p2

# Medie condizionate per area geografica
aggregate(ds$RETRIC, by=list(ds$RIP5), mean)    # medie condizionate
#       Group.1          x
#1       Nord         1402.183
#2       Centro       1301.749
#3       Mezzogiorno  1274.883

aggregate(ds$RETRIC, by=list(ds$RIP5), median)  # mediane condizionate
#     Group.1    x
#1       1      1400
#2       2      1300
#3       3      1300

aggregate(ds$RETRIC, by=list(ds$RIP5), var)     # varianze condizionate
#     Group.1        x
#1       1      298997.2
#2       2      269626.9
#3       3      266037.8

aggregate(ds$RETRIC, by=list(ds$RIP5), sd)      # SD condizionate
#       Group.1   x
#1       Nord 546.8064
#2       Centro 519.2561
#3       Sud 515.7885

p3 <- ggplot(data = ds, aes(x = CLETAD, y = RETRIC)) +
  geom_boxplot() +
  # geom_col(aes( x = Species, y = Petal.Width)) +
  ggtitle("Confronto reddito lavoratori dipendenti per Classe di età") +
  xlab("Classe di età") +
  ylab("Retrib. netta mese precedente")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

# Medie condizionate per classe di età
aggregate(ds$RETRIC, by=list(ds$CLETAD), mean)    # medie condizionate
#Group.1        x
#1       2 1010.707
#2       3 1209.114
#3       4 1340.344
#4       5 1406.132
#5       6 1455.768
#6       7 1366.304
#7       8 1411.818

aggregate(ds$RETRIC, by=list(ds$CLETAD), var)     # varianze condizionate
#   Group.1        x
#1       2 140159.9
#2       3 201960.3
#3       4 271027.9
#4       5 296820.6
#5       6 321321.2
#6       7 410108.1
#7       8 513196.

aggregate(ds$RETRIC, by=list(ds$CLETAD), sd)      # SD condizionate
#Group.1        x
#1       2 374.3793
#2       3 449.4000
#3       4 520.6034
#4       5 544.8125
#5       6 566.8520
#6       7 640.3968
#7       8 716.3772

p4 <- ggplot(data = ds, aes(x = EDULEV, y = RETRIC)) +
  geom_boxplot() +
  # geom_col(aes( x = Species, y = Petal.Width)) +
  ggtitle("Confronto reddito lavoratori dipendenti per Titolo di studio") +
  xlab("Titolo di studio") +
  ylab("Retrib. netta mese precedente")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p4

# Medie condizionate per classe di titolo di studio
aggregate(ds$RETRIC, by=list(ds$EDULEV), mean)    # medie condizionate
#       Group.1                    x
#1       Nessun titolo        993.1579
#2       licenza elementare   1024.8077
#3       licenza media        1175.9803
#4       Diploma 2-3          1257.1140
#5       Diploma 3-5          1338.6485
#6       Laurea               1640.9968

aggregate(ds$RETRIC, by=list(ds$EDULEV), median)  # mediane condizionate
#   Group.1    x
#1       1  900
#2       2 1055
#3       3 1200
#4       4 1290
#5       5 1330
#6       6 1560

aggregate(ds$RETRIC, by=list(ds$EDULEV), var)     # varianze condizionate
#Group.1        x
#1       1 162394.2
#2       2 192417.3
#3       3 198359.2
#4       4 193310.1
#5       5 249634.5
#6       6 369420.4

aggregate(ds$RETRIC, by=list(ds$EDULEV), sd)      # SD condizionate
#Group.1        x
#1       1 402.9816
#2       2 438.6540
#3       3 445.3754
#4       4 439.6704
#5       5 499.6343
#6       6 607.7997

p5 <- ggplot(data = ds, aes(x = C9, y = RETRIC)) +
  geom_boxplot() +
  # geom_col(aes( x = Species, y = Petal.Width)) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5)) +
  ggtitle("Confronto reddito lavoratori dipendenti per posizione") +
  xlab("Posizione nella professione") +
  ylab("Retrib. netta mese precedente")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p5

# Medie condizionate per posizione nella professione
aggregate(ds$RETRIC, by=list(ds$C9), mean)    # medie condizionate
#Group.1         x
#1         Dirigente 2648.2514
#2            Quadro 1988.4725
#3         Impiegato 1408.2986
#4           Operaio 1144.9514
#5       Apprendista  980.6111
#6 Proprio domicilio  735.0000

aggregate(ds$RETRIC, by=list(ds$C9), median)  # mediane condizionate
#Group.1    x
#1         Dirigente 3000
#2            Quadro 1900
#3         Impiegato 1400
#4           Operaio 1200
#5       Apprendista  990
#6 Proprio domicilio  850

aggregate(ds$RETRIC, by=list(ds$C9), var)     # varianze condizionate
#Group.1         x
#1         Dirigente 283525.40
#2            Quadro 299104.43
#3         Impiegato 204907.97
#4           Operaio 185269.38
#5       Apprendista  99645.99
#6 Proprio domicilio  72950.00

aggregate(ds$RETRIC, by=list(ds$C9), sd)      # SD condizionate
#Group.1        x
#1         Dirigente 532.4710
#2            Quadro 546.9044
#3         Impiegato 452.6676
#4           Operaio 430.4293
#5       Apprendista 315.6675
#6 Proprio domicilio 270.0926

library(gridExtra)
grid.arrange(p1, p5, p3, p4, nrow = 2, ncol = 2)




# Analisi di associazione tra le variabili categoriche #####

# Chi Square Test
# H0: The The two variables are independent
# H1: The two variables are related
# Significance level = 0.05

par(mfrow=c(1,1))

# EDULEV vs SG11
chisq1 <- chisq.test(ds$EDULEV, ds$SG11, correct = FALSE)
chisq1
#Pearson's Chi-squared test
#
#data:  ds$EDULEV and ds$SG11
#X-squared = 680.47, df = 5, p-value < 2.2e-16

# Dipendenza!
contrib1 <- 100*chisq1$residuals^2/chisq1$statistic
round(contrib1, 3)
#         ds$SG11
#ds$EDULEV              1      2
#Nessun tit             0.414  0.462
#Licenza elementare     1.449  1.618
#Licenza media          18.815 21.014
#Diploma 2-3            0.970  1.084
#Diploma 4-5            0.204  0.228
#Laurea                 25.387 28.354

# Il contributo maggiore lo hanno le modalità Licenzia media e Laurea

# EDULEV vs RIP5
chisq2 <- chisq.test(ds$EDULEV, ds$RIP5, correct = FALSE)
chisq2
#Pearson's Chi-squared test

#Pearson's Chi-squared test

#data:  ds$EDULEV and ds$RIP5
#X-squared = 665.48, df = 20, p-value < 2.2e-16

# Dipendenza!
contrib2 <- 100*chisq2$residuals^2/chisq2$statistic
round(contrib2, 3)
#ds$RIP5
#ds$EDULEV            Nord Ovest Nord Est Centro    Sud  Isole
#Nessun titolo           0.496    1.370  0.504  0.237  2.850
#Licenza elementare      0.260    2.155  0.001  2.199  2.610
#Licenza media           1.907    0.595  0.747  0.171  0.315
#Diploma 2-3             6.339   26.942 10.191 16.474 11.696
#Diploma 4-5             1.113    1.993  3.147  1.927  0.087
#Laurea                  1.840    0.025  0.145  0.314  1.351

# In questo caso, il contributo maggiore è della modalità Diploma 2-3


# EDULEV vs C9
ds$EDULEV <- as.ordered(ds$EDULEV)
levels(ds$EDULEV) <- c(
  "Nessun titolo",
  "Licenza elementare",
  "Licenza media",
  "Diploma 2-3",
  "Diploma 4-5",
  "Laurea"
)

chisq3 <- chisq.test(ds$EDULEV, ds$C9, correct = FALSE)
chisq3
#Pearson's Chi-squared test
#
#data:  ds$EDULEV and ds$C9
#X-squared = 10686, df = 25, p-value < 2.2e-16

# Dipendenza!
contrib3 <- 100*chisq3$residuals^2/chisq3$statistic
round(contrib3, 3)
#ds$EDULEV        Dirigente Quadro Impiegato Operaio Apprendista Proprio domicilio
#Nessun titolo     0.005    0.061     0.282   0.427       0.006             0.000
#Licenza elementare 0.083  0.247     1.441   2.171       0.012             0.001
#Licenza media     1.192  3.965    11.915  20.177       0.033             0.011
#Diploma 2-3      0.309  1.029     0.980   2.189       0.058             0.005
#Diploma 4-5      0.761  0.595     3.429   1.970       0.109             0.022
#Laurea            7.447 15.279     5.009  18.645       0.116             0.017

# In questo caso, il contributo maggiore è della modalità Licenzia Media e Laurea


chisq4 <- chisq.test(ds$LAVSPE, ds$RIP5, correct = FALSE)
chisq4
#Pearson's Chi-squared test
#
#Pearson's Chi-squared test
#
#data:  ds$LAVSPE and ds$RIP5
#X-squared = 1034.2, df = 16, p-value < 2.2e-16

# Dipendenza
contrib4 <- 100*chisq4$residuals^2/chisq4$statistic
round(contrib4, 3)
#                                ds$RIP5
#ds$LAVSPE                         Nord Ovest Nord Est Centro    Sud  Isole
#  Comune di residenza                 15.067    0.679  2.339  6.436 12.509
#  Altro comune, stessa provincia       5.055    4.385  2.356  5.099  7.371
#  Altra provincia, stessa regione     11.620    3.413  0.074  4.310  2.348
#  Altra regione                        0.051    0.605  0.072  6.047  1.192
#  Estero                               5.400    0.156  1.782  1.623  0.009

# C9 vs SG11
chisq5 <- chisq.test(ds$C9, ds$SG11, correct = FALSE)
chisq5

#Pearson's Chi-squared test
#
#data:  ds$C9 and ds$SG11
#X-squared = 1309.6, df = 5, p-value < 2.2e-16


# Dipendenza!
contrib5 <- 100*chisq5$residuals^2/chisq5$statistic
round(contrib5, 3)
#ds$SG11
#ds$C9               Maschio Femmina
#Dirigente           1.268   1.416
#Quadro              0.014   0.015
#Impiegato          24.431  27.287
#Operaio            21.357  23.853
#Apprendista         0.137   0.152
#Proprio domicilio   0.033   0.037

# In questo caso, il contributo maggiore è della modalità Licenzia Media e Laurea



# Anova a una via ####
reg1 = lm(RETRIC ~ SG11, data=ds)
summary(reg1)
#Call:
#  lm(formula = RETRIC ~ SG11, data = ds)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1226.34  -314.16   -14.16   285.84  1785.84 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1476.336      4.466  330.54   <2e-16 ***
#  SG11Femmina -262.172      6.498  -40.34   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 521 on 25790 degrees of freedom
#Multiple R-squared:  0.05936,	Adjusted R-squared:  0.05933 
#F-statistic:  1628 on 1 and 25790 DF,  p-value: < 2.2e-16

anova(reg1) 
#Analysis of Variance Table
#
#Response: RETRIC
#Df     Sum Sq   Mean Sq F value    Pr(>F)    
#SG11          1  441847956 441847956  1627.6 < 2.2e-16 ***
#  Residuals 25790 7001249990    271472                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Check omogeneity of variances
plot(reg1, 1)


# Check normality assumption
plot(reg1, 2)


reg2 = lm(RETRIC ~ RIP5, data=ds)
summary(reg2)

#Call:
#  lm(formula = RETRIC ~ RIP5, data = ds)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1156.5  -306.5    -6.5   277.9  1727.9 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1399.096      5.810 240.806   <2e-16 ***
#  RIP5Nord Est    7.409      9.002   0.823     0.41    
#RIP5Centro    -97.348      9.090 -10.709   <2e-16 ***
#  RIP5Sud      -126.953     10.591 -11.986   <2e-16 ***
#  RIP5Isole    -118.745     13.795  -8.608   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 534.2 on 25787 degrees of freedom
#Multiple R-squared:  0.01128,	Adjusted R-squared:  0.01113 
#F-statistic: 73.58 on 4 and 25787 DF,  p-value: < 2.2e-16

anova(reg2) 
#Analysis of Variance Table
#Response: RETRIC
#Df     Sum Sq  Mean Sq F value    Pr(>F)    
#RIP5          4   83989294 20997323  73.577 < 2.2e-16 ***
#  Residuals 25787 7359108652   285381                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Check omogeneity of variances


plot(reg2, 1)

# Check normality assumption
plot(reg2, 2)



reg4 = lm(RETRIC ~ C9, data=ds)
summary(reg4)
#Call:
#  lm(formula = RETRIC ~ C9, data = ds)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2328.25  -284.95    25.05   271.70  1855.05 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          2648.25      19.25  137.58   <2e-16 ***
#  C9Quadro             -659.78      22.05  -29.93   <2e-16 ***
#  C9Impiegato         -1239.95      19.70  -62.96   <2e-16 ***
#  C9Operaio           -1503.30      19.70  -76.32   <2e-16 ***
#  C9Apprendista       -1667.64      38.74  -43.05   <2e-16 ***
# C9Proprio domicilio -1913.25     185.12  -10.34   <2e-16 ***
#  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 451 on 25786 degrees of freedom
#Multiple R-squared:  0.2953,	Adjusted R-squared:  0.2952 
#F-statistic:  2161 on 5 and 25786 DF,  p-value: < 2.2e-16

anova(reg4)
#Analysis of Variance Table
#
#Response: RETRIC
#Df     Sum Sq   Mean Sq    F value           Pr(>F)    
#C9     5       2198214292    439642858  2161.5 < 2.2e-16 ***
#Residuals 25786 5244883653    203400                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Check omogeneity of variances
plot(reg4, 1)

# Check normality assumption
plot(reg4, 2)


reg5 = lm(RETRIC ~ EDULEV, data=ds)
summary(reg5)
#Call:
#  lm(formula = RETRIC ~ EDULEV, data = ds)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1391.00  -325.98   -15.98   261.35  1824.02 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1238.451      9.952 124.448  < 2e-16 ***
#  EDULEV.L     509.389     32.718  15.569  < 2e-16 ***
#  EDULEV.Q     117.286     29.316   4.001 6.33e-05 ***
# EDULEV.C      53.500     23.940   2.235   0.0254 *  
#  EDULEV^4      77.478     18.137   4.272 1.95e-05 ***
#  EDULEV^5      -6.931     11.792  -0.588   0.5567    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 507.9 on 25786 degrees of freedom
#Multiple R-squared:  0.1063,	Adjusted R-squared:  0.1061 
#F-statistic: 613.3 on 5 and 25786 DF,  p-value: < 2.2e-16

anova(reg5)
#Analysis of Variance Table
#
#Response: RETRIC
#Df         Sum Sq   Mean Sq      F value     Pr(>F)    
#EDULEV        5      791019375 158203875  613.26 < 2.2e-16 ***
#Residuals 25786 6652078570    257972                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Check omogeneity of variances
plot(reg5, 1)

# Check normality assumption
plot(reg5, 2)

# Analisi della covarianza
cor(ds$RETRIC,ds$ETAM)   # 0.1887217 (too low)

cor(ds$RETRIC,ds$DURATT) # 0.3136425


reg21 = lm(RETRIC ~ C9 + C27 + DURATT, data=ds)
summary(reg21)
#Call:
#  lm(formula = RETRIC ~ C9 + C27 + DURATT, data = ds)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2291.88  -226.87   -15.08   204.86  2271.08 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          3.081e+03  1.801e+01  171.00   <2e-16 ***
#  C9Quadro            -6.430e+02  1.856e+01  -34.64   <2e-16 ***
#  C9Impiegato         -1.122e+03  1.663e+01  -67.46   <2e-16 ***
#  C9Operaio           -1.336e+03  1.670e+01  -80.01   <2e-16 ***
#  C9Apprendista       -1.473e+03  3.282e+01  -44.87   <2e-16 ***
#  C9Proprio domicilio -1.733e+03  1.559e+02  -11.12   <2e-16 ***
# C27                 -5.517e+02  6.017e+00  -91.69   <2e-16 ***
#  DURATT               6.098e-01  1.814e-02   33.62   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 379.7 on 25784 degrees of freedom
#Multiple R-squared:  0.5005,	Adjusted R-squared:  0.5003 
#F-statistic:  3690 on 7 and 25784 DF,  p-value: < 2.2e-16

# Multiple R-squared:  0.5005,  Adjusted R-squared:  0.5003
# F-statistic:  3690 on 7 and 25784 DF,  p-value: < 2.2e-16
anova(reg21)
#Analysis of Variance Table
#
#Response: RETRIC
#             Df     Sum Sq    Mean Sq F value    Pr(>F)    
#C9            5 2198214292  439642858  3048.8 < 2.2e-16 ***
#  C27           1 1363745694 1363745694  9457.2 < 2.2e-16 ***
#  DURATT        1  163038109  163038109  1130.6 < 2.2e-16 ***
#  Residuals 25784 3718099851     144202                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

confint(reg21, level=0.95) # CIs for model parameters
#                       2.5 %        97.5 %
#  (Intercept)          3045.3150374  3115.9357553
#C9Quadro             -679.3890044  -606.6198338
#C9Impiegato         -1154.1882113 -1089.0141482
#C9Operaio           -1368.8647025 -1303.4013659
#C9Apprendista       -1536.8346866 -1408.1822039
#C9Proprio domicilio -2038.8653326 -1427.7044923
#C27Part-Time        -563.4895506  -539.9014505
#DURATT                  0.5742755     0.6453712

vif(reg21)
#         GVIF    Df   GVIF^(1/(2*Df))
#C9     1.076108  5        1.007362
#C27    1.034282  1        1.016996
#DURATT 1.086673  1        1.042436

reg23 = lm(RETRIC ~ SG11 + RIP5 + C9 + C27 + DURATT, data=ds)
summary(reg23)
#Call:
#  lm(formula = RETRIC ~ SG11 + RIP3 + C9 + C27 + DURATT, data = ds)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2405.99  -212.77    -7.83   196.92  2278.98
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          3.108e+03  1.743e+01  178.25   <2e-16 ***
#  SG11Femmina         -1.881e+02  4.973e+00  -37.82   <2e-16 ***
#  RIP3Centro          -8.664e+01  5.666e+00  -15.29   <2e-16 ***
#  RIP3Mezzogiorno     -1.457e+02  5.814e+00  -25.06   <2e-16 ***
# C9Quadro            -6.216e+02  1.787e+01  -34.78   <2e-16 ***
#  C9Impiegato         -1.087e+03  1.603e+01  -67.83   <2e-16 ***
#  C9Operaio           -1.345e+03  1.608e+01  -83.67   <2e-16 ***
#  C9Apprendista       -1.477e+03  3.159e+01  -46.75   <2e-16 ***
#  C9Proprio domicilio -1.700e+03  1.500e+02  -11.33   <2e-16 ***
#  C27                 -4.726e+02  6.147e+00  -76.88   <2e-16 ***
#  DURATT               6.366e-01  1.747e-02   36.44   <2e-16 ***
#  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 365.4 on 25781 degrees of freedom
#Multiple R-squared:  0.5374,	Adjusted R-squared:  0.5372 
#F-statistic:  2995 on 10 and 25781 DF,  p-value: < 2.2e-16

#Multiple R-squared:  0.5374,	Adjusted R-squared:  0.5372 
#F-statistic:  2995 on 10 and 25781 DF,  p-value: < 2.2e-16
anova(reg23)
#Analysis of Variance Table
#
#Response: RETRIC
#             Df     Sum Sq   Mean Sq F value    Pr(>F)    
#SG11          1  441847956 441847956  3308.4 < 2.2e-16 ***
#  RIP3          2   96771717  48385858   362.3 < 2.2e-16 ***
#  C9            5 2372103251 474420650  3552.3 < 2.2e-16 ***
#  C27           1  911877524 911877524  6827.8 < 2.2e-16 ***
#  DURATT        1  177369735 177369735  1328.1 < 2.2e-16 ***
#  Residuals 25781 3443127763    133553                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

confint(reg23, level=0.95) # CIs for model parameters
#                       2.5 %        97.5 %
#  (Intercept)          3073.4468250  3141.7887858
#SG11Femmina          -197.8611077  -178.3651065
#RIP3Centro            -97.7425552   -75.5314681
#RIP3Mezzogiorno      -157.0860603  -134.2960353
#C9Quadro             -656.6344337  -586.5727049
#C9Impiegato         -1118.3252476 -1055.5039366
#C9Operaio           -1376.4596338 -1313.4432860
#C9Apprendista       -1538.6070349 -1414.7881625
#C9Proprio domicilio -1993.7417128 -1405.5380763
#C27                  -484.6504169  -460.5523228
#DURATT                  0.6023479     0.6708246

vif(reg23)
#       GVIF     Df   GVIF^(1/(2*Df))
#SG11   1.190529  1        1.091114
#RIP5   1.008898  4        1.001108
#C9     1.143604  5        1.013509
#C27    1.165563  1        1.079612
#DURATT 1.088466  1        1.043296

