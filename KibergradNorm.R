#Nastavimo mesto
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Naložimo datoteko Kibergrad v dataframe

Kibergrad <- read.csv("Kibergrad.csv")

#Posebej nas zanimajo dohodki.

Dohodek <- Kibergrad$DOHODEK

#Naložimo knjižnjico dplyr
#install.packages("dplyr")
library(dplyr)

Ena <- filter(Kibergrad, TIP == 1)
Dve <- filter(Kibergrad, TIP == 2)
Tri <- filter(Kibergrad, TIP == 3)

DohEna <- Ena$DOHODEK
DohDve <- Dve$DOHODEK
DohTri <- Tri$DOHODEK

#Sumimo, da dohodki niso porazdeljeni normalno. Da se dodatno 
#prepričamo, uporabimo Q-Q test.

par(mfrow=c(4, 1))

qqnorm(DohEna, pch = 1, frame = FALSE, main = "Q-Q grafikon družin tipa 1")
qqline(DohEna, col = "blue", lwd = 2)

qqnorm(DohDve, pch = 1, frame = FALSE, main = "Q-Q grafikon družin tipa 2")
qqline(DohDve, col = "blue", lwd = 2)

qqnorm(DohTri, pch = 1, frame = FALSE, main = "Q-Q grafikon družin tipa 3")
qqline(DohTri, col = "blue", lwd = 2)

qqnorm(Dohodek, pch = 1, frame = FALSE, main = "Q-Q populacijskega dohodka")
qqline(Dohodek, col = "blue", lwd = 2)


#Vidimo, da dohodki res niso normalno porazdeljeni.