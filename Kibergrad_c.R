#Nastavimo mesto, kjer so shranjene datoteke
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Naložimo datoteko Kibergrad v dataframe

Kibergrad <- read.csv("Kibergrad.csv")

# Posebej nas zanimajo dohodki.

Dohodek <- Kibergrad$DOHODEK
#Izračunamo celotno (populacijsko) varianco dohodkov v S. Funkcija var() 
izračuna vzorčno varianco, zato moramo var(Dohodek) pomnožiti z (N-1)/N, kjer
je N število družin. V našem primeru je N = length(Dohodek).

N <- length(Dohodek)

S <- (N-1)/N * var(Dohodek)

# Sedaj se lotimo obravnave s tipi pojasnjene variance
#Naložimo knjižnjico dplyr
install.packages("dplyr")
library(dplyr)

Ena <- filter(Kibergrad, TIP == 1)
Dve <- filter(Kibergrad, TIP == 2)
Tri <- filter(Kibergrad, TIP == 3)

DohEna <- Ena$DOHODEK
DohDve <- Dve$DOHODEK
DohTri <- Tri$DOHODEK

#Sedaj se lotimo računanja pojasnjene in nepojasnjene variance
#Posebej zapišemo koliko podatkov hrani vsak tip.

n1 <- nrow(Ena)
n2 <- nrow(Dve)
n3 <- nrow(Tri)

# Sedaj poračunamo povprečje znotraj vsake skupine in skupno povprečje
p1 <- mean(DohEna)
p2 <- mean(DohDve)
p3 <- mean(DohTri)
p <- mean(Dohodek)

# Sledi izračun pojasnjene variance

pv <- (n1 * (p1 - p)^2 + n2 * (p2 - p)^2 + n3 * (p3 - p)^2) / N

#Uporabimo zvezo Varianca = pojasnjena var. + nepojansnjena var.

nv <- S - pv

S
pv
nv

paste(print("Deleža pojasnjene in nepojasnjene variance sta potem"), print(pv/S),print("in"), print(nv / S))

paste0(print("Standardni odklon dohodka populacije je "), print(sqrt(S)))