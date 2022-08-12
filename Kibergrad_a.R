#Nastavimo lokacijo na mapo, ki vsebuje nalogo
#setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")
setwd("C:/Users/Jimmy/Desktop/faks/3. letnik/STAT/Projektna-STAT")

#Po potrebi naložimo knjižnjico dplyr z ukazom install.packages("dplyr")

library(dplyr)

#Pretvorimo tabelo v obliki csv datoteke v dataframe, s katerimi R operira

vzorec1 <- read.csv("KibergradVzorec1.csv")

#Shranimo si vnose za tipe in za dohodke

Tip <- vzorec1$TIP
Dohodek <- vzorec1$DOHODEK

#Da preberemo ekstremne vrednosti (maksimum ter minimum) in kvartile lahko
#uporabimo fukncijo summary(). To lahko storimo na celem vzorcu, ali pa samo na
#dohodkih, ki so shranjeni v tabeli Dohodek. Da izpišemo vrednosti po tipih, pred tem
#filtriramo podatke po tipih.

DohTip1 <- filter(vzorec1, TIP == 1)$DOHODEK
DohTip2 <- filter(vzorec1, TIP == 2)$DOHODEK
DohTip3 <- filter(vzorec1, TIP == 3)$DOHODEK

povzetek1 <- summary(DohTip1)
povzetek2 <- summary(DohTip2)
povzetek3 <- summary(DohTip3)

print(povzetek1) 
print(povzetek2)
print(povzetek3)

#Pri tem nam funkcija izpiše tudi povprečje vrednosti (mean)
#Poračunajmo še interkvartilne razmike

IQR1 <- IQR(DohTip1)
IQR2 <- IQR(DohTip2)
IQR3 <- IQR(DohTip3)

print(IQR1)
print(IQR2)
print(IQR3)

#Sedaj narišemo vzporedne škatle z brki

boxplot(Dohodek~Tip, data = vzorec1, main="Škatle z brki za dohodke družin glede na tip",yaxp = c(0, 250000, 10), xlab = "Tip Družine", ylab = "Dohodek")