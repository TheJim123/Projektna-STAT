#Nastavimo na lokacijo v računalniku, ki vsebuje vse datoteke projektne naloge
setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Če še nismo, naložimo paket tidyverse
#install.packages("tidyverse")

#Naložimo podatke

TempLJ <- read.csv(Temp_LJ.csv)

Mesec <- TempLJ$MESEC
Temp <- TempLJ$TEMPERATURA

#Za občutek narišemo razpršeni graf temperatur glede na mesec

plot(Mesec, temp, xaxp = c(0, 12, 12))
