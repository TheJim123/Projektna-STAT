#Za delovanje skripte sta potrebna paketa "dplyr" in "ggpubr"
install.packages("dplyr")
install.packages("ggpubr")

library("dplyr")
library("ggpubr")

#Nastavimo lokacijo na mapo, v kateri je shranjena naloga
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Vzorci "KibergradVzorec1.csv", "KibergradVzorec2.csv", ... "KibergradVzorec5.csv" 
#so bili pridobljeni preko vzorčenja v LibreOffice Calc

#Pretvorimo tabelo v obliki csv datoteke v dataframe, s katerimi R operira. 

vzorec1 <- read.csv("KibergradVzorec1.csv")
vzorec2 <- read.csv("KibergradVzorec2.csv")
vzorec3 <- read.csv("KibergradVzorec3.csv")
vzorec4 <- read.csv("KibergradVzorec4.csv")
vzorec5 <- read.csv("KibergradVzorec5.csv")

# Vsaki družini sedaj dodamo še en podatek - kateremu vzorcu pripada. 

vzorec1$VZOREC <- rep(1, 500)
vzorec2$VZOREC <- rep(2, 500)
vzorec3$VZOREC <- rep(3, 500)
vzorec4$VZOREC <- rep(4, 500)
vzorec5$VZOREC <- rep(5, 500)

# Razširjene tabele nato sestavimo v eno samo in izluščimo družine tipa 1

VzorciSkupaj <- rbind(vzorec1, rbind(vzorec2, rbind(vzorec3, rbind(vzorec4, vzorec5))))
Podatki <- filter(VzorciSkupaj, TIP==1)

LepiPodatki <- filter(Podatki, DOHODEK <= 250000)

Dohodek <- Podatki$DOHODEK
Vzorec <- Podatki$VZOREC

LepDohodek <- LepiPodatki$DOHODEK
LepVzorec <- LepiPodatki$VZOREC

#boxplot(Dohodek~Vzorec, data = Podatki, main="Škatle z brki za dohodke družin tipa 1 glede na vzorec",yaxp = c(0, 500000, 25), xlab = "Vzorec", ylab = "Dohodek")
boxplot(LepDohodek~LepVzorec, data = LepiPodatki, main="Škatle z brki za dohodke družin tipa 1 glede na vzorec",yaxp = c(0, 250000, 25), xlab = "Vzorec", ylab = "Dohodek")

summary(filter(vzorec1, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
IQR(filter(vzorec1, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
sd(filter(vzorec1, TIP == 1 & DOHODEK <= 250000)$DOHODEK)

summary(filter(vzorec2, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
IQR(filter(vzorec2, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
sd(filter(vzorec2, TIP == 1 & DOHODEK <= 250000)$DOHODEK)

summary(filter(vzorec3, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
IQR(filter(vzorec3, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
sd(filter(vzorec3, TIP == 1 & DOHODEK <= 250000)$DOHODEK)

summary(filter(vzorec4, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
IQR(filter(vzorec4, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
sd(filter(vzorec4, TIP == 1 & DOHODEK <= 250000)$DOHODEK)

summary(filter(vzorec5, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
IQR(filter(vzorec5, TIP == 1 & DOHODEK <= 250000)$DOHODEK)
sd(filter(vzorec5, TIP == 1 & DOHODEK <= 250000)$DOHODEK)

#Preverimo, ali je dohodek porazdeljen normalno
#ggqqplot(filter(vzorec1, TIP == 1 & DOHODEK <= 250000)$DOHODEK, main = "Q-Q graf za dohodke družin tipa 1 v vzorcu 1")