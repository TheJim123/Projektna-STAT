#Nastavite lokacijo na STAT
#setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")
setwd("C:\Users\Jimmy\Desktop\faks\3. letnik\STAT\Projektna-STAT")
#Vzorec "KibergradVzorec1.csv" je bil pridobljen preko vzorčenja v LibreOffice Calc

#Pretvorimo tabelo v obliki csv datoteke v dataframe, s katerimi R operira

vzorec1 <- read.csv("KibergradVzorec1.csv")

#Shranimo si vnose za tipe in za dohodke

Tip <- vzorec1$TIP
Dohodek <- vzorec1$DOHODEK

#Narišemo vzporedne škatle z brki

boxplot(Dohodek~Tip, data = vzorec1, main="Škatle z brki za dohodke družin glede na tip",yaxp = c(0, 250000, 10), xlab = "Tip Družine", ylab = "Dohodek")
