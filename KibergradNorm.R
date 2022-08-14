#Nastavimo mesto
#setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")
setwd("C:/Users/Jimmy/Desktop/faks/3. letnik/STAT/Projektna-STAT")

#Naložimo datoteko Kibergrad v dataframe

Kibergrad <- read.csv("Kibergrad.csv")

# Posebej nas zanimajo dohodki.

Dohodek <- Kibergrad$DOHODEK

#Naložimo knjižnjico dplyr
install.packages("dplyr")
library(dplyr)

Ena <- filter(Kibergrad, TIP == 1)
Dve <- filter(Kibergrad, TIP == 2)
Tri <- filter(Kibergrad, TIP == 3)

DohEna <- Ena$DOHODEK
DohDve <- Dve$DOHODEK
DohTri <- Tri$DOHODEK

#Narišimo gostote za podatke za vsak tip in primerjajmo histograme s krivuljo
#normalne porazdelitve
par(mfrow=c(3, 3))

plot(density(DohEna), main="Gostota dohodkov družin tipa 1")
polygon(density(DohEna), col="red", border="blue")

plot(density(DohDve), main="Gostota dohodkov družin tipa 2")
polygon(density(DohDve), col="green", border="blue")

plot(density(DohTri), main="Gostota dohodkov družin tipa 3")
polygon(density(DohTri), col="red", border="blue")

h1<-hist(DohEna, breaks=100, col="red", xlab="Dohodki družin tipa 1",
   main="Histogram s krivuljo N porazd.") 
xfit<-seq(min(DohEna),max(DohEna),length=40)
yfit<-dnorm(xfit,mean=mean(DohEna),sd=sd(DohEna))
yfit <- yfit*diff(h1$mids[1:2])*length(DohEna)
lines(xfit, yfit, col="blue", lwd=2) 

h2<-hist(DohDve, breaks=100, col="red", xlab="Dohodki družin tipa 2",
   main="Histogram s krivuljo N porazd.") 
xfit<-seq(min(DohDve),max(DohDve),length=40)
yfit<-dnorm(xfit,mean=mean(DohDve),sd=sd(DohDve))
yfit <- yfit*diff(h2$mids[1:2])*length(DohDve)
lines(xfit, yfit, col="blue", lwd=2) 

h3<-hist(DohTri, breaks=100, col="red", xlab="Dohodki družin tipa 3",
   main="Histogram s krivuljo N porazd.") 
xfit<-seq(min(DohTri),max(DohTri),length=40)
yfit<-dnorm(xfit,mean=mean(DohTri),sd=sd(DohTri))
yfit <- yfit*diff(h3$mids[1:2])*length(DohTri)
lines(xfit, yfit, col="blue", lwd=2) 

#Sumimo, da dohodki niso porazdeljeni normalno. Da se dodatno 
#prepričamo, uporabimo Q-Q test.

qqnorm(DohEna, pch = 1, frame = FALSE, main = "Q-Q grafikon družin tipa 1")
qqline(DohEna, col = "blue", lwd = 2)

qqnorm(DohDve, pch = 1, frame = FALSE, main = "Q-Q grafikon družin tipa 2")
qqline(DohDve, col = "blue", lwd = 2)

qqnorm(DohTri, pch = 1, frame = FALSE, main = "Q-Q grafikon družin tipa 3")
qqline(DohTri, col = "blue", lwd = 2)

#Vidimo, da dohodki res niso normalno porazdeljeni.