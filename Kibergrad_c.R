#Nastavimo mesto
setwd("C:/Users/Jimmy/Desktop/faks/3. letnik/STAT/Projektna-STAT")

#Naložimo datoteko Kibergrad v dataframe

Kibergrad <- read.csv("Kibergrad.csv")

# Posebej nas zanimajo dohodki.

Dohodek <- Kibergrad$DOHODEK
#Izračunamo celotno varianco dohodkov v S

S <- var(Dohodek)

# Sedaj se lotimo obravnave s tipi pojasnjene variance
#Naložimo knjižnjico dplyr
#install.packages("dplyr", "lsr")
library(dplyr)
library(lsr)

Ena <- filter(Kibergrad, TIP == 1)
Dve <- filter(Kibergrad, TIP == 2)
Tri <- filter(Kibergrad, TIP == 3)

#Narišimo gostote za podatke za vsak tip in primerjajmo histograme s krivuljo
#normalne porazdelitve

DohEna <- Ena$DOHODEK
DohDve <- Dve$DOHODEK
DohTri <- Tri$DOHODEK

plot(density(DohEna), main="Gostota dohodkov družin tipa 1")
polygon(density(DohEna), col="red", border="blue")

#h1<-hist(DohEna, breaks=200, col="red", xlab="Dohodki družin tipa 1",
#   main="Histogram s krivuljo normalne porazdelitve") 
#xfit<-seq(min(DohEna),max(DohEna),length=40)
#yfit<-dnorm(xfit,mean=mean(DohEna),sd=sd(DohEna))
#yfit <- yfit*diff(h1$mids[1:2])*length(DohEna)
#lines(xfit, yfit, col="blue", lwd=2) 


plot(density(DohDve), main="Gostota dohodkov družin tipa 2")
polygon(density(DohDve), col="green", border="blue")

#h2<-hist(DohDve, breaks=100, col="red", xlab="Dohodki družin tipa 2",
#   main="Histogram s krivuljo normalne porazdelitve") 
#xfit<-seq(min(DohDve),max(DohDve),length=40)
#yfit<-dnorm(xfit,mean=mean(DohDve),sd=sd(DohDve))
#yfit <- yfit*diff(h2$mids[1:2])*length(DohDve)
#lines(xfit, yfit, col="blue", lwd=2) 


plot(density(DohTri), main="Gostota dohodkov družin tipa 3")
polygon(density(DohTri), col="red", border="blue")

#h3<-hist(DohTri, breaks=100, col="red", xlab="Dohodki družin tipa 3",
#   main="Histogram s krivuljo normalne porazdelitve") 
#xfit<-seq(min(DohTri),max(DohTri),length=40)
#yfit<-dnorm(xfit,mean=mean(DohTri),sd=sd(DohTri))
#yfit <- yfit*diff(h3$mids[1:2])*length(DohTri)
#lines(xfit, yfit, col="blue", lwd=2) 

#Sumimo, da dohodki niso porazdeljeni normalno. Da se dodatno 
#prepričamo, uporabimo Q-Q test.

#qqnorm(DohEna, pch = 1, frame = FALSE)
#qqline(DohEna, col = "blue", lwd = 2)

#qqnorm(DohDve, pch = 1, frame = FALSE)
#qqline(DohDve, col = "blue", lwd = 2)

#qqnorm(DohTri, pch = 1, frame = FALSE)
#qqline(DohTri, col = "blue", lwd = 2)

#Vidimo, da dohodki res niso normalno porazdeljeni.

#Sedaj se lotimo računanja pojasnjene in nepojasnjene variance
#Posebej zapišemo koliko podatkov hrani vsak tip.

n1 <- nrow(Ena)
n2 <- nrow(Dve)
n3 <- nrow(Tri)

# Sedaj poračunamo povprečje znotraj vsake skupine in skupno povprečje
p1 <- mean(Ena$DOHODEK)
p2 <- mean(Dve$DOHODEK)
p3 <- mean(Tri$DOHODEK)
p <- mean(Dohodek)

# Sledi izračun pojasnjene variance

pv <- (n1 * (p1 - p)^2 + n2 * (p2 - p)^2 + n3 * (p3 - p)^2) / (n1 + n2 + n3 - 1)

#Uporabimo zvezo Varianca = pojasnjena var. + nepojansnjena var.

nv <- S - pv

#Pri izračunu mi je pomagala spletna stran https://omr.fnm.um.si/wp-content/uploads/2018/04/4.pdf
S
pv
nv