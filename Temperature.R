#Nastavimo na lokacijo v računalniku, ki vsebuje vse datoteke projektne naloge
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Če še nismo, naložimo paket tidyverse
#install.packages("tidyverse")

#Naložimo podatke

TempLJ <- read.csv("Temp_LJ.csv")

Mesec <- TempLJ$MESEC
Temp <- TempLJ$TEMPERATURA

#Za občutek narišemo razpršeni graf temperatur glede na mesec

plot(Mesec, Temp, xaxp = c(0, 12, 12))

#Sestavimo matriko Xa za model A

Xa <- matrix(rep(1, 4*420), nrow = 420, ncol = 4)

for(i in 1:420){
	Xa[i, 2] <- i
	Xa[i, 3] <- sin(i*pi / 6)
	Xa[i, 4] <- cos(i*pi / 6)
}

#Izračunamo cenilko po MNK za vektor parametrov modela A
Ba <- lsfit(Xa, Temp, intercept = FALSE)

Betaa <- Ba$coefficients

#Sestavimo matriko Xb za model B

Xb <- matrix(rep(0, 13*420), nrow = 420, ncol = 13)

for(i in 1:420){
	Xb[i, 13] <- i
	for(j in 1:11){
		if(i %% 12 == 0){
			Xb[i, 12] <- 1
		}
		else if(i %% 12 == j){
			Xb[i, j] <- 1
		}
	}
	
}

#Izračunamo cenilko po MNK za vektor parametrov modela B
Bb <- lsfit(Xb, Temp, intercept = FALSE)

Betab<- Bb$coefficients

#Računamo reziduale za oba modela

resa <- Temp - Xa%*%Betaa

RSSa <- norm(resa, type="2")

resb <- Temp - Xb%*%Betab

RSSb <- norm(resb, type="2")

#Poračunamo kvocient F

n <- length(Temp)
p <- length(Betab)
q <- length(Betaa)

Fs <- ((RSSa - RSSb)/(p - q)) / (RSSb / (n - p))

t1 <- qf(0.99, df1 = p-q, df2 = n-p)
t2 <- qf(0.95, df1 = p-q, df2 = n-p)

#Izračun Akaikejeve informacije

#Za model A

AICa <- 2*q + n*log(RSSa)

#Za model B

AICb <- 2*p + n*log(RSSb)

#Narišemo modela

modelA <- function(x){
	y <- Betaa[[1]] + Betaa[[2]]*x + Betaa[[3]]*sin(x *pi/6) + Betaa[[4]]*cos(x*pi/6)
}

modelB <- function(x) {
	m <- rep(0, length(x))
	for(i in 1:length(x)){
		if(x[i]%%12 != 0){
			j <- x[i]%%12
			m[i] <- Betab[[j]]
		}
		else {
			m[i] <- Betab[[12]]
		}
	}
	y <- m + Betab[[13]]*x
}

num <- seq(1, 240, 1)

par(mfrow=c(2, 1))

plot(num, modelA(num), main="Model A v primerjavi z meritvami", type ="l", col="red", xlab="Mesec", ylab= "Temperatura [°C]", xaxp = c(0, 240, 10))
points(Temp, col="blue")

plot(num, modelB(num), main="Model B v primerjavi z meritvami", type ="l", col="red", xlab="Mesec", ylab= "Temperatura [°C]", xaxp = c(0, 240, 10))
points(Temp, col="blue")


paste(c("Cenilka za vektor parametrov modela A je:", Betaa))
paste(c("RSS modela A je:", RSSa))
paste(c("Cenilka za vektor parametrov modela B je:", Betab))
paste(c("RSS modela B je:", RSSb))
paste(c("Kvocient F za preizkus znaša:", Fs))
paste(c("Testna vrednost inverza kumulativne funkcije F-distribucije za alfa = 0.01:", t1))
paste(c("Testna vrednost inverza kumulativne funkcije F-distribucije za alfa = 0.05:", t2))
paste(c("AIC modela A je:", AICa))
paste(c("AIC modela B je:", AICb))
