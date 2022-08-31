#Nastavimo mesto datoteke (Po potrebi spremenite na mesto, kamor ste shranili datoteke)
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Naložimo podatke eksperimentov
kratki <- read.csv("Kromatin_kratki.csv")
srednji <- read.csv("Kromatin_srednji.csv")
dolgi <- read.csv("Kromatin_dolgi.csv")

#Za cenilko, ki smo jo pridobili po metodi momentov, izračunamo numerične približke

#Prvi eksperiment
n1 <- nrow(kratki)
x1 <- kratki[, 1]
t1 <- sqrt(2/pi)*mean(x1)

SE1 <- sqrt((4 - pi)/(n1 * pi))*t1

#Drugi eksperiment
n2 <- nrow(srednji)
x2 <- srednji[, 1]
t2 <- sqrt(2/pi)*mean(x2)

SE2 <- sqrt((4 - pi)/(n2 * pi))*t2

#Tretji eksperiment
n3 <- nrow(dolgi)
x3 <- dolgi[, 1]
t3 <- sqrt(2/pi)*mean(x3)

SE3 <- sqrt((4 - pi)/(n3 * pi))*t3

#Zapišimo ocenjene gostote porazdelitve

gostota <- function(y, t) {
# y je vektor razdalj, t skalarni parameter
	if(t != 0) {
		b <- rep(0, length(y)) #vektor v katerega hranimo rezultate
		for (i in 1:length(y)){
			if(y[i] >0){
				b[i] <- y[i]/(t)^2 * exp(- (y[i]^2) / (2*(t)^2))
			}
			else {
				b[i] <- 0
			}
		}
	}
	return(b)
}

tk <- sort(c(seq(0.001, 4, 0.001), t1-SE1, t1, t1+SE1, t2-SE2, t2, t2+SE2, t3-SE3, t3, t3+SE3))

verjetje1 <- function(t) {
#t je vektor parametrov
	r <- rep(0, length(t))
	for(i in 1:length(t)){
		r[i] <- prod(gostota(x1, t[i]))
	}
	return(r)
}

verjetje2 <- function(t) {
#t je vektor parametrov
	r <- rep(0, length(t))
	for(i in 1:length(t)){
		r[i] <- prod(gostota(x2, t[i]))
	}
	return(r)
}

verjetje3 <- function(t) {
#t je vektor parametrov
	r <- rep(0, length(t))
	for(i in 1:length(t)){
		r[i] <- prod(gostota(x3, t[i]))
	}
	return(r)
}
#Za vsak eksperiment še narišemo verjetje

par(mfrow=c(3, 1))

plot(tk, verjetje1(tk), type="l", main="Verjetje v 1. poskusu", xaxt= "n", xlab = "", ylab="")
axis(1, at = sort(c(seq(0, 6, 1), round(t1, 5))), las=2)
abline(v = t1, col = "blue")
abline(v = t1-SE1, col = "red", lty = 2, lwd=1)
abline(v = t1+SE1, col = "red", lty = 2, lwd=1)

plot(tk, verjetje2(tk), type="l", main="Verjetje v 2. poskusu", xaxt= "n", xlab = "", ylab="")
axis(1, at = sort(c(seq(0, 6, 1), round(t2, 5))), las=2)
abline(v = t2, col = "blue")
abline(v = t2-SE2, col = "red", lty = 2, lwd=1)
abline(v = t2+SE2, col = "red", lty = 2, lwd=1)

plot(tk, verjetje3(tk), type="l", main="Verjetje v 3. poskusu", xaxt = "n", xlab = "", ylab="")
axis(1, at = sort(c(seq(0, 12, 1), round(t3, 5))), las=2)
abline(v = t3, col = "blue")
abline(v = t3-SE3, col = "red", lty = 2, lwd=1)
abline(v = t3+SE3, col = "red", lty = 2, lwd=1)

cat("Približek za theta v prvem eksperimentu je", t1, ".")
cat("Standardna napaka v njem pa znaša",SE1, ".")

cat("Približek za theta v drugem eksperimentu je", t2, ".")
cat("Standardna napaka v njem znaša", SE2, ".")

cat("Približek za theta v tretjem eksperimentu je", t3, ".")
cat("Standardna napaka v njem znaša ", SE3, ".")
