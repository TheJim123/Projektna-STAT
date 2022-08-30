#Nastavimo mesto datoteke (Po potrebi spremenite na mesto, kamor ste shranili datoteke)
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Naložimo podatke eksperimentov
kratki <- read.csv("Kromatin_kratki.csv")
srednji <- read.csv("Kromatin_srednji.csv")
dolgi <- read.csv("Kromatin_dolgi.csv")

#Za cenilko, ki smo jo pridobili po metodi največjega verjetja, izračunamo
#numerične približke

#Prvi eksperiment
n1 <- nrow(kratki)
x1 <- kratki[, 1]
t1 <- sqrt(sum(x1^2)/(2*n1))

SE1 <- t1*sqrt(1 - (gamma(n1 + 0.5)/(sqrt(n1)*gamma(n1)))^2)

#Drugi eksperiment
n2 <- nrow(srednji)
x2 <- srednji[, 1]
t2 <- sqrt(sum(x2^2)/(2*n2))

# SE2 <- t2*sqrt(1 - (gamma(n2 + 0.5)/(sqrt(n2)*gamma(n2)))^2)/sqrt(n2)
#Pri izračunu gamma(n2) nam R vrne Inf (neskončno). Posledično, nam za SE2 vrne
#vrednost NaN. Zaradi tega bomo tukaj vstavili drugod izračunano vrednost.
#Spletna stran wolframalpha nam vrne numerični rezultat
SE2 <- 0.06589590

#Tretji eksperiment
n3 <- nrow(dolgi)
x3 <- dolgi[, 1]
t3 <- sqrt(sum(x3^2)/(2*n3))

SE3 <- t3*sqrt(1 - (gamma(n3 + 0.5)/(sqrt(n3)*gamma(n3)))^2)


#Zapišimo ocenjene gostote porazdelitve

gostota <- function(y, t) {
# y je vektor razdalj, t skalarni parameter
	if(t != 0) {
		b <- rep(0, length(y))
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

gost1 <- function(r) {
	gostota(r, t1)
}

gost2 <- function(r) {
	gostota(r, t2)
}

gost3 <- function(r) {
	gostota(r, t3)
}

#Za vsak eksperiment še narišemo gostoto porazdelitve

xk <- sort(c(seq(0, 6, 0.001), t1-SE1, t1, t1+SE1))
xs <- sort(c(seq(0, 8, 0.001), t2-SE2, t2, t2+SE2))
xd <- sort(c(seq(0, 12, 0.001), t3-SE3, t3, t3+SE3))

xa <- sort(c(xd, t1-SE1, t1, t1+SE1, t2-SE2, t2, t2+SE2))

par(mfrow=c(2, 2))

plot(xk, gost1(xk), type="l", main="Gostota razdalj v 1. poskusu", xaxt= "n", yaxt = "n", xlab = "", ylab="")
axis(1, at = sort(c(seq(0, 6, 1), round(t1, 5))), las=2)
axis(2, at = seq(0, 0.6, 0.01), las=2)
abline(v = t1, col = "blue")
abline(v = t1-SE1, col = "red", lty = 2, lwd=1)
abline(v = t1+SE1, col = "red", lty = 2, lwd=1)

plot(xs, gost2(xs), type="l", main="Gostota razdalj v 2. poskusu", xaxt= "n", yaxt = "n", xlab = "", ylab="")
axis(1, at = sort(c(seq(0, 10, 1), round(t2, 5))), las=2)
axis(2, at = seq(0, 0.3, 0.01), las=2)
abline(v = t2, col = "blue")
abline(v = t2-SE2, col = "red", lty = 2, lwd=1)
abline(v = t2+SE2, col = "red", lty = 2, lwd=1)

plot(xd, gost3(xd), type="l", main="Gostota razdalj v 3. poskusu", xaxt = "n", yaxt = "n", xlab = "", ylab="")
axis(1, at = sort(c(seq(0, 12, 1), round(t3, 5))), las=2)
axis(2, at = seq(0, 0.2, 0.01), las=2)
abline(v = t3, col = "blue")
abline(v = t3-SE3, col = "red", lty = 2, lwd=1)
abline(v = t3+SE3, col = "red", lty = 2, lwd=1)

plot(xa, gost1(xa), type="l", main="Gostote razdalj v vseh treh poskusih", xaxt= "n", yaxt = "n", xlab = "", ylab="", col="red")
axis(1, at = sort(c(seq(0, 12, 1), round(t1, 5), round(t2, 5), round(t3, 5))), las=2)
axis(2, at = seq(0, 0.6, 0.01), las=2)

lines(xa, gost2(xa), type="l", col="green")
lines(xa, gost3(xa), type="l", col="blue")

abline(v = t1, col = "violet")
abline(v = t1-SE1, col = "orange", lty = 2, lwd=1)
abline(v = t1+SE1, col = "orange", lty = 2, lwd=1)

abline(v = t2, col = "violet")
abline(v = t2-SE2, col = "orange", lty = 2, lwd=1)
abline(v = t2+SE2, col = "orange", lty = 2, lwd=1)

abline(v = t3, col = "violet")
abline(v = t3-SE3, col = "orange", lty = 2, lwd=1)
abline(v = t3+SE3, col = "orange", lty = 2, lwd=1)

legend(x="topright", legend=c("Gostota za I. poskus", "Gostota za II. poskus", "Gostota za III. poskus"), col = c("red", "green", "blue"), lwd=2)

cat("Približek za theta v prvem eksperimentu je", t1, ".")
cat("Standardna napaka v njem pa znaša",SE1, ".")

cat("Približek za theta v drugem eksperimentu je", t2, ".")
cat("Standardna napaka v njem znaša", SE2, ".")

cat("Približek za theta v tretjem eksperimentu je", t3, ".")
cat("Standardna napaka v njem znaša ", SE3, ".")
