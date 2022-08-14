#Nastavimo mesto datoteke (Po potrebi spremenite na mesto, kamor ste shranili datoteke)
setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Naložimo podatke eksperimentov
kratki <- read.csv("Kromatin_kratki.csv")
srednji <- read.csv("Kromatin_srednji.csv")
dolgi <- read.csv("Kromatin_dolgi.csv")

#Za cenilko, ki smo jo pridobili po metodi momentov, izračunamo numerične približke

#Prvi eksperiment
n1 <- nrow(kratki)
x1 <- kratki[, 1]
t1 <- sqrt(2/pi)*mean(x1)

#SE1 MORA BITI ZA CENILKO

#Drugi eksperiment
n2 <- nrow(srednji)
x2 <- srednji[, 1]
t2 <- sqrt(2/pi)*mean(x2)

#SE2 MORA BITI ZA CENILKO

#Tretji eksperiment
n3 <- nrow(dolgi)
x3 <- dolgi[, 1]
t3 <- sqrt(2/pi)*mean(x3)

#SE3 MORA BITI ZA CENILKO

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

par(mfrow=c(2, 3))
plot(density(x1), main="Gostota razdalj v 1. poskusu")
plot(density(x2), main="Gostota razdalj v 2. poskusu")
plot(density(x3), main="Gostota razdalj v 3. poskusu")

x <- seq(0, 11, 0.01)
plot(x, gost1(x), type="l", main="Gostota razdalj v 1. poskusu")
plot(x, gost2(x), type="l", main="Gostota razdalj v 2. poskusu")
plot(x, gost3(x), type="l", main="Gostota razdalj v 3. poskusu")


cat("Približek za theta v prvem eksperimentu je", t1, ".")
#cat("Standardna napaka v njem pa znaša",SE1, ".")

cat("Približek za theta v drugem eksperimentu je", t2, ".")
#cat("Standardna napaka v njem znaša", SE2, ".")

cat("Približek za theta v tretjem eksperimentu je", t3, ".")
#cat("Standardna napaka v njem znaša ", SE3, ".")
