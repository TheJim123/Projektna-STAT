#Nastavimo mesto datoteke (Po potrebi spremenite na mesto, kamor ste shranili datoteke)
#Npr. setwd("C:/Users/Uporabnik/Documents/GitHub/STAT")

#Naložimo podatke eksperimentov
kratki <- read.csv("Kromatin_kratki.csv")
srednji <- read.csv("Kromatin_srednji.csv")
dolgi <- read.csv("Kromatin_dolgi.csv")

#Prvi eksperiment
n1 <- nrow(kratki)
x1 <- kratki[, 1]

t1.mnv <- sqrt(sum(x1^2)/(2*n1))
t1.mm <- sqrt(2/pi)*mean(x1)

#Drugi eksperiment
n2 <- nrow(srednji)
x2 <- srednji[, 1]

t2.mnv <- sqrt(sum(x2^2)/(2*n2))
t2.mm <- sqrt(2/pi)*mean(x2)

#Tretji eksperiment
n3 <- nrow(dolgi)
x3 <- dolgi[, 1]

t3.mnv <- sqrt(sum(x3^2)/(2*n3))
t3.mm <- sqrt(2/pi)*mean(x3)

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

gostota1MNV <- function(r) {
	gostota(r, t1.mnv)
}

gostota1MM <- function(r) {
	gostota(r, t1.mm)
}


gostota2MNV <- function(r) {
	gostota(r, t2.mnv)
}

gostota2MM <- function(r) {
	gostota(r, t2.mm)
}


gostota3MNV <- function(r) {
	gostota(r, t3.mnv)
}

gostota3MM <- function(r) {
	gostota(r, t3.mm)
}

#Za vsak eksperiment poračunamo širino intervalov po modificiranem FD pravilu
wk <- 2.6*IQR(x1)/(n1)^(1/3)
ws <- 2.6*IQR(x2)/(n2)^(1/3)
wd <- 2.6*IQR(x3)/(n3)^(1/3)

#Zdaj poračunajmo vektorje, ki nam bodo določili meje intervalov

meje <- function(x){
	#Sprejme stolpec meritev in vrne vektor mej med katerimi je razdalja enaka 
	#širini razredov po modificiranem Freedman-Diaconisovem pravilu
	n <- length(x)
	M <- max(x)
	i <- IQR(x)
	#Širina razredov po Freedman-Diaconisovem pravilu zaokrožena na 5 decimalk
	w <- round(2.6*i/n^(1/3), 5)
	vec <- c(0)
	j <- 0
	while (j < M){
		j <- j+w
		vec <- c(vec, j)
	}
	return(vec)
}

xk <- sort(c(seq(0, max(x1+0.5), 0.001), t1.mnv, t1.mm))
xs <- sort(c(seq(0, max(x2+0.5), 0.001), t2.mnv, t2.mm))
xd <- sort(c(seq(0, max(x3+0.5), 0.001), t3.mnv, t3.mm))

xa <- sort(c(xk, xs, xd))

par(mfrow=c(1, 3))

h1<-hist(x1, breaks = meje(x1), col="red", xlab="meritve", ylab="gostota", ylim = c(0.00, 0.7),
   main="Histogram meritev I. poskusa", prob = TRUE) 
lines(xk, gostota1MNV(xk), col="green", lwd=2)
lines(xk, gostota1MM(xk), col="blue", lwd=2)
legend(x="topright", legend=c("Gostota po MNV", "Gostota po MM"), col = c("green", "blue"), lwd=2)

h2<-hist(x2, breaks = meje(x2), col="red", xlab="meritve", ylab="gostota", ylim = c(0, 0.4),
   main="Histogram meritev II. poskusa", prob = TRUE) 
lines(xs, gostota2MNV(xs), col="green", lwd=2)
lines(xs, gostota2MM(xs), col="blue", lwd=2)
legend(x="topright", legend=c("Gostota po MNV", "Gostota po MM"), col = c("green", "blue"), lwd=2)

h3<-hist(x3, breaks = meje(x3), col="red", xlab="meritve", ylab="gostota", ylim = c(0, 0.3),
   main="Histogram meritev III. poskusa", prob = TRUE) 
lines(xd, gostota3MNV(xd), col="green", lwd=2)
lines(xd, gostota3MM(xd), col="blue", lwd=2)
legend(x="topright", legend=c("Gostota po MNV", "Gostota po MM"), col = c("green", "blue"), lwd=2)