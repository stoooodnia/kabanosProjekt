library(e1071)
library(fitdistrplus)
library(actuar)

dane_kabanos <- read.csv("./csvki/tar_d_compared_with_mak.csv")
dane_makaron <- read.csv("./csvki/mak_d_compared_with_tar.csv")

# zad1 

kabanos_zamk <- dane_kabanos[,'Zamkniecie']
makaron_zamk <- dane_makaron[,'Zamkniecie']

# zad3

kfn <- fitdist(kabanos_zamk, "norm")
summary(fn)

kfln <- fitdist(kabanos_zamk, "lnorm")
summary(fn)

kfw <- fitdist(kabanos_zamk, "weibull")
summary(kfw)

mfn <- fitdist(makaron_zamk, "norm")
summary(mfn)

mfln <- fitdist(makaron_zamk, "lnorm")
summary(mfln)

mfw <- fitdist(makaron_zamk, "weibull")
summary(mfw)

# zad5

# tarczynski weibull

N <- 10000
kabanos_n <- length(kabanos_zamk); kabanos_n

kabanos_Dw <- c()

for (i in 1:N) {
  kabanos_Yw <- rweibull(kabanos_n,kfw$estimate[1],kfw$estimate[2])
  
  kabanos_Dw[i] <- ks.test(kabanos_Yw, pweibull, kfw$estimate[1], kfw$estimate[2], exact=TRUE)$statistic
}

# obliczamy dn wartosc statystki Dn, dla x1, x2, ..., xn oraz wybranego F0

kabanos_dn_w <- ks.test(kabanos_zamk, pweibull, kfw$estimate[1], kfw$estimate[2],exact=TRUE)$statistic
kabanos_dn_w
#wyniki na hitogramie

par(mfrow=c(1,1))
hist(kabanos_Dw,main="Histogram", xlim=c(0,0.14))
points(kabanos_dn_w, 0, pch=19, col=2)


#obliczamy prawdopodobieństwo p = P(Dn > dn)

kabanos_p_value_n <- length(kabanos_Dw[kabanos_Dw>kabanos_dn_w])/N; kabanos_p_value_n

# ustalamy alfa - poziom istotności testu
alpha <- 0.05

kabanos_p_value_n <= alpha

# makaron log-normal

M <- 10000
m <- length(makaron_zamk); m

Dmln <- c()

for (i in 1:M) {
  Ymln <- rlnorm(m, mfln$estimate[1],mfln$estimate[2])
  
  Dmln[i] <- ks.test(Ymln, plnorm, mfln$estimate[1], mfln$estimate[2], exact=TRUE)$statistic
}

# obliczanie dn, wart statystyki Dn i rozkładu F0 w A 
dmln_ln <- ks.test(makaron_zamk, plnorm, mfln$estimate[[1]], mfln$estimate[[2]], exact=TRUE)$statistic
dmln_ln

#histogram 
par(mfrow=c(1,1))
hist(Dmln,main="Histogram", xlim=c(0,0.14))
points(dmln_ln, 0, pch=19, col=2)

#odleglosc dystrybuanty empirycznej (wartosc statystki Dn) oraz dystrybuanty F0
p_value_m_ln <- length(Dmln[Dmln>dmln_ln])/N; p_value_m_ln

# poziom istotnosci 0.5
alpha <- 0.05
p_value_m_ln <= alpha
