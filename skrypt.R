library(e1071)


dane_kabanos <- read.csv("kabanosProjekt/csvki/tar_d_compared_with_mak.csv")
dane_makaron <- read.csv("kabanosProjekt/csvki/mak_d_compared_with_tar.csv")

# zad1 

#kabanos_zamk <- dane_kabanos$Zamkniecie
#makaron_zamk <- dane_makaron$Zamkniecie

kabanos_zamk <- dane_kabanos[,'Zamkniecie']
makaron_zamk <- dane_makaron[,'Zamkniecie']

plot(kabanos_zamk)
plot(makaron_zamk)

hist(kabanos_zamk, prob=TRUE)
hist(makaron_zamk, prob=TRUE)

# zad2

mean(kabanos_zamk)
mean(makaron_zamk)

sd(kabanos_zamk)
sd(makaron_zamk)

skewness(kabanos_zamk)
skewness(makaron_zamk)

kurtosis(kabanos_zamk)
kurtosis(makaron_zamk)

# zad3

plot(rnorm(kabanos_zamk))
