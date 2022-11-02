dane_kabanos <- read.csv("/Users/karolstudniarek/Desktop/REPOZYTORIA/kabanosProjekt/csvki/tar_d_compared_with_mak.csv")
dane_makaron <- read.csv("/Users/karolstudniarek/Desktop/REPOZYTORIA/kabanosProjekt/csvki/mak_d_compared_with_tar.csv")

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


