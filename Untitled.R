library("fitdistrplus")

dane_kabanos <- read.csv("./csvki/tar_d_compared_with_mak.csv")

plotdist(dane_kabanos$Zamkniecie, histo = TRUE, demp = TRUE)

descdist(dane_kabanos$Zamkniecie, boot = 1000)

?fitdist

gofstat(list(fendo.ln, fendo.ll, fendo.P, fendo.B), fitnames = c("lnorm", "llogis", "Pareto", "Burr"))

