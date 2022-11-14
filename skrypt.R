library(e1071)
library(fitdistrplus)
library(actuar)

dane_kabanos <- read.csv("./csvki/tar_d_compared_with_mak.csv")
dane_makaron <- read.csv("./csvki/mak_d_compared_with_tar.csv")

# zad1 

kabanos_zamk <- dane_kabanos[,'Zamkniecie']
makaron_zamk <- dane_makaron[,'Zamkniecie']

plotdist(kabanos_zamk, histo = TRUE, demp = TRUE)
plotdist(makaron_zamk, histo = TRUE, demp = TRUE)

?plotdist

# zad2

descdist(kabanos_zamk, boot = 1001)
descdist(makaron_zamk, boot = 1001)

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

# cz.2

par(mfrow = c(2,2))
plot.legend <- c("normal", "log-normal", "weibull")
denscomp(list(kfn, kfln, kfw), legendtext = plot.legend)
qqcomp(list(kfn, kfln, kfw), legendtext = plot.legend)
cdfcomp(list(kfn, kfln, kfw), legendtext = plot.legend)
ppcomp(list(kfn, kfln, kfw), legendtext = plot.legend)

denscomp(list(mfn, mfln, mfw), legendtext = plot.legend)
qqcomp(list(mfn, mfln, mfw), legendtext = plot.legend)
cdfcomp(list(mfn, mfln, mfw), legendtext = plot.legend)
ppcomp(list(mfn, mfln, mfw), legendtext = plot.legend)


# zad4
 
#kab.fn <- fitdist(kabanos_zamk, "norm", start = list(shape = 1, scale = 500))
#kab.fln <- fitdist(kabanos_zamk, "lnorm", start = list(shape = 1, scale = 500))
#kab.fp <- fitdist (kabanos_zamk, "pareto", start = list(shape1 = 0.3, shape2 = 1, rate = 1))

#mak.fn <- fitdist(makaron_zamk, "norm", start = list(shape = 1, scale = 500))
#mak.fln <- fitdist(makaron_zamk, "lnorm", start = list(shape = 1, scale = 500))
#mak.fp <- fitdist (makaron_zamk, "pareto", start = list(shape1 = 0.3, shape2 = 1, rate = 1))

#cdfcomp(list(kab.fn, kab.fln, kab.fp), xlogscale = TRUE, ylogscale = TRUE, legend = c("normal", "log-normal", "Pareto"))
#cdfcomp(list(mak.fn, mak.fln, mak.fp), xlogscale = TRUE, ylogscale = TRUE, legend = c("normal", "log-normal", "Pareto"))

gofstat(list(kfn, kfln, kfw), fitnames = c("norm","lnorm", "weibull"))
gofstat(list(mfn, mfln, mfw), fitnames = c("norm","lnorm", "weibull"))








