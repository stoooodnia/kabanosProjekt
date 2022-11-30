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
# ZAD 5
N <- 10000
n_kab <- length(kabanos_zamk); n

Dln <- c()

for (i in 1:N) {
  Yln <- rlnorm(n_peo, estimated_param_lnorm$estimate[1], estimated_param_lnorm$estimate[2])
  
  Dln[i] <- ks.test(Yln, plnorm, estimated_param_lnorm$estimate[1],estimated_param_lnorm$estimate[2],exact=TRUE)$statistic
}


dn_ln <- ks.test(kabanos_zamk, plnorm, estimated_param_lnorm$estimate[1],estimated_param_lnorm$estimate[2],exact=TRUE)$statistic
dn_ln

par(mfrow=c(1,1))
hist(Dln,prob=T, main = "Histogram", xlim = c(0,0.14))
points(dn_ln,0,pch=19,col=2)
dev.off()

p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln

#wartosci p-value rowne zero,oznaczają, że przy dowolnie przyjetym poziomie istotnosci
#hipoteze o rownosci rozkladow odrzucamy


# ZAD 5
N <- 10000
n_peo <- length(kab); n

Dln <- c()

for (i in 1:N) {
  Yln <- rlnorm(n_peo, estimated_param_lnorm$estimate[1], estimated_param_lnorm$estimate[2])
  
  Dln[i] <- ks.test(Yln, plnorm, estimated_param_lnorm$estimate[1],estimated_param_lnorm$estimate[2],exact=TRUE)$statistic
}


dn_ln <- ks.test(peo_close, plnorm, estimated_param_lnorm$estimate[1],estimated_param_lnorm$estimate[2],exact=TRUE)$statistic
dn_ln

png("img/peo_hist_hip.png", width = 1000, height = 600, units = 'px', res = 100)
par(mfrow=c(1,1))
hist(Dln,prob=T, main = "Histogram", xlim = c(0,0.14))
points(dn_ln,0,pch=19,col=2)
dev.off()

p_value_ln <- length(Dln[Dln>dn_ln])/N; p_value_ln

#wartosci p-value rowne zero,oznaczają, że przy dowolnie przyjetym poziomie istotnosci
#hipoteze o rownosci rozkladow odrzucamy






