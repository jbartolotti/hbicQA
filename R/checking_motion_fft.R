
dummyfunction <- function(){
library(ggplot2)
library(grid)
library(png)

#system.file('extdata','myfile.png', package = 'hbicQA')
kayv_drift <- png::readPNG(file.path('inst','extdata','kayvanrad_2021_drift.png'))

ggplot(data.frame(x = 0:6, y = 0:6), aes(x = x, y = y)) +
  theme_bw() +
  annotation_custom(rasterGrob(kayv_drift,
                               width = unit(1,'npc'),
                               height = unit(1,'npc')),
                    -Inf,Inf,0,4) +
  geom_point() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))




################
setwd('//kumc.edu/data/Research/Hoglund/Bartolotti_J/fmriquality')
mm <- read.delim('mot_deriv.r01.1D', header = FALSE, sep = ' ')
mm2 <- read.delim('motion_demean_boldqa.1D', header = FALSE, sep = ' ')

colnames(mm) <- c('roll','pitch','yaw','dS','dL','dP')
colnames(mm2) <- c('roll','pitch','yaw','dS','dL','dP')

library(ggplot2)
ggplot(mm2, aes(x = 1:dim(mm)[1], y = dL)) + geom_line()



library(signal)

x <- fft(mm2$dP)
N <- length(x)
f <- (0:(N-1)) * 1 / N

xx <- x[1:(N/2)]
ff <- f[1:(N/2)]

plot(ff, Mod(xx), type = "h", xlab = "Frequency (Hz)", ylab = "Amplitude", main = "Amplitude Spectrum")

dfP <- data.frame(ff = ff, modx = Mod(xx), xx = xx)
ggplot(dfP, aes(x = ff, y = modx)) +
  geom_segment(aes(xend = ff, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum") +
  coord_cartesian(ylim = c(0,15))


xS <- fft(mm2$dS)
NS <- length(xS)
fS <- (0:(NS-1)) * 1 / NS

xxS <- xS[1:(NS/2)]
ffS <- fS[1:(NS/2)]

plot(ffS, Mod(xxS), type = "h", xlab = "Frequency (Hz)", ylab = "Amplitude", main = "Amplitude Spectrum")

df <- data.frame(ffS = ffS, modxs = Mod(xxS), xxS = xxS)
ggplot(df, aes(x = ffS, y = modxs)) +
  geom_segment(aes(xend = ffS, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum") +
  coord_cartesian(ylim = c(0,15))

aa <- data.frame(f = ff, xdP = Mod(xx), xdS = Mod(xxS))
aa$PminusS <- aa$xdP - aa$xdS
ggplot(aa, aes(x = f, y = PminusS)) +
  geom_segment(aes(xend = f, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum: dP - dS")


xdP <- fft(mm2$dP)
xdS <- fft(mm2$dS)
xdL <- fft(mm2$dL)

N <- length(xdP)
f <- (0:(N-1)) * 1/N

xdP <- xdP[1:(N/2)]
xdS <- xdS[1:(N/2)]
xdL <- xdL[1:(N/2)]
f <- f[1:(N/2)]

df <- data.frame(f = f, xdP = Mod(xdP), xdS = Mod(xdS), xdL = Mod(xdL))
df$P_S <- df$xdP - df$xdS
df$P_L <- df$xdP - df$xdL
df$S_L <- df$xdS - df$xdL


library(tidyr)
df_long <- pivot_longer(df, cols = c(xdP, xdS, xdL, P_S, P_L, S_L), names_to = "measure", values_to = "value")

ggplot(df_long, aes(x = f, y = value)) +
  geom_segment(aes(xend = f, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum") + facet_wrap(.~measure, ncol = 3) + theme_bw()
ggsave('myfft.png', width = 12, height = 8)




##########
mme <- read.delim('motion_demean_ME.1D', header = FALSE, sep = ' ')

colnames(mme) <- c('roll','pitch','yaw','dS','dL','dP')


xedP <- fft(mme$dP)
xedS <- fft(mme$dS)
xedL <- fft(mme$dL)

Ne <- length(xedP)
fe <- (0:(Ne-1)) * 1.355/Ne

xedP <- xedP[1:(Ne/2)]
xedS <- xedS[1:(Ne/2)]
xedL <- xedL[1:(Ne/2)]
fe <- fe[1:(Ne/2)]

dfe <- data.frame(f = fe, xdP = Mod(xedP), xdS = Mod(xedS), xdL = Mod(xedL))
dfe$P_S <- dfe$xdP - dfe$xdS
dfe$P_L <- dfe$xdP - dfe$xdL
dfe$S_L <- dfe$xdS - dfe$xdL


dfe_long <- pivot_longer(dfe, cols = c(xdP, xdS, xdL, P_S, P_L, S_L), names_to = "measure", values_to = "value")
ggplot(dfe_long, aes(x = f, y = value)) +
  geom_segment(aes(xend = f, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum") + facet_wrap(.~measure, ncol = 3) + theme_bw()

ggsave('myfft_ME.png', width = 12, height = 8)

ggplot(subset(dfe_long, measure %in% c('xdP','xdS','xdL')), aes(x = f, y = value)) +
  geom_segment(aes(xend = f, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum") + facet_wrap(.~measure, ncol = 1) + theme_bw() +
  coord_cartesian(ylim = c(0,15))
ggsave('myfft_ME.png', width = 6, height = 6)


ggplot(subset(df_long, measure %in% c('xdP','xdS','xdL')), aes(x = f, y = value)) +
  geom_segment(aes(xend = f, yend = 0), color = "blue", size = 1) +
  xlab("Frequency (Hz)") +
  ylab("Amplitude") +
  ggtitle("Amplitude Spectrum") + facet_wrap(.~measure, ncol = 1) + theme_bw() +
  coord_cartesian(ylim = c(0,15))
ggsave('myfft_BOLDQA.png', width = 6, height = 6)
}

