qdir <- '//kumc.edu/data/Research/Hoglund/Bartolotti_J/QA'
qa <- read.csv(file.path(qdir, 'QA_report.csv'))
qafb <- read.csv(file.path(qdir, 'QA_report_fbirn.csv'))

dates <- list(baseline = list(start = '2000-01-01', end = '2021-12-31'),
              gradient_first = list(start = '2022-01-01', end = '2022-07-31'),
              graderror = list(start = '2022-08-01', end = '2022-08-18'),
              gradient_second = list(start = '2022-08-19', end = '2023-11-24'),
              gradient_third = list(start = '2023-11-25', end = '2023-12-12')
              )
dates$baseline$start_epoch <- as.numeric(as.Date(dates$baseline$start))
dates$baseline$end_epoch <- as.numeric(as.Date(dates$baseline$end))
dates$gradient_first$start_epoch <- as.numeric(as.Date(dates$gradient_first$start))
dates$gradient_first$end_epoch <- as.numeric(as.Date(dates$gradient_first$end))
dates$graderror$start_epoch <- as.numeric(as.Date(dates$graderror$start))
dates$graderror$end_epoch <- as.numeric(as.Date(dates$graderror$end))
dates$gradient_second$start_epoch <- as.numeric(as.Date(dates$gradient_second$start))
dates$gradient_second$end_epoch <- as.numeric(as.Date(dates$gradient_second$end))
dates$gradient_third$start_epoch <- as.numeric(as.Date(dates$gradient_third$start))
dates$gradient_third$end_epoch <- as.numeric(as.Date(dates$gradient_third$end))


qa_baseline <- subset(qa, scandate_epoch >= dates$baseline$start_epoch & scandate_epoch <= dates$baseline$end_epoch)
qa_gradient_first <- subset(qa, scandate_epoch >= dates$gradient_first$start_epoch & scandate_epoch <= dates$gradient_first$end_epoch)
qa_graderror <- subset(qa, scandate_epoch >= dates$graderror$start_epoch & scandate_epoch <= dates$graderror$end_epoch)
qa_gradient_second <- subset(qa, scandate_epoch >= dates$gradient_second$start_epoch & scandate_epoch <= dates$gradient_second$end_epoch)
qa_gradient_third <- subset(qa, scandate_epoch >= dates$gradient_third$start_epoch & scandate_epoch <= dates$gradient_third$end_epoch)

qafb_baseline <- subset(qafb, scandate_epoch >= dates$baseline$start_epoch & scandate_epoch <= dates$baseline$end_epoch)
qafb_gradient_first <- subset(qafb, scandate_epoch >= dates$gradient_first$start_epoch & scandate_epoch <= dates$gradient_first$end_epoch)
qafb_graderror <- subset(qafb, scandate_epoch >= dates$graderror$start_epoch & scandate_epoch <= dates$graderror$end_epoch)
qafb_gradient_second <- subset(qafb, scandate_epoch >= dates$gradient_second$start_epoch & scandate_epoch <= dates$gradient_second$end_epoch)
qafb_gradient_third <- subset(qafb, scandate_epoch >= dates$gradient_third$start_epoch & scandate_epoch <= dates$gradient_third$end_epoch)


qa_baseline$period <- 'baseline'
qa_gradient_first$period <- 'grad1'
qa_graderror$period <- 'grad1_error'
qa_gradient_second$period <- 'grad2'
qa_gradient_third$period <- 'grad3'

qal <- rbind(qa_baseline, qa_gradient_first, qa_graderror, qa_gradient_second, qa_gradient_third)
qal$phantom <- 'bullet'

qafb_baseline$period <- 'baseline'
qafb_gradient_first$period <- 'grad1'
qafb_graderror$period <- 'grad1_error'
qafb_gradient_second$period <- 'grad2'
qafb_gradient_third$period <- 'grad3'

qafbl <- rbind(qafb_baseline, qafb_gradient_first, qafb_graderror, qafb_gradient_second, qafb_gradient_third)
qafbl$phantom <- 'fbirn'

qq <- rbind(qal,qafbl)
qq$gradient_period <- qq$period
qq$gradient_period[qq$gradient_period == 'baseline'] <- 'grad1'



library(ggplot2)
library(ggbeeswarm)
library(Hmisc)
ggplot(qq, aes(x = gradient_period, y = drift, color = phantom)) + geom_quasirandom()


qqdrift <- qq
qqdrift$measure <- 'drift'
qqdrift$value <- qqdrift$drift
qqdriftfit <- qq
qqdriftfit$measure <- 'driftfit'
qqdriftfit$value <- qqdrift$driftfit

qqpercentFluc <- qq
qqpercentFluc$measure <- 'percentFluc'
qqpercentFluc$value <- qqpercentFluc$percentFluc

qqrdc <- qq
qqrdc$measure <- 'rdc'
qqrdc$value <- qqrdc$rdc

qqSNR <- qq
qqSNR$measure <- 'SNR'
qqSNR$value <- qqSNR$SNR

qqSFNR <- qq
qqSFNR$measure <- 'SFNR'
qqSFNR$value <- qqSFNR$SFNR

qqq <- rbind(qqdrift, qqdriftfit, qqpercentFluc, qqrdc, qqSNR, qqSFNR)
qqqed <- qqq
qqqed <- subset(qqqed,  drift < 2 & percentFluc < .2 & SNR > 50)

ggplot(qqqed, aes(x = gradient_period, y = value, color = phantom)) +
  theme_bw() +
  geom_quasirandom() +
  scale_color_manual(values = c('#FF6666','#6666FF')) +
  stat_summary(data = subset(qqqed, phantom == 'bullet' & gradient_period %in% c('grad1','grad2')), fun.data = 'mean_cl_normal', geom = 'errorbar', width = .3, color = 'black', position = position_nudge(x = -.2)) +
  stat_summary(data = subset(qqqed, phantom == 'bullet'), fun.y = 'mean', geom = 'point', color = 'black', size = 3, position = position_nudge(x = -.2)) +
  stat_summary(data = subset(qqqed, phantom == 'bullet'), fun.y = 'mean', geom = 'point', color = 'red', size = 1.5, position = position_nudge(x = -.2)) +
  stat_summary(data = subset(qqqed, phantom == 'fbirn' & gradient_period %in% c('grad1','grad2')), fun.data = 'mean_cl_normal', geom = 'errorbar', width = .3, color = 'black', position = position_nudge(x = .2)) +
  stat_summary(data = subset(qqqed, phantom == 'fbirn'), fun.y = 'mean', geom = 'point', color = 'black', size = 3, position = position_nudge(x = .2)) +
  stat_summary(data = subset(qqqed, phantom == 'fbirn'), fun.y = 'mean', geom = 'point', color = 'blue', size = 1.5, position = position_nudge(x = .2)) +

  #  stat_summary(fun.y = 'mean', geom = 'point', size = 1.5) +
#  stat_summary(data = subset(qqqed, gradient_period %in% c('grad1','grad2')), fun.data = 'mean_cl_normal', geom = 'errorbar', width = .3) +
  facet_wrap(.~measure, scales = 'free', ncol = 3) +
  labs(title = 'Gradient 1: 2017-1-1 to 2022-07-31\nGradient1_Error: 2022-08-01 to 2022-08-29\nGradient 2: 2022-08-29 to 2023-11-24\nGradient 3: 2023-12-12 to present')

ggsave('QA_by_gradient.png', width = 16, height = 8)




