
makeFigures <- function(longreport){

  longreport$measure <- as.character(longreport$measure)
  #all measures
  makeFigures_selectedMeasures(longreport,'_allmeasures')

  #core measures
  core_measures = c("percentFluc","drift","driftfit","SNR","SFNR","rdc")
  makeFigures_selectedMeasures(subset(longreport, measure %in% core_measures),'_coremeasures',figwidth = 12, figheight = 6)

}

makeFigures_selectedMeasures <- function(longreport, suffix,figwidth = 30, figheight = 'calc'){
  jandates = c('010117','010118','010119','010120','010121','010122')
  dates = c('070116','100116',
            '040117','070117','100117',
            '040118','070118','100118',
            '040119','070119','100119',
            '040120','070120','100120',
            '040121','070121','100121'
  )

  shadegreen <- '#009933'
  linegreen <- '#33ff33'
  shadeblue <- '#0033cc'
  lineblue <- '#33ccff'
  #3/2 ratio for figure size.
  if(figheight == 'calc'){figheight = figwidth*2/3}

  #1SD, 60&365
  ggplot(longreport, aes(x = scandate_epoch, y = value)) +
  theme_bw() +
  geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
  geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
  scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
  labs(x = '',title = 'Green: 1yr smooth. Blue: 60day smooth. Shaded = +/-1SD') +
  geom_point() +
  geom_ribbon(aes(y = value_smooth365, ymin = value_smooth365-value_smooth_sd365, ymax = value_smooth365+value_smooth_sd365),
              fill = shadegreen, alpha = .5) +
  geom_ribbon(aes(y = value_smooth60, ymin = value_smooth60-value_smooth_sd60, ymax = value_smooth60+value_smooth_sd60),
              fill = shadeblue, alpha = .5) +
  geom_line(aes(y = value_smooth365),color = linegreen) +
  geom_line(aes(y = value_smooth60),color = lineblue) +
  facet_wrap(.~measure,scales = 'free') +
  theme(axis.text.x = element_text(angle = 30))
  ggsave(sprintf('measures_1-365_1-60%s.png',suffix),width = figwidth, height = figheight)


#1&2 SD, 60
  ggplot(longreport, aes(x = scandate_epoch, y = value)) +
    theme_bw() +
    geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
    geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
    scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
    labs(x = '',title = 'Blue: 60day smooth. Shaded = +/- 1&2 SD') +
    geom_point() +
    geom_ribbon(aes(y = value_smooth60, ymin = value_smooth60-2*value_smooth_sd60, ymax = value_smooth60+2*value_smooth_sd60),
                fill = shadeblue, alpha = .25) +
    geom_ribbon(aes(y = value_smooth60, ymin = value_smooth60-value_smooth_sd60, ymax = value_smooth60+value_smooth_sd60),
                fill = shadeblue, alpha = .5) +
    geom_line(aes(y = value_smooth60),color = lineblue) +
    geom_point(data = subset(longreport, value < value_smooth60-2*value_smooth_sd60 | value > value_smooth60+2*value_smooth_sd60), color = 'red') +
    facet_wrap(.~measure,scales = 'free')+
    theme(axis.text.x = element_text(angle = 30))

  ggsave(sprintf('measures_1-60_2-60%s.png',suffix),width = figwidth, height = figheight)

  #1&2 SD, 365
  ggplot(longreport, aes(x = scandate_epoch, y = value)) +
    theme_bw() +
    geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
    geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
    scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
    labs(x = '',title = 'Green: 1yr smooth. Shaded = +/- 1&2 SD') +
    geom_point() +
    geom_ribbon(aes(y = value_smooth365, ymin = value_smooth365-2*value_smooth_sd365, ymax = value_smooth365+2*value_smooth_sd365),
                fill = shadegreen, alpha = .25) +
    geom_ribbon(aes(y = value_smooth365, ymin = value_smooth365-value_smooth_sd365, ymax = value_smooth365+value_smooth_sd365),
                fill = shadegreen, alpha = .5) +
    geom_line(aes(y = value_smooth365),color = linegreen) +
    geom_point(data = subset(longreport, value < value_smooth365-2*value_smooth_sd365 | value > value_smooth365+2*value_smooth_sd365), color = 'red') +
    facet_wrap(.~measure,scales = 'free')+
    theme(axis.text.x = element_text(angle = 30))

  ggsave(sprintf('measures_1-365_2-365%s.png',suffix),width = figwidth, height = figheight)


}

