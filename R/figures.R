
makeFigures <- function(longreport, figdir,dosave = TRUE){

  longreport$measure <- as.character(longreport$measure)
  #all measures
  makeFigures_selectedMeasures(longreport,'_allmeasures',figdir, dosave = dosave)

  #core measures
  core_measures = c("percentFluc","drift","driftfit","SNR","SFNR","rdc")
  makeFigures_selectedMeasures(subset(longreport, measure %in% core_measures),'_coremeasures',figdir,figwidth = 12, figheight = 6, dosave = dosave)

}

makeFigures_selectedMeasures <- function(longreport, suffix, figdir, figwidth = 30, figheight = 'calc', dosave = TRUE){
  jandates = c('010117','010118','010119','010120','010121','010122')
  dates = c('070116','100116',
            '040117','070117','100117',
            '040118','070118','100118',
            '040119','070119','100119',
            '040120','070120','100120',
            '040121','070121','100121'
  )
  current_measures <- unique(longreport$measure)
  num_measures <- length(current_measures)
  shadegreen <- '#009933'
  linegreen <- '#33ff33'
  shadeblue <- '#0033cc'
  lineblue <- '#33ccff'

  dot_outline_green <- '#3CA61C'
  dot_outline_red <- '#E12A1B'

  #3/2 ratio for figure size.
  if(figheight == 'calc'){figheight = figwidth*2/3}

  numcol <- round(sqrt(num_measures))+1
  numrow <- ceiling(num_measures/numcol)

  oneyeardat <- subset(longreport, scandate_epoch > as.numeric(Sys.Date())-365)
  oneyeardat$z_value_smooth60 <- unlist(lapply(1:dim(oneyeardat)[1], function(x){
    (oneyeardat$value[x] - mean(oneyeardat$value[oneyeardat$measure == oneyeardat$measure[x]],na.rm = TRUE)) / sd(oneyeardat$value[oneyeardat$measure == oneyeardat$measure[x]],na.rm = TRUE)
    }))
  oneyeardat_mostrecent <- oneyeardat[oneyeardat$scandate_epoch == max(oneyeardat$scandate_epoch, na.rm = TRUE),]
  #zscore dotplot 60 days, 1&2sd calced on 365 to present
  ggplot(subset(oneyeardat, scandate_epoch > as.numeric(Sys.Date())-60), aes(x = measure, y = z_value_smooth60, alpha = scandate_epoch)) +
  theme_bw() +
    scale_x_discrete() +
    annotate('rect',xmin = 0, xmax = num_measures+1, ymin = -2, ymax = 2, fill = '#98B6FA', alpha = .4) +
    annotate('rect',xmin = 0, xmax = num_measures+1, ymin = -1, ymax = 1, fill = '#98B6FA', alpha = .3) +
    geom_quasirandom(data = subset(oneyeardat, scandate_epoch > as.numeric(Sys.Date())-60 & scandate_epoch < max(oneyeardat$scandate_epoch, na.rm = TRUE)),
                     width = .2, size = 2) +
    geom_point(data = subset(oneyeardat_mostrecent, z_value_smooth60 <=2 & z_value_smooth60 >=-2 ),
               color = dot_outline_green, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
    geom_point(data = subset(oneyeardat_mostrecent, z_value_smooth60 >2 | z_value_smooth60 < -2),
               color = dot_outline_red, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +

    scale_alpha_continuous(range = c(.2,1), guide = 'none') +
    labs(x = '', y = 'Z Scale', title = sprintf('fBIRN QA, %s to %s\nShaded 1 & 2 SD ',Sys.Date()-60,Sys.Date())) +
    theme(axis.text.x = element_text(angle = 90, size = 14, vjust = .5))
  if(dosave){ggsave(file.path(figdir,sprintf('60day_dotplot_sd365%s.png',suffix)),width = min(6,round(num_measures*2/3)), height = 5, dpi = 200)}

  #zscore facet lineplot 60 days, 1&2sd calced on 365 to present
  ggplot(subset(oneyeardat, scandate_epoch > as.numeric(Sys.Date())-60), aes(x = scandate_epoch, y = z_value_smooth60, alpha = scandate_epoch)) +
    theme_bw() +
    annotate('rect',xmin = as.numeric(Sys.Date()-60), xmax = as.numeric(Sys.Date()), ymin = -2, ymax = 2, fill = '#98B6FA', alpha = .4) +
    annotate('rect',xmin = as.numeric(Sys.Date()-60), xmax = as.numeric(Sys.Date()), ymin = -1, ymax = 1, fill = '#98B6FA', alpha = .3) +
    geom_point(color = 'black', size = 1) +
    geom_line(color = 'black', size = 1) +
    geom_point(data = oneyeardat_mostrecent,
               color = 'red', fill = 'black', size = 2, shape = 21, stroke = 1 ) +
    scale_alpha_continuous(range = c(.2,1), guide = 'none') +
    labs(x = '', y = 'Z Scale', title = sprintf('fBIRN QA, %s to %s\nShaded 1 & 2 SD ',Sys.Date()-60,Sys.Date())) +
    facet_wrap(.~measure, ncol = numcol) +
    scale_x_continuous(breaks = oneyeardat$scandate_epoch[oneyeardat$scandate_epoch > as.numeric(Sys.Date()-60)],
                       labels = format(as.Date(oneyeardat$scandate_epoch[oneyeardat$scandate_epoch > as.numeric(Sys.Date()-60)], origin = '1970-01-01'), format = '%m-%d')) +
        theme(axis.text.x = element_text(angle = 90, size = 12, vjust = .5))
  if(dosave){ ggsave(file.path(figdir,sprintf('60day_lineplot_sd365%s.png',suffix)),width = numcol*2, height = numrow*2+1, dpi = 200)}

  for(thismeasure in current_measures){
  #1&2 SD, 60 day smooth, individual figures, post first jandate
    measuredat <- subset(longreport, measure == thismeasure & scandate_epoch > as.numeric(as.Date(jandates[1],format = '%m%d%y')))
  ggplot(measuredat, aes(x = scandate_epoch, y = value)) +
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
      geom_point(data = subset(measuredat, value < value_smooth60-2*value_smooth_sd60 | value > value_smooth60+2*value_smooth_sd60), color = 'red') +
      theme(axis.text.x = element_text(angle = 30))

    if(dosave){ggsave(file.path(figdir,sprintf('measure_%s_1-60_2-60.png',thismeasure)),width = 6, height = 4, dpi = 200)}

  }


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
  if(dosave){ggsave(file.path(figdir,sprintf('measures_1-365_1-60%s.png',suffix)),width = figwidth, height = figheight, dpi = 200)}


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

  if(dosave){ggsave(file.path(figdir,sprintf('measures_1-60_2-60%s.png',suffix)),width = figwidth, height = figheight, dpi = 200)}

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

  if(dosave){ggsave(file.path(figdir,sprintf('measures_1-365_2-365%s.png',suffix)),width = figwidth, height = figheight, dpi = 200)}




}

