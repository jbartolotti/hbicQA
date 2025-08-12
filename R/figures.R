
FIGURES.makeFigures <- function(phantoms, longreport, figdir, config, dosave = TRUE, service_reports = NA){

  core_measures = c("percentFluc","drift","driftfit","SNR","SFNR","rdc")
    corereport <- list()
  for(p in phantoms){
    longreport[[p$name]]$measure <- as.character(longreport[[p$name]]$measure)
    corereport[[p$name]] <- subset(longreport[[p$name]], measure %in% core_measures)
  }
  #all measures
  FIGURES.makeFigures_selectedMeasures(phantoms, longreport,'_allmeasures',figdir, config, dosave = dosave, service_reports = service_reports)

  #core measures
  FIGURES.makeFigures_selectedMeasures(phantoms, corereport,'_coremeasures',figdir, config, figwidth = 12, figheight = 6, dosave = dosave, service_reports = service_reports)
}




FIGURES.makeFigures_selectedMeasures <- function(phantoms, thisreport, suffix, figdir, config, figwidth = 30, figheight = 'calc', dosave = TRUE, service_reports = NA){
  # Get the dates of all january1 since 2017
  # Get the dates of all april, july, october1 since july2016 until today
  years <- seq(17,as.integer(format(Sys.Date(), "%y")))
  jandates <- paste('0101',years, sep = '')
  dates <- unlist(lapply(c('0401','0701','1001'),function(x){paste(x,c('16',years), sep = '')}))
  dates <- dates[dates != '040116']
  dates <- dates[as.numeric(as.Date(dates, format = '%m%d%y')) < as.integer(Sys.Date())]


  current_measures <- as.character()
  oneyeardat <- list()
  oneyeardat_mostrecent <- list()
  measure_oneyear <- list()

  #Get list of measures being used, and create one-year back datasets
  for(p in phantoms){
    current_measures <- unique(c(current_measures, unique(thisreport[[p$name]]$measure)))

    isoneyear <- thisreport[[p$name]]$scandate_epoch > as.numeric(Sys.Date())-365
    if(any(isoneyear)){

    oneyeardat[[p$name]] <- subset(thisreport[[p$name]], scandate_epoch > as.numeric(Sys.Date())-365)
    oneyeardat[[p$name]]$measure_phantom <- paste(oneyeardat[[p$name]]$measure, p$name, sep = '_')
    oneyeardat[[p$name]]$phantom <- p$name
    oneyeardat[[p$name]]$z_value_365 <- unlist(lapply(1:dim(oneyeardat[[p$name]])[1], function(x){
      (oneyeardat[[p$name]]$value[x] - mean(oneyeardat[[p$name]]$value[oneyeardat[[p$name]]$measure == oneyeardat[[p$name]]$measure[x]],na.rm = TRUE)) / sd(oneyeardat[[p$name]]$value[oneyeardat[[p$name]]$measure == oneyeardat[[p$name]]$measure[x]],na.rm = TRUE)
    }))
    oneyeardat_mostrecent[[p$name]] <- oneyeardat[[p$name]][oneyeardat[[p$name]]$scandate_epoch == max(oneyeardat[[p$name]]$scandate_epoch, na.rm = TRUE),]
    } else {
      oneyeardat[[p$name]] <- NA
    }
  }

  #Figure settings
  num_measures <- length(current_measures)
  shadegreen <- '#009933'
  linegreen <- '#33ff33'
  shadeblue <- '#0033cc'
  lineblue <- '#33ccff'
  darklineblue = 'blue'
  shadeorange <- '#663300'
  lineorange <- '#FF9933'
  darklineorange = '#E89611'

  dot_outline_green <- '#3CA61C'
  dot_outline_red <- '#E12A1B'
  dot_outline_lime <- '#00FF00'
  dot_outline_pink <- '#FF00FF'
  dot_outline_blue <- 'blue'

  dot_outline_good_bullet <- dot_outline_green
  dot_outline_bad_bullet <- dot_outline_red
  dot_outline_good_fbirn <- dot_outline_green #dot_outline_lime
  dot_outline_bad_fbirn <- dot_outline_red #dot_outline_pink
  dot_outline_excess_bullet <- dot_outline_blue
  dot_outline_excess_fbirn <- dot_outline_blue


  service_colors <- list(
    'cooling' = '#3399FF',
    'coil' = '#CCCC00',
    'gradient' = '#009933',
    'spectroscopy' = '#FF0033'
    )

  yextents <- list(
    drift = c(0,4),
    driftfit = c(0,3),
    SNR = c(100,400),
    SFNR = c(100,400),
    percentFluc = c(0,.25),
    rdc = c(0,15)

    )

  gradient_dates <- c('2022-08-19', '2023-11-25', '2025-03-20','2025-07-23')
  gradient_epochs <- as.numeric(as.Date(gradient_dates))


  #3/2 ratio for figure size.
  if(figheight == 'calc'){figheight = figwidth*2/3}
  numcol <- round(sqrt(num_measures))+1
  numrow <- ceiling(num_measures/numcol)


  # Single phantom plots
  mycolors = c('#E89611','blue')

  if(any('bullet' %in% names(phantoms))){
    phantoms$bullet$shade = shadeorange
    phantoms$bullet$line = lineorange
    phantoms$bullet$darkline = darklineorange
    phantoms$bullet$dot_outline_good = dot_outline_good_bullet
    phantoms$bullet$dot_outline_bad = dot_outline_bad_bullet
    phantoms$bullet$dot_outline_excess = dot_outline_excess_bullet

  }
  if(any('fbirn' %in% names(phantoms))){
  phantoms$fbirn$shade = shadeblue
  phantoms$fbirn$line = lineblue
  phantoms$fbirn$darkline = darklineblue
  phantoms$fbirn$dot_outline_good = dot_outline_good_fbirn
  phantoms$fbirn$dot_outline_bad = dot_outline_bad_fbirn
  phantoms$fbirn$dot_outline_excess = dot_outline_excess_fbirn
  }

  index = 0
  for(p in phantoms){
    index = index+1
    if(is.data.frame(oneyeardat[[p$name]])){
      FIGURES.zscoredotplot(subset(oneyeardat[[p$name]], scandate_epoch > as.numeric(Sys.Date())-60),
                            oneyeardat_mostrecent[[p$name]],
                            figdir, suffix, p$name, dosave, num_measures, p$dot_outline_good, p$dot_outline_bad, p$dot_outline_excess
                            )
      FIGURES.zscorelineplot(subset(oneyeardat[[p$name]], scandate_epoch > as.numeric(Sys.Date())-60),
                            oneyeardat_mostrecent[[p$name]],
                            figdir, suffix, p$name, dosave, num_measures, mycolors[index], numcol, numrow
      )
    }
  }

  for(p in phantoms){

    for(thismeasure in current_measures){
      #1&2 SD, 60 day smooth, individual figures, post first jandate
      measuredat <- subset(thisreport[[p$name]], measure == thismeasure & scandate_epoch > as.numeric(as.Date(jandates[1],format = '%m%d%y')))
      h <- ggplot2::ggplot(measuredat, ggplot2::aes(x = scandate_epoch, y = value)) +
        ggplot2::theme_bw() +
        ggplot2::geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
        ggplot2::geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
        ggplot2::scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
        ggplot2::labs(x = '',title = sprintf('%s. 60day smooth. Shaded = +/- 1&2 SD',thismeasure), y = thismeasure)

        if(!is.na(service_reports) && class(service_reports) == 'data.frame'){
          h <- h +
          ggplot2::geom_vline(xintercept = service_reports$epoch_day[service_reports$category_cooling != ''], color = service_colors$cooling) +
          ggplot2::geom_vline(xintercept = service_reports$epoch_day[service_reports$category_coil != ''], color = service_colors$coil) +
          ggplot2::geom_vline(xintercept = service_reports$epoch_day[service_reports$category_gradient != ''], color = service_colors$gradient) +
          ggplot2::geom_vline(xintercept = service_reports$epoch_day[service_reports$category_sepectroscopy != ''], color = service_colors$spectroscopy)


        }

        h <- h + ggplot2::geom_point() +
        ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth60, ymin = value_smooth60-2*value_smooth_sd60, ymax = value_smooth60+2*value_smooth_sd60),
                             fill = p$shade, alpha = .25) +
        ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth60, ymin = value_smooth60-value_smooth_sd60, ymax = value_smooth60+value_smooth_sd60),
                             fill = p$shade, alpha = .5) +
        ggplot2::geom_line(ggplot2::aes(y = value_smooth60),color = p$line) +
        ggplot2::geom_point(data = subset(measuredat, value < value_smooth60-2*value_smooth_sd60 | value > value_smooth60+2*value_smooth_sd60), color = 'red') +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
        h
      if(dosave){ggplot2::ggsave(file.path(figdir,sprintf('measure_%s_1-60_2-60_%s.png',thismeasure,p$name)),width = 6, height = 4, dpi = 200)}



    }

  }

  # Combined phantom plots
  if(all(c('bullet','fbirn') %in% names(phantoms))){

    if(phantoms$bullet$report$fig60 && phantoms$fbirn$report$fig60){
  dat <- rbind(subset(oneyeardat$bullet, scandate_epoch > as.numeric(Sys.Date())-60),
               subset(oneyeardat$fbirn, scandate_epoch > as.numeric(Sys.Date())-60))
  lastscan <- rbind(oneyeardat_mostrecent$bullet,oneyeardat_mostrecent$fbirn)
  FIGURES.zscoredotplot_both(dat, lastscan, figdir, suffix, c('bullet','fbirn'), dosave, num_measures, dot_outline_green, dot_outline_red, current_measures)
  FIGURES.zscorelineplot(dat, lastscan, figdir, suffix, c('bullet','fbirn'), dosave, num_measures, mycolors, numcol, numrow)

    }

  thisreport$bullet$phantom = 'bullet'
  thisreport$fbirn$phantom = 'fbirn'
  bothlong = rbind(thisreport$bullet, thisreport$fbirn)

  for(thismeasure in current_measures){
    #1&2 SD, 60 day smooth, individual figures, post first jandate
    measuredat <- subset(bothlong, measure == thismeasure & scandate_epoch > as.numeric(as.Date(config$historical_start)))
    oneyeardat_bullet <- subset(bothlong, measure == thismeasure & scandate_epoch > as.numeric(Sys.Date()-365) & phantom == 'bullet')
    oneyearmean_bullet <- mean(oneyeardat_bullet$value, na.rm = TRUE)
    oneyearsd_bullet <- sd(oneyeardat_bullet$value, na.rm = TRUE)

    oneyeardat_fbirn <- subset(bothlong, measure == thismeasure & scandate_epoch > as.numeric(Sys.Date()-365) & phantom == 'fbirn')
    oneyearmean_fbirn <- mean(oneyeardat_fbirn$value, na.rm = TRUE)
    oneyearsd_fbirn <- sd(oneyeardat_fbirn$value, na.rm = TRUE)

    h <- ggplot2::ggplot(measuredat, ggplot2::aes(x = scandate_epoch, y = value, color = phantom)) +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
      ggplot2::geom_vline(xintercept = gradient_epochs, color = '#FFAAAA') +

      ggplot2::scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
      ggplot2::labs(x = '',title = sprintf('%s. Line: 60day smooth',thismeasure), y = thismeasure) +
#      ggplot2::geom_ribbon(data = subset(measuredat, phantom == 'fbirn' & scandate_epoch > as.numeric(as.Date('2023-01-01'))), ggplot2::aes(y = value_smooth60, ymin = value_smooth60-2*value_smooth_sd365, ymax = value_smooth60+2*value_smooth_sd365),
#                           fill = '#AAAAAA', alpha = .5, color = '#AAAAAA') +
      ggplot2::geom_point(data = subset(measuredat, color == 'auto'), alpha = .7) +
      ggplot2::geom_line(ggplot2::aes(y = value_smooth60), size = 1) +
#      ggplot2::geom_point(data = subset(measuredat, phantom == 'fbirn' & ((value-oneyearmean_fbirn)/oneyearsd_fbirn < -2 | (value-oneyearmean_fbirn)/oneyearsd_fbirn > 2)), color = 'red', shape = 4, size = 3) +
#      ggplot2::geom_point(data = subset(measuredat, phantom == 'bullet' & ((value-oneyearmean_bullet)/oneyearsd_bullet < -2 | (value-oneyearmean_bullet)/oneyearsd_bullet > 2)), color = 'red', shape = 4, size = 3) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30)) +
      ggplot2::scale_color_manual(values = c(phantoms$bullet$line, phantoms$fbirn$line), guide = 'none')+
      ggplot2::coord_cartesian(xlim = c(as.numeric(as.Date(config$historical_start)),NA))

    for(mycolor in unique(measuredat$color)){
      if (mycolor != 'auto'){
        h <- h + ggplot2::geom_point(data = subset(measuredat, color == mycolor), color = mycolor )
      }
    }
    h


    if(dosave){ggplot2::ggsave(file.path(figdir,sprintf('measure_%s_1-60_2-60_bullet_fbirn.png',thismeasure)),width = 6, height = 4, dpi = 200)}

    imfile <- system.file('extdata',sprintf('kayvanrad_2021_%s_smh.png',thismeasure), package = 'hbicQA')

    if (file.exists(imfile)){
        im <- png::readPNG(imfile)
        #Compare to other sites
        measuredat <- subset(bothlong, measure == thismeasure & scandate_epoch > as.numeric(as.Date(jandates[1],format = '%m%d%y')))
        ggplot2::ggplot(measuredat, ggplot2::aes(x = scandate_epoch, y = value, color = phantom)) +
        ggplot2::theme_bw() +
        ggplot2::annotation_raster(im, ymin = yextents[[thismeasure]][1], ymax = yextents[[thismeasure]][2], xmin = as.numeric(as.Date(jandates[1],format = '%m%d%y')), xmax = as.numeric(Sys.Date())) +
        ggplot2::geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
        ggplot2::geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
        ggplot2::scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
        ggplot2::labs(x = '',title = sprintf('%s. 60day smooth, compared to 13 sites',thismeasure) , y = thismeasure) +
        ggplot2::geom_point(ggplot2::aes(fill = phantom), color = 'black', shape = 21) +
        ggplot2::geom_line(ggplot2::aes(y = value_smooth60)) +
        ggplot2::scale_color_manual(values = c(phantoms$bullet$line, phantoms$fbirn$line), guide = 'none') +
        ggplot2::scale_fill_manual(values = c(phantoms$bullet$line, phantoms$fbirn$line), guide = 'none') +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30)) +
        ggplot2::coord_cartesian(xlim = c(as.numeric(as.Date(jandates[1],format = '%m%d%y')), as.numeric(Sys.Date())),ylim = yextents[[thismeasure]], expand = FALSE)
      if(dosave){ggplot2::ggsave(file.path(figdir,sprintf('measure_%s_1-60_2-60_bullet_fbirn_sites.png',thismeasure)),width = 6, height = 4, dpi = 200)}
    }
  }
  }



}

oldfigures = function(){
    #1SD, 60&365
    ggplot2::ggplot(thisreport, ggplot2::aes(x = scandate_epoch, y = value)) +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
      ggplot2::scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
      ggplot2::labs(x = '',title = 'Green: 1yr smooth. Blue: 60day smooth. Shaded = +/-1SD') +
      ggplot2::geom_point() +
      ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth365, ymin = value_smooth365-value_smooth_sd365, ymax = value_smooth365+value_smooth_sd365),
                fill = shadegreen, alpha = .5) +
      ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth60, ymin = value_smooth60-value_smooth_sd60, ymax = value_smooth60+value_smooth_sd60),
                fill = shadeblue, alpha = .5) +
      ggplot2::geom_line(ggplot2::aes(y = value_smooth365),color = linegreen) +
      ggplot2::geom_line(ggplot2::aes(y = value_smooth60),color = lineblue) +
      ggplot2::facet_wrap(.~measure,scales = 'free') +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))
    if(dosave){ggplot2::ggsave(file.path(figdir,sprintf('measures_1-365_1-60%s.png',suffix)),width = figwidth, height = figheight, dpi = 200)}


  #1&2 SD, 60
    ggplot2::ggplot(thisreport, ggplot2::aes(x = scandate_epoch, y = value)) +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
      ggplot2::scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
      ggplot2::labs(x = '',title = 'Blue: 60day smooth. Shaded = +/- 1&2 SD') +
      ggplot2::geom_point() +
      ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth60, ymin = value_smooth60-2*value_smooth_sd60, ymax = value_smooth60+2*value_smooth_sd60),
                  fill = shadeblue, alpha = .25) +
      ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth60, ymin = value_smooth60-value_smooth_sd60, ymax = value_smooth60+value_smooth_sd60),
                  fill = shadeblue, alpha = .5) +
      ggplot2::geom_line(ggplot2::aes(y = value_smooth60),color = lineblue) +
      ggplot2::geom_point(data = subset(thisreport, value < value_smooth60-2*value_smooth_sd60 | value > value_smooth60+2*value_smooth_sd60), color = 'red') +
      ggplot2::facet_wrap(.~measure,scales = 'free')+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

    if(dosave){ggplot2::ggsave(file.path(figdir,sprintf('measures_1-60_2-60%s.png',suffix)),width = figwidth, height = figheight, dpi = 200)}

    #1&2 SD, 365
    ggplot2::ggplot(thisreport, ggplot2::aes(x = scandate_epoch, y = value)) +
      ggplot2::theme_bw() +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(jandates,format = '%m%d%y')), color = 'black') +
      ggplot2::geom_vline(xintercept = as.numeric(as.Date(dates,format = '%m%d%y')), color = '#CCCCCC') +
      ggplot2::scale_x_continuous(breaks = as.numeric(as.Date(jandates,format = '%m%d%y')), labels = as.character(as.Date(jandates, format = '%m%d%y'))) +
      ggplot2::labs(x = '',title = 'Green: 1yr smooth. Shaded = +/- 1&2 SD') +
      ggplot2::geom_point() +
      ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth365, ymin = value_smooth365-2*value_smooth_sd365, ymax = value_smooth365+2*value_smooth_sd365),
                  fill = shadegreen, alpha = .25) +
      ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth365, ymin = value_smooth365-value_smooth_sd365, ymax = value_smooth365+value_smooth_sd365),
                  fill = shadegreen, alpha = .5) +
      ggplot2::geom_line(ggplot2::aes(y = value_smooth365),color = linegreen) +
      ggplot2::geom_point(data = subset(thisreport, value < value_smooth365-2*value_smooth_sd365 | value > value_smooth365+2*value_smooth_sd365), color = 'red') +
      ggplot2::facet_wrap(.~measure,scales = 'free')+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30))

    if(dosave){ggplot2::ggsave(file.path(figdir,sprintf('measures_1-365_2-365%s.png',suffix)),width = figwidth, height = figheight, dpi = 200)}

  }






FIGURES.zscoredotplot <- function(dat, lastscan, figdir, suffix, phantom_name, dosave, num_measures, dot_outline_good, dot_outline_bad, dot_outline_excess){
  #zscore dotplot 60 days, 1&2sd calced on 365 to present
  ggplot2::ggplot(dat, ggplot2::aes(x = measure, y = z_value_365, alpha = scandate_epoch)) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete() +
    ggplot2::annotate('rect',xmin = 0, xmax = num_measures+1, ymin = -2, ymax = 2, fill = '#98B6FA', alpha = .4) +
    ggplot2::annotate('rect',xmin = 0, xmax = num_measures+1, ymin = -1, ymax = 1, fill = '#98B6FA', alpha = .3) +
    ggbeeswarm::geom_quasirandom(width = .2, size = 2) +
    # ggbeeswarm::geom_quasirandom(data = subset(oneyeardat, scandate_epoch > as.numeric(Sys.Date())-60 & scandate_epoch < max(oneyeardat$scandate_epoch, na.rm = TRUE)),
    #                             width = .2, size = 2) +
    ggplot2::geom_point(data = subset(lastscan, z_value_365 <=2 & z_value_365 >=-2 ),
                        color = dot_outline_good, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
    ggplot2::geom_point(data = subset(lastscan, z_value_365 >2 & measure %in% c('drift','driftfit','percentFluc')),
                        color = dot_outline_bad, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
    ggplot2::geom_point(data = subset(lastscan, z_value_365 < -2 & measure %in% c('rdc','SFNR','SNR')),
                        color = dot_outline_bad, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
    ggplot2::geom_point(data = subset(lastscan, z_value_365 < -2 & measure %in% c('drift','driftfit','percentFluc')),
                        color = dot_outline_excess, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
    ggplot2::geom_point(data = subset(lastscan, z_value_365 > 2 & measure %in% c('rdc','SFNR','SNR')),
                        color = dot_outline_excess, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +

    ggplot2::scale_alpha_continuous(range = c(.2,1), guide = 'none') +
    ggplot2::labs(x = '', y = 'Z Scale', title = sprintf('fBIRN QA, %s to %s\nShaded 1 & 2 SD',Sys.Date()-60,Sys.Date())) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 14, vjust = .5))
  if(dosave){
    ggplot2::ggsave(file.path(figdir,sprintf('60day_dotplot_sd365%s_%s.png',suffix,phantom_name)),width = min(6,round(num_measures*2/3)), height = 5, dpi = 200)
  }

}

FIGURES.zscoredotplot_both <- function(dat, lastscan, figdir, suffix, phantom_names, dosave, num_measures, dot_outline_good, dot_outline_bad, current_measures){

mylabs = rep('',num_measures*2)
mylabs[seq(1,num_measures*2,2)] = sort(current_measures)

ggplot2::ggplot(dat, ggplot2::aes(x = measure_phantom, y = z_value_365, alpha = scandate_epoch, shape = phantom)) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_discrete(labels = mylabs) + #breaks = seq(1,11,2)) +
  ggplot2::annotate('rect',xmin = 0, xmax = (num_measures*2)+1, ymin = -2, ymax = 2, fill = '#98B6FA', alpha = .4) +
  ggplot2::annotate('rect',xmin = 0, xmax = (num_measures*2)+1, ymin = -1, ymax = 1, fill = '#98B6FA', alpha = .3) +
  ggbeeswarm::geom_quasirandom(width = .2, size = 2) +
  # ggbeeswarm::geom_quasirandom(data = subset(oneyeardat, scandate_epoch > as.numeric(Sys.Date())-60 & scandate_epoch < max(oneyeardat$scandate_epoch, na.rm = TRUE)),
  #                             width = .2, size = 2) +
  ggplot2::geom_point(data = subset(lastscan, z_value_365 <=2 & z_value_365 >=-2 & phantom == phantom_names[1]),
                      color = dot_outline_good, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
  ggplot2::geom_point(data = subset(lastscan, z_value_365 >2 | z_value_365 < -2 & phantom == phantom_names[1]),
                      color = dot_outline_bad, fill = 'black', size = 3, shape = 21, stroke = 1.5 ) +
  ggplot2::geom_point(data = subset(lastscan, z_value_365 <=2 & z_value_365 >=-2 & phantom == phantom_names[2]),
                      color = dot_outline_good, fill = 'black', size = 3, shape = 24, stroke = 1.5 ) +
  ggplot2::geom_point(data = subset(lastscan, z_value_365 >2 | z_value_365 < -2 & phantom == phantom_names[2]),
                      color = dot_outline_bad, fill = 'black', size = 3, shape = 24, stroke = 1.5 ) +
  ggplot2::geom_vline(xintercept = seq(.5,(num_measures*2)+.5,2)) +
  #   ggplot2::coord_cartesian(xlim = c(.5, num_measures*2+.5)), expand = FALSE)

  ggplot2::scale_alpha_continuous(range = c(.2,1), guide = 'none') +
  ggplot2::scale_shape_discrete(guide = 'none') +
  ggplot2::labs(x = '', y = 'Z Scale', title = sprintf('fBIRN QA, %s to %s\nShaded 1 & 2 SD \nPhantom: %s-● %s-▲',Sys.Date()-60,Sys.Date(), phantom_names[1], phantom_names[2])) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 14, vjust = 1, hjust = 1))
  if(dosave){
    ggplot2::ggsave(file.path(figdir,sprintf('60day_dotplot_sd365%s_%s_%s.png',suffix,phantom_names[1], phantom_names[2])),width = min(6,round(num_measures*2/3)), height = 5, dpi = 200)
}


}

FIGURES.zscorelineplot <- function(dat, lastscan, figdir, suffix, phantom_name, dosave, num_measures, mycolors, numcol, numrow){
  good_green <- '#3CA61C'
  bad_red <- '#E12A1B'
  excess_blue <- 'blue'

#zscore facet lineplot 60 days, 1&2sd calced on 365 to present
ggplot2::ggplot(dat, ggplot2::aes(x = scandate_epoch, y = z_value_365, alpha = scandate_epoch, color = phantom)) +
  ggplot2::theme_bw() +
  ggplot2::annotate('rect',xmin = as.numeric(Sys.Date()-60), xmax = as.numeric(Sys.Date()), ymin = -2, ymax = 2, fill = '#98B6FA', alpha = .4) +
  ggplot2::annotate('rect',xmin = as.numeric(Sys.Date()-60), xmax = as.numeric(Sys.Date()), ymin = -1, ymax = 1, fill = '#98B6FA', alpha = .3) +
  #ggplot2::geom_point(size = 1) + #color = 'black', size = 1) +
  ggplot2::geom_line(linewidth = 1) + #color = 'black', size = 1) +
  ggplot2::geom_point(data = subset(dat, z_value_365 <=2 & z_value_365 >= -2 ), size = 2, color = good_green) +
  ggplot2::geom_point(data = subset(dat, z_value_365 >2 & measure %in% c('drift','driftfit','percentFluc') ), size = 2, color = bad_red) +
  ggplot2::geom_point(data = subset(dat, z_value_365 < -2 & measure %in% c('drift','driftfit','percentFluc') ), size = 2, color = excess_blue) +
  ggplot2::geom_point(data = subset(dat, z_value_365 < -2 & measure %in% c('rdc','SFNR','SNR') ), size = 2, color = bad_red) +
  ggplot2::geom_point(data = subset(dat, z_value_365 > 2 & measure %in% c('rdc','SFNR','SNR') ), size = 2, color = excess_blue) +

#  ggplot2::geom_point(data = subset(lastscan, z_value_365 <=2 & z_value_365 >= -2 ), ggplot2::aes(fill = good_green),
#                       color = 'black', size = 2, shape = 21, stroke = 1 ) +
#  ggplot2::geom_point(data = subset(lastscan, z_value_365 >2 & z_value_365 < -2 ), ggplot2::aes(fill = bad_red),
#                      color = 'black', size = 2, shape = 21, stroke = 1 ) +
  ggplot2::scale_alpha_continuous(range = c(.2,1), guide = 'none') +
  ggplot2::labs(x = '', y = 'Z Scale', title = sprintf('fBIRN QA, %s to %s\nShaded 1 & 2 SD ',Sys.Date()-60,Sys.Date())) +
  ggplot2::facet_wrap(.~measure, ncol = numcol) +
  ggplot2::scale_color_manual(values = mycolors, guide = 'none') +
  ggplot2::scale_fill_manual(values = mycolors, guide = 'none') +
  ggplot2::scale_x_continuous(breaks = dat$scandate_epoch,
                              labels = format(as.Date(dat$scandate_epoch, origin = '1970-01-01'), format = '%m-%d')) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, vjust = .5))
if(dosave){ ggplot2::ggsave(file.path(figdir,sprintf('60day_lineplot_sd365%s_%s.png',suffix,paste(phantom_name,collapse = '_'))),width = numcol*2, height = numrow*2+1, dpi = 200)}

}
