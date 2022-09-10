runfBIRN <- function(date, indir4, outdir4, tempdir = NA){
  notes <- list()
  mymessage <- as.character()
  datestr <-  sprintf('%06d',date)
  pwd <- getwd()
  already_processed <- FALSE
  if(!is.na(tempdir)){
    setwd(indir4)
    system2('tar', args = c(
      '-zcf',
      'DICOM.tar.gz',
      'DICOM'
      ), wait = TRUE
      )
    system2('mv', args = c(
      'DICOM.tar.gz',
      file.path(tempdir,'DICOM.tar.gz')
    ), wait = TRUE
    )
    system2('gunzip', args = c(
      file.path(tempdir,'DICOM.tar.gz')
    ), wait = TRUE
    )


    mymessage <- c(mymessage,sprintf('Temporary directory %s created.\nContents of %s copied to %s.',tempdir,indir4,tempdir))
    datadir <- tempdir
    } else {datadir <- indir4}
  if(file.exists(outdir4) && length(dir(outdir4, all.files=TRUE,no.. = TRUE)) > 0)
  {
    already_processed <- TRUE
    warning(sprintf('Directory %s is non-empty; skipping fmriqa_phantomqa.pl.',outdir4))
  } else {
    system2('dicom2bxh', args = c(
      '--xcede',
      file.path(datadir,'*.dcm'),
      file.path(datadir,sprintf('QC_%s_WRAPPED.xml',datestr))
    ), wait= TRUE
    )
    system2('fmriqa_phantomqa.pl', args = c(
      file.path(datadir, sprintf('QC_%s_WRAPPED.xml',datestr)),
      outdir4
    ), wait = TRUE
    )
#    if(!is.na(tempdir)){
#      system2('rm', args = c(
#        '-r ',
#        tempdir
#      ), wait = TRUE
#      )
#      mymessage <- c(mymessage,sprintf('Removing temporary files in %s',tempdir))
#    }

  }
  if(file.exists(outdir4) && length(dir(outdir4, all.files=TRUE,no.. = TRUE)) > 0){

    mymessage <- sprintf('fBIRN complete. Proceeding with file %s\n',outdir4)
    if(already_processed){
      mymessage <- c(mymessage,'NB: data already existed and was not reprocessed.')
    }
  } else{stop(sprintf('FAIL: %s does not exist or is not readable.', outdir4))}

  notes$message <- mymessage
  return(notes)
}

windir <- '//kumc.edu/data/Research/Hoglund/Bartolotti_J/QA'
getTolerances <- function(report, dofigures = FALSE, remove_duplicate_rows = TRUE){
  if(class(report) == 'character'){
    report <- read.csv(report)
  }
  longreport <- reshape2::melt(data.table::setDT(report),
                     id.vars = c('folder','scandate','folder_date','folder_epoch',"scandate_epoch",'epoch_delta'),
                     variable.name = "measure",)
  longreport <- as.data.frame(longreport)
  dayrange <- 30
  mycol <- sprintf('value_smooth%s',dayrange)
  mycolsd <- sprintf('value_smooth_sd%s',dayrange)

  longreport[,mycol] <- NA
  longreport[,mycolsd] <- NA

  #colnames(longreport)[colnames(longreport) == 'temp'] = sprintf('value_smooth%s',dayrange)
  for(mes in unique(longreport$measure)){
    for(i in which(longreport$measure == mes)){
      thisepoch <- longreport$scandate_epoch[i]
      myrows <- which(longreport$measure == mes &
                        !is.na(longreport$scandate_epoch) &
                        longreport$scandate_epoch > thisepoch - dayrange &
                        longreport$scandate_epoch < thisepoch + dayrange)
      longreport[i,mycol] <- mean(longreport$value[myrows],na.rm = TRUE)
      longreport[i,mycolsd] <- sd(longreport$value[myrows],na.rm = TRUE)
    }

  }

  dayrange <- 60
  mycol <- sprintf('value_smooth%s',dayrange)
  mycolsd <- sprintf('value_smooth_sd%s',dayrange)

  longreport[,mycol] <- NA
  longreport[,mycolsd] <- NA

  #colnames(longreport)[colnames(longreport) == 'temp'] = sprintf('value_smooth%s',dayrange)
  for(mes in unique(longreport$measure)){
    for(i in which(longreport$measure == mes)){
      thisepoch <- longreport$scandate_epoch[i]
      myrows <- which(longreport$measure == mes &
                        !is.na(longreport$scandate_epoch) &
                        longreport$scandate_epoch > thisepoch - dayrange &
                        longreport$scandate_epoch < thisepoch + dayrange)
      longreport[i,mycol] <- mean(longreport$value[myrows],na.rm = TRUE)
      longreport[i,mycolsd] <- sd(longreport$value[myrows],na.rm = TRUE)
    }

  }

  dayrange <- 365
  mycol <- sprintf('value_smooth%s',dayrange)
  mycolsd <- sprintf('value_smooth_sd%s',dayrange)

  longreport[,mycol] <- NA
  longreport[,mycolsd] <- NA

  #colnames(longreport)[colnames(longreport) == 'temp'] = sprintf('value_smooth%s',dayrange)
  for(mes in unique(longreport$measure)){
    for(i in which(longreport$measure == mes)){
      thisepoch <- longreport$scandate_epoch[i]
      myrows <- which(longreport$measure == mes &
                        !is.na(longreport$scandate_epoch) &
                        longreport$scandate_epoch > thisepoch - dayrange &
                        longreport$scandate_epoch < thisepoch + dayrange)
      longreport[i,mycol] <- mean(longreport$value[myrows],na.rm = TRUE)
      longreport[i,mycolsd] <- sd(longreport$value[myrows],na.rm = TRUE)
    }

  }

if(remove_duplicate_rows){
  longreport <- longreport[!duplicated(longreport),]
}


if(dofigures){
  ggplot2::ggplot(longreport, ggplot2::aes(x = scandate_epoch, y = value)) +
    ggplot2::theme_bw() +
    ggplot2::geom_point() +
    ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth365, ymin = value_smooth365-2*value_smooth_sd365, ymax = value_smooth365+2*value_smooth_sd365),
                fill = '#009933', alpha = .2) +
    ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth365, ymin = value_smooth365-value_smooth_sd365, ymax = value_smooth365+value_smooth_sd365),
                fill = '#009933', alpha = .5) +
    ggplot2::geom_ribbon(ggplot2::aes(y = value_smooth60, ymin = value_smooth60-value_smooth_sd60, ymax = value_smooth60+value_smooth_sd60),
                fill = '#0033cc', alpha = .5) +
  #  geom_ribbon(ggplot2::aes(y = value_smooth30, ymin = value_smooth30-value_smooth_sd30, ymax = value_smooth30+value_smooth_sd30), fill = 'red', alpha = .4) +
    ggplot2::geom_line(ggplot2::aes(y = value_smooth365),color = '#33ff33') +
    ggplot2::geom_line(ggplot2::aes(y = value_smooth60),color = '#33ccff') +
  #  geom_line(ggplot2::aes(y = value_smooth30),color = 'red') +

    ggplot2::facet_wrap(.~measure,scales = 'free')
  ggplot2::ggsave('measures.png',width = 30, height = 20)
}
return(longreport)
}
