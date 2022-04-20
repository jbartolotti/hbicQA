runfBIRN <- function(date, indir4, outdir4){
  datestr <-  sprintf('%06d',date)
  already_processed <- FALSE

  if(file.exists(outdir4) && length(dir(outdir4, all.files=TRUE,no.. = TRUE)) > 0)
  {
    already_processed <- TRUE
    warning(sprintf('Directory %s is non-empty; skipping fmriqa_phantomqa.pl.',outdir4))
  } else {
    system2('dicom2bxh', args = c(
      '--xcede',
      file.path(indir4,'*.dcm'),
      file.path(indir4,sprintf('QC_%s_WRAPPED.xml',datestr))
    ), wait= TRUE
    )
    system2('fmriqa_phantomqa.pl', args = c(
      file.path(indir4, sprintf('QC_%s_WRAPPED.xml',datestr)),
      outdir4
    ), wait = TRUE
    )

  }
  notes <- list()
  mymessage <- as.character()
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
getTolerances <- function(report){
  if(class(report) == 'character'){
    reportdat <- read.csv(report)
  }
  longreport <- melt(setDT(reportdat),
                     id.vars = c('folder','scandate','folder_date','folder_epoch',"scandate_epoch",'epoch_delta'),
                     variable.name = "measure")
  longreport <- as.data.frame(longreport)
  dayrange <- 30
  mycol <- sprintf('value_smooth%s',dayrange)
  longreport[,mycol] <- NA
  #colnames(longreport)[colnames(longreport) == 'temp'] = sprintf('value_smooth%s',dayrange)
  for(mes in unique(longreport$measure)){
    for(i in which(longreport$measure == mes)){
      thisepoch <- longreport$scandate_epoch[i]
      myrows <- which(longreport$measure == mes &
                        !is.na(longreport$scandate_epoch) &
                        longreport$scandate_epoch > thisepoch - dayrange &
                        longreport$scandate_epoch < thisepoch + dayrange)
      longreport[i,mycol] <- mean(longreport$value[myrows],na.rm = TRUE)
    }

  }



ggplot(longreport, aes(x = scandate_epoch, y = value)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(y = value_smooth30),color = 'red') +
  facet_wrap(.~measure,scales = 'free')
ggsave('measures.png',width = 30, height = 20)
}
