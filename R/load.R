move_qc <- function(date, savedir, rawdir, overwrite = FALSE){
  datestr <- sprintf('%06d',date)
  rawfile <- file.path(rawdir,sprintf('qc_%s',datestr))
  savefile <- file.path(savedir,sprintf('qc_%s',datestr))

  do_copy <- TRUE
  copy_succeed <- FALSE
  already_existed <- FALSE
  if (!file.exists(rawfile))
  {stop(sprintf('ERROR: %s does not exist. Date should be format MMDDYY.',rawfile))}
  if (file.exists(savefile))
  {
    already_existed <- TRUE
    do_copy <- overwrite
    if (!overwrite) {
      warning(sprintf('Target %s already exists; skipping copy. Set overwrite = TRUE to force copy.',savefile))
    }
  }
  if(do_copy)
  {
    dir.create(savefile, showWarnings = FALSE)
    #yes, copy to savedir. Otherwise it will make another new folder inside the savefile folder
    copy_succeed <- file.copy(from = rawfile, to = savedir, recursive = TRUE)
    if (!copy_succeed){stop(sprintf('copy from %s to %s failed. Do you have write permissions?',rawfile,savefile))}
  }

  notes <- list()
  notes$target_exists <- file.exists(savefile)
  notes$did_copy <- copy_succeed
  mymessage <- as.character()
  if (notes$target_exists) {
    mymessage <- sprintf('proceeding with file %s.\n',savefile)
    if(already_existed){
      if(overwrite){was_or_not <- '*WAS*'} else{was_or_not = 'was *NOT*'}
      mymessage <- c(mymessage,sprintf('  file already existed and %s overwritten',was_or_not))
    }
  }else{stop(sprintf('FAIL: %s does not exist or is not readable.', savefile))}
  notes$message <- mymessage

  return(notes)
}
