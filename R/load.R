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

readQAMeasures <- function(basedir, analysisdir, measures, scans, fixfoldernames = TRUE){
  qa_dirs <- listQADirs(basedir,analysisdir,scans)
  qa_measures <- initializeQAdf(measures) #data frame: cols folder,scandate,measures[..]
  for (qa in qa_dirs)
  {
    qa_xml_file <- file.path(basedir,analysisdir,qa,'summaryQA.xml')
    if(!file.exists(qa_xml_file))
    {
      warning(sprintf('Warning: no summaryQA.xml file found in %s',qa_xml_file))
      qa_measures[dim(qa_measures)[1]+1,] = c(qa,NA,NA,NA,NA,NA,NA)
    }else
    {
      qa_measures[dim(qa_measures)[1]+1,] = NA
      rr <- dim(qa_measures[1])
      qa_measures[rr,'folder'] <- qa
      qa_measures[rr,'scandate'] <- readMeasure('scandate',qa_xml_file)
      for (mm in measures)
      {
        qa_measures[rr,mm] <- as.numeric(readMeasure(mm,qa_xml_file)[1])
      }
    }
  }
  if(fixfoldernames){qa_measures <- fixQAfoldernames(qa_measures)}

  #get epochs from foldernames and scandates
  qa_measures$folder_date <- do.call('c', lapply(qa_measures$folder, function(x){as.Date(gsub('QC_','',x), format = '%m%d%y')}))
  qa_measures$folder_epoch <- as.numeric(qa_measures$folder_date) #days since epoch start
  qa_measures$scandate_epoch <- as.numeric(as.Date(qa_measures$scandate)) #days since epoch start
  qa_measures$epoch_delta <- qa_measures$scandate_epoch - qa_measures$folder_epoch

  return(qa_measures)
}

listQADirs <- function(basedir,analysisdir,scans){
  qa_dirs <- dir(file.path(basedir,analysisdir))
  qa_dirs <- qa_dirs[grep('QC_',qa_dirs)]
  qa_dirs <- qa_dirs[grep('_fBIRN',qa_dirs,invert = TRUE)]
  if (scans != 'all'){warning('selecting subset of scans for report not enabled yet.')}
  return(qa_dirs)
}

initializeQAdf <- function(measures){
  qa_measures <- data.frame(folder = as.character(),
                            scandate = as.character(),
                            stringsAsFactors = FALSE)
  qa_measures <- data.frame(matrix(nrow = 0, ncol = 2+length(measures)))
  colnames(qa_measures) <- c('folder','scandate',measures)
  return(qa_measures)
}


