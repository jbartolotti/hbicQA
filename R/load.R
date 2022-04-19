
findNewScans <- function(rawdir, targetdir){
  arcscan <- dir(rawdir)
  arcscan <- arcscan[grep('qc',arcscan)]
  imscan <- imscan[grep('zip',imscan,invert =TRUE)]
  imscan <- imscan[grep('qc',imscan)]

  uncopied <- arcscan[!(arcscan %in% imscan)]


  }




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

readQAMeasures <- function(basedir, analysisdir, measures, read_qa_measures = NA, scan_names='all',scans_after_epoch='all' , fixfoldernames = TRUE){
  qa_dirs <- listQADirs(basedir,analysisdir,
                        scan_names = scan_names,scans_after_epoch = scans_after_epoch,
                        fixfoldernames = fixfoldernames)
  #If all measures of interest are in the already completed report,
  #then only read directories that don't already exist in the already completed report
  if(!is.na(read_qa_measures) && all(measures %in% colnames(read_qa_measures))){
    already_read <- read_qa_measures$folder
    qa_dirs <- qa_dirs[!(qa_dirs %in% already_read)]
  }

  qa_measures <- initializeQAdf(measures) #data frame: cols folder,scandate,measures[..]
  for (qa in qa_dirs)
  {
    qa_xml_file <- file.path(basedir,analysisdir,qa,'summaryQA.xml')
    if(!file.exists(qa_xml_file))
    {
      warning(sprintf('Warning: no summaryQA.xml file found in %s',qa_xml_file))
      qa_measures[dim(qa_measures)[1]+1,] = c(qa,rep(NA,dim(qa_measures)[2]-1))
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

  #get epochs from foldernames and scandates
  qa_measures$folder_date <- do.call('c', lapply(qa_measures$folder, function(x){as.Date(gsub('QC_','',x), format = '%m%d%y')}))
  qa_measures$folder_epoch <- as.numeric(qa_measures$folder_date) #days since epoch start
  qa_measures$scandate_epoch <- as.numeric(as.Date(qa_measures$scandate)) #days since epoch start
  qa_measures$epoch_delta <- qa_measures$scandate_epoch - qa_measures$folder_epoch

  #combine the processed measures with the already completed file, using only fields that exist in the current file
  print(read_qa_measures)
  if(!is.na(read_qa_measures) && all(measures %in% colnames(read_qa_measures))){
    message('binding')
    qa_measures <- rbind(qa_measures, read_qa_measures[,colnames(read_qa_measures %in% qa_measures)])
  }
  return(qa_measures)
}

#scan_names excludes folders whose names are not in scan_names.
#scans_after_epoch takes a number.
#   If it is > 5000, it uses that number as the minimum epoch(e.g. as.numeric(as.Date('2020-01-01')))
#   If it is < 5000, it makes the minimum that many days before the current date. So 90 will return all folders with a datename that is < 90 days back.
#Note that this way of filtering based on foldernames is not perfect because the folder names had errors.

listQADirs <- function(basedir,analysisdir,scan_names='all',scans_after_epoch='all',fixfoldernames=TRUE){
  qa_dirs <- dir(file.path(basedir,analysisdir))
  qa_dirs <- qa_dirs[grep('QC_',qa_dirs)]
  qa_dirs <- qa_dirs[grep('_fBIRN',qa_dirs,invert = TRUE)]
  if(fixfoldernames){qa_dirs <- fixQAfoldernames(qa_dirs)}
  if (length(scan_names)>1 || scan_names != 'all'){
    qa_dirs <- qa_dirs[qa_dirs %in% scan_names]
  }
  if (scans_after_epoch != 'all'){
    if(scans_after_epoch > 5000){min_epoch <- scans_after_epoch
    }else{min_epoch <- as.numeric(Sys.Date())-scans_after_epoch}

    qa_epoch <- unlist(lapply(qa_dirs,function(x){as.numeric(as.Date(gsub('QC_','',x), format = '%m%d%y'))}))
    qa_dirs <- qa_dirs[qa_epoch > min_epoch]
    }
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


