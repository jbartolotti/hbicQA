
LOAD.findNewScans <- function(rawdir, targetdir, phantoms, dayrange = 9999){
  arcscan <- list(all = dir(rawdir))
  imscan <- list(all = dir(targetdir))
  uncopied <- list()
  returndat <- list()

  for(p in phantoms){
    # Get contents of archive and images directory, and get the names of arc folders not yet in images.
    arcscan[[p$name]] <- arcscan$all[grepl(sprintf('(^%s[0-9]{6}%s$)',p$prefix, p$suffix),arcscan$all)] #capture beginning string (^), qc_, six digits, suffix (i.e. '' or '_fbirn') and end of string ($)
    imscan[[p$name]] <- imscan$all[grepl(sprintf('(^%s[0-9]{6}%s$)',p$prefix, p$suffix),imscan$all)] #capture qc_, six digits, suffix, and end of string ($)
    uncopied[[p$name]] <- arcscan[[p$name]][ !(arcscan[[p$name]] %in% imscan[[p$name]]) ]

    dayrange_start <- as.numeric(Sys.Date())-dayrange

    #get the date portion between the prefix and suffix and convert to epoch.
    uncopied_days <- unlist(lapply(uncopied[[p$name]],
                      function(x){
                        as.numeric(as.Date(
                          gsub(sprintf(".*%s(.+)%s*",p$prefix,p$suffix), "\\1", x), format = '%m%d%y'))
                      }))
    #extract the list of uncopied scans that have epochs later than the start date.
    uncopied_dayrange <- uncopied[[p$name]][uncopied_days > dayrange_start]

    # get the date characters (i.e. MMDDYY) from the matched list by removing the prefix and suffix
    uncopied_dayrange_date <-gsub(sprintf(".*%s(.+)%s*",p$prefix,p$suffix), "\\1", uncopied_dayrange)

    returndat[[p$name]] <- uncopied_dayrange_date
  }
#uncopied_list <- list(bullet = as.numeric(uncopied_dayrange_dateonly_bullet),
#     fbirn = as.numeric(uncopied_dayrange_dateonly_fbirn))
  return(returndat)
}

LOAD.service_reports <- function(folder = NA, filename = NA){
  if (is.na(folder)){
    folder <- '//kumc.edu/data/Research/Hoglund/Brooks_W/Skyra_QC/Reports/service'
  }
  if (is.na(filename)){
    filename <- 'service_reports_categorized.txt'
  }
  dat <- read.delim(file.path(folder,filename), sep = '\t')
  subdat <- subset(dat, (category_spectroscopy != '' | category_gradient != '' | category_coil != '' | category_cooling != '') & note_only != 'note')
  subdat$epoch_day <- subdat$epoch / (24*60*60)
  return(subdat)
}


LOAD.move_qc <- function(date, savedir, rawdir, phantom, overwrite = FALSE){

  rawfile <- file.path(rawdir,sprintf('%s%s%s',phantom$prefix, date, phantom$suffix))
  savefile <- file.path(savedir,sprintf('%s%s%s',phantom$prefix, date, phantom$suffix))

  do_copy <- TRUE
  copy_succeed <- FALSE
  already_existed <- FALSE
  if (!file.exists(rawfile))
  {warning(sprintf('%s does not exist. Date should be format MMDDYY.',rawfile))}
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
    if (!copy_succeed){warning(sprintf('copy from %s to %s failed. Do you have write permissions?',rawfile,savefile))}
  }

  notes <- list()
  notes$target_exists <- file.exists(savefile)
  notes$did_copy <- copy_succeed
  mymessage <- as.character()
  if (notes$target_exists) {
    mymessage <- sprintf('Data file %s ready for processing.', savefile)
    if(already_existed){
      if(overwrite){was_or_not <- '*WAS*'} else{was_or_not = 'was *NOT*'}
      mymessage <- c(mymessage,sprintf('  file already existed and %s overwritten',was_or_not))
    } else {
      mymessage <- c(mymessage, sprintf('  file was successfully copied from %s.\n',rawfile))
      }
  }else{warning(sprintf('%s does not exist or is not readable.', savefile))}
  notes$message <- mymessage

  return(notes)
}

LOAD.readQAMeasures <- function(basedir, analysisdir, measures, phantom, read_qa_measures = NA, scan_names='all',scans_after_epoch='all' , fixfoldernames = TRUE){
  qa_dirs <- LOAD.listQADirs(basedir,analysisdir, phantom,
                        scan_names = scan_names,scans_after_epoch = scans_after_epoch,
                        fixfoldernames = fixfoldernames)
  #If all measures of interest are in the already completed report,
  #then only read directories that don't already exist in the already completed report
  if( typeof(read_qa_measures) == 'list'  && all(measures %in% colnames(read_qa_measures))){
    already_read <- read_qa_measures$folder
    qa_dirs <- qa_dirs[!(qa_dirs %in% already_read)]
  }

  qa_measures <- initializeQAdf(measures) #data frame: cols folder,scandate,measures[..]
  for (qa in qa_dirs)
  {
    message(sprintf('extracting measures for: %s',qa))
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
      qa_measures[rr,'scandate'] <- UTIL.readMeasure('scandate',qa_xml_file)
      qa_measures[rr,'color'] <- 'auto'
      for (mm in measures)
      {
        qa_measures[rr,mm] <- as.numeric(UTIL.readMeasure(mm,qa_xml_file)[1])
      }
    }
  }
 #get epochs from foldernames and scandates
  qa_measures$folder_date <- as.character(do.call('c', lapply(qa_measures$folder, function(x){as.Date(gsub('QC_','',x), format = '%m%d%y')})))
    qa_measures$folder_epoch <- as.numeric(qa_measures$folder_date) #days since epoch start
    qa_measures$scandate_epoch <- as.numeric(as.Date(qa_measures$scandate)) #days since epoch start
    qa_measures$epoch_delta <- qa_measures$scandate_epoch - qa_measures$folder_epoch

  #combine the processed measures with the already completed file, using only fields that exist in the current file
  if( typeof(read_qa_measures) == 'list' && all(measures %in% colnames(read_qa_measures))){
    qa_measures <- rbind(qa_measures, read_qa_measures[,colnames(read_qa_measures) %in% colnames(qa_measures)])
  }
  return(qa_measures)
}

#scan_names excludes folders whose names are not in scan_names.
#scans_after_epoch takes a number.
#   If it is > 5000, it uses that number as the minimum epoch(e.g. as.numeric(as.Date('2020-01-01')))
#   If it is < 5000, it makes the minimum that many days before the current date. So 90 will return all folders with a datename that is < 90 days back.
#Note that this way of filtering based on foldernames is not perfect because the folder names had errors.

LOAD.listQADirs <- function(basedir,analysisdir, phantom, scan_names='all',scans_after_epoch='all',fixfoldernames=TRUE){
  qa_dirs <- dir(file.path(basedir,analysisdir))
  qa_dirs <- qa_dirs[grep(sprintf('^%s[0-9]{6}%s$',phantom$postprocess_prefix, phantom$postprocess_suffix),qa_dirs)]
  if(fixfoldernames){qa_dirs <- UTIL.fixQAfoldernames(qa_dirs)}
  if (length(scan_names)>1 || scan_names != 'all'){
    qa_dirs <- qa_dirs[qa_dirs %in% scan_names]
  }
  if (scans_after_epoch != 'all'){
    if(scans_after_epoch > 5000){min_epoch <- scans_after_epoch
    }else{min_epoch <- as.numeric(Sys.Date())-scans_after_epoch}

    qa_epoch <- unlist(lapply(qa_dirs,function(x){
      as.numeric(as.Date( gsub(sprintf(".*%s(.+)%s*",phantom$postprocess_prefix,phantom$postprocess_suffix), "\\1", x) , format = '%m%d%y'))
      }))
    qa_dirs <- qa_dirs[qa_epoch > min_epoch]
    }
  return(qa_dirs)
}

initializeQAdf <- function(measures){
  #qa_measures <- data.frame(folder = as.character(),
  #                          scandate = as.character(),
  #                          color = as.character(),
  #                          stringsAsFactors = FALSE)
  qa_measures <- data.frame(matrix(nrow = 0, ncol = 3+length(measures)))
  colnames(qa_measures) <- c('folder','scandate','color',measures)
  return(qa_measures)
}




# logical vector of all entries that end in 25 or greater
check2025 <- function(strings) {
  logical_vector <- sapply(strings, function(x) {
    last_two_chars <- substr(x, nchar(x)-1, nchar(x))
    if (grepl("^[0-9]+$", last_two_chars)) {
      as.numeric(last_two_chars) >= 25
    } else {
      FALSE
    }
  })
  return(logical_vector)
}

# Force Skyra_QC_999999 format
format_skyraqc <- function(strings) {
  manipulated_strings <- sapply(strings, function(x) {
    # Split the string into text and date code parts
    parts <- strsplit(x, "_|-")[[1]]
    text_part <- paste(parts[1:2], collapse = "_")
    date_code <- parts[3]

    # Ensure correct capitalization and replace - with _
    text_part <- gsub("-", "_", text_part)
    text_part <- "Skyra_QC"

    # Combine text part and date code
    manipulated_string <- paste(text_part, date_code, sep = "_")
    return(manipulated_string)
  })
  return(manipulated_strings)
}


# Function to filter directories based on the year and format
check2025_downloaded <- function(directories) {
  filtered_directories <- directories[sapply(directories, function(x) {
    # Check if the directory matches the qc_123456_fbirn format
    if (grepl("^qc_[0-9]{6}_fbirn$", x)) {
      # Extract the date part
      date_part <- strsplit(x, "_")[[1]][2]
      year <- as.numeric(substr(date_part, 5, 6))
      # Check if the year is greater than or equal to 25
      return(year >= 25)
    } else {
      return(FALSE)
    }
  })]
  return(filtered_directories)
}


# Function to extract dates from qc_downloaded
extract_dates_downloaded <- function(directories) {
  dates <- sapply(directories, function(x) {
    date_part <- strsplit(x, "_")[[1]][2]
    return(date_part)
  })
  return(dates)
}

# Function to extract dates from qc_scans_2025$subject_formatted
extract_dates_scans <- function(subjects) {
  dates <- sapply(subjects, function(x) {
    date_part <- strsplit(x, "_")[[1]][3]
    return(date_part)
  })
  return(dates)
}

LOAD.findNewScans_RXNAT <- function(xnat_renviron, imagefolder, reportfile, blacklist_dates = NULL){

    # See what's downloaded
  qc_downloaded <- check2025_downloaded(dir(imagefolder))

  # see what's in the report already
  report <- read.csv(reportfile)
  report_dates <- unlist(lapply(report$folder, function(x) {strsplit(x,'_')[[1]][2]}))

  # See what's in XNAT
  # Connect using credentials in Renviron_xnat
  readRenviron(xnat_renviron)
  hbic <- Rxnat::xnat_connect('https://xnat.kumc.edu', xnat_name='HBIC')

  # Get qc scans
  project_ID <- '9999'
  hbic_exp <- hbic$experiments()
  qc_scans <- subset(hbic_exp, project == project_ID)

  # filter for names of qc scans 2025 or later
  qc_scans_2025 <- qc_scans[check2025(qc_scans$subject),]
  qc_scans_2025$subject_formatted <- format_skyraqc(qc_scans_2025$subject)

  # Extract dates
  dates_downloaded <- extract_dates_downloaded(qc_downloaded)
  dates_scans <- extract_dates_scans(qc_scans_2025$subject_formatted)

  #scans_to_download <- qc_scans_2025$subject_formatted[!dates_scans %in% dates_downloaded]
  dates_to_download <- dates_scans[!dates_scans %in% dates_downloaded]
  if(!is.null(blacklist_dates)){
    dates_to_download <- dates_to_download[!dates_to_download %in% blacklist_dates]
  }

  dates_to_calc <- dates_scans[!dates_scans %in% report_dates]
  if(!is.null(blacklist_dates)){
    dates_to_calc <- dates_to_calc[!dates_to_calc %in% blacklist_dates]
  }

  returndat <- list(get_xnat = dates_to_download,
                    get_report = dates_to_calc)
  return(returndat)
}

LOAD.move_qc_rxnat <- function(date, savedir, xnat_renviron, phantom, conn = NULL){
  if(is.null(conn)){
    readRenviron(xnat_renviron)
    conn <- Rxnat::xnat_connect('https://xnat.kumc.edu', xnat_name='HBIC')
  }
  # Get qc scans
  project_ID <- '9999'
  hbic_exp <- conn$experiments()
  qc_scans <- subset(hbic_exp, project == project_ID)
  qc_scans$subject_formatted <- format_skyraqc(qc_scans$subject)

  dates_scans <- extract_dates_scans(qc_scans$subject_formatted)
  scans_to_download <- names(dates_scans)[dates_scans %in% date]
  #date <- c("022625","032825","040325","040825","041425","042225")



  target_dir <- file.path(savedir,sprintf('%s%s%s',phantom$prefix, date, phantom$suffix))

    if(file.exists(target_dir)){
      message(sprintf('Skipping %s, target exists', scans_to_download))
    } else{

      this_scan <- subset(qc_scans, subject_formatted == scans_to_download)
      if(dim(this_scan)[1]){
        temp_dir <- tempdir()
        dl <- Rxnat::download_xnat_dir(conn, this_scan$ID, file_dir = temp_dir, timeout_duration = 600)
        utils::unzip(file.path(temp_dir,sprintf('%s.zip',this_scan$ID)), exdir = temp_dir)
        dir.create(target_dir, recursive = TRUE)
        scans_dir <- list.dirs(temp_dir, recursive = TRUE, full.names = TRUE)
        scans_dir <- scans_dir[grepl("/scans$", scans_dir)]  # Find the scans directory

        if (length(scans_dir) > 0) {
          data_dirs <- list.dirs(scans_dir, recursive = FALSE, full.names = TRUE)

          for (dir in data_dirs) {
            file.copy(dir, target_dir, recursive = TRUE)
          }

          # Copy dicoms to the structure that hbicQA is looking for, until we can fix hbicQA
          bold_dir <- dir(target_dir)[grepl('.*EPI.*Bold.*',dir(target_dir))]
          # Find the data directory
          data_directory <- find_dcm_directory(file.path(target_dir,bold_dir))

          if (!is.null(data_directory)) {
            # Create the new directory structure
            new_directory <- file.path(target_dir, "SCANS", "4", "DICOM")
            dir.create(new_directory, recursive = TRUE, showWarnings = FALSE)

            # Copy the .dcm files to the new directory
            dcm_files <- list.files(data_directory, full.names = TRUE)
            copy_success <- file.copy(dcm_files, new_directory)
          }

        }
      }
    }
  return(list(conn = conn, notes = list(message = 'message here')))

  }


# Function to find the directory containing .dcm files
find_dcm_directory <- function(dir) {
  subdirs <- list.dirs(dir, recursive = TRUE)
  for (subdir in subdirs) {
    dcm_files <- list.files(subdir, pattern = "\\.dcm$", full.names = TRUE)
    if (length(dcm_files) > 0) {
      return(subdir)
    }
  }
  return(NULL)
}




