

#hbicqa(datelist = c(071023, 072023, 072423, 073123),
#       docopyfromraw = FALSE,
#       dofbirn = TRUE,
#       doreports = TRUE,
#       dofigures = TRUE,
#       dohtmlreport = TRUE,
#       doservicereport = TRUE)

#hbicqa(datelist = c(121123), docopyfromraw = FALSE, dofbirn = TRUE, doreports = TRUE, dofigures = TRUE, dohtmlreport = TRUE, doservicereport = FALSE)


#' @export
hbicqa <- function(datelist='lookup_2025',
                   basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                   imagedir = 'Images',
                   analysisdir = 'Analysis',
                   rawdir = '/xnatdata/arch/9999/arc001',
                   reportdir = '~/R-Drive/Bartolotti_J/QA',
                   fBIRN_temp_dir = NA,
                   docopyfromraw = FALSE,
                   docopyfromraw_rxnat = TRUE,
                   dofbirn = FALSE,
                   doreports = FALSE,
                   dofigures = FALSE,
                   dohtmlreport = FALSE,
                   doservicereport = FALSE,
                   scans_after_epoch = 'all',
                   phantoms = list('bullet' = list(name = 'bullet',pretty_name = 'Bullet',
                                                   prefix = 'qc_', postprocess_prefix = 'QC_',
                                                   suffix = '', postprocess_suffix = '',
                                                   report = list(lastscan = FALSE, fig60 = FALSE)),
                                   'fbirn' = list(name = 'fbirn',pretty_name = 'fBIRN',
                                                  prefix = 'qc_', postprocess_prefix = 'QC_',
                                                  suffix = '_fbirn', postprocess_suffix = '_fbirn',
                                                  report = list(lastscan = TRUE, fig60 = TRUE)))

                   ){
  # checks for availability of system functions, i.e. afni and bxh_xcede
  syspath <- UTILS.checkPath(basedir,rawdir,reportdir)
  # if files are copied to a tempdir for fbirn processing, create the dir
  if (!is.na(fBIRN_temp_dir)){dir.create(fBIRN_temp_dir,showWarnings = FALSE)}

  # Get the datelist, a list of MMDDYY qc scans to process.
  # LIST
  if (typeof(datelist) == 'list'){
  datelist_list <- datelist
  #CHARACTER
  } else if(length(datelist)==1 && typeof(datelist)=='character'){
    if (length(datelist)==1 && datelist == 'lookup'){
      datelist_list <- LOAD.findNewScans(rawdir, file.path(basedir,imagedir), phantoms)
    } else if(length(datelist)==1 && datelist == 'lookup_oneyear'){
      datelist_list <- LOAD.findNewScans(rawdir, file.path(basedir,imagedir), phantoms, dayrange = 365)
    } else if(length(datelist)==1 && datelist == 'lookup_2025'){
      returndat <- LOAD.findNewScans_RXNAT('~/.Renviron_xnat', file.path(basedir,imagedir), file.path(reportdir, 'QA_report_fbirn.csv'),c('022625')) #blacklist 2/26/25
      datelist_list <- list()
      datelist_list$fbirn <- returndat$get_xnat
      datelist_list$bullet <- NA
      report_datelist_list <- list()
      report_datelist_list$fbirn <- returndat$get_report
      report_datelist_list$bullet <- NA
    } else if(all(!is.na(suppressWarnings(as.numeric(datelist))))){
      #it's all numbers as a string
      datelist_list <- list()
      for(p in phantoms){ datelist_list[[p$name]] <- as.numeric(datelist)}
    } else {
      stop("datelist must be either 'lookup', 'lookup_oneyear' a character/number/vector with format MMDDYY (to apply to all phantoms), or a list with a number/vector for each phantom")
    }
  # NUMERIC
  } else if(length(datelist)==1 && typeof(datelist)=='numeric'){
    datelist_list <- list()
    for(p in phantoms){ datelist_list[[p$name]] <- datelist}
  }
  qa_measures <- list()

  xnat_conn <- NULL
  for(p in phantoms){
    for(date in datelist_list[[p$name]]){
      if(!is.na(date)){
      #convert numbers to strings, 50386 to '050386'
      if(typeof(date)=='double'){date <- sprintf('%06d',date)}

      #move new raw qa scans to storage location
      if(docopyfromraw){
        move_qc_notes <- LOAD.move_qc(date, file.path(basedir,imagedir), rawdir, p)
        message(move_qc_notes$message)
      }

      if(docopyfromraw_rxnat){
          conn_notes <- LOAD.move_qc_rxnat(date, file.path(basedir,imagedir), '~/.Renviron_xnat', p,conn = xnat_conn)
          if(is.null(xnat_conn)){
            xnat_conn <- conn_notes$conn
          }
          message(conn_notes$notes$message)
      }
      #visual inspection function goes here
      }
    }

    reportdates <- datelist_list[[p$name]]
    if(exists('report_datelist_list')){reportdates <- report_datelist_list[[p$name]]}
    for(date in reportdates){
      if(!is.na(date)){
        if(dofbirn){
        #find qc series 4 for fbirn processing
        fBIRN_scan4_input <- file.path(basedir,imagedir,sprintf('%s%s%s',p$prefix, date, p$suffix),'SCANS','4','DICOM')
        if(!is.na(fBIRN_temp_dir)){
          fBIRN_scan4_temp_input <- file.path(fBIRN_temp_dir,sprintf('%s%s%s',p$prefix, date, p$suffix),'SCANS','4','DICOM')
          dir.create(fBIRN_scan4_temp_input,recursive = TRUE, showWarnings = FALSE)
        }
        fBIRN_scan4_output <- file.path(basedir,analysisdir,sprintf('%s%s%s',p$postprocess_prefix, date, p$postprocess_suffix))

        runfBIRN_notes <- PROCESS.runfBIRN(date, fBIRN_scan4_input,fBIRN_scan4_output, p, fBIRN_temp_dir)
        message(runfBIRN_notes$message)
      }
      #ASL processing script goes here

      #Monthly ADNI Gradient Nonlinearity goes here
    }
    }
    message(sprintf('fBIRN done for %s, phantom: %s', paste(datelist_list[[p$name]],collapse = ' '),p$name))

    qa_measures[[p$name]]<- NA
    if (doreports){
      qa_measures[[p$name]] <- fBIRN_Report(measures = 'all', phantom = p,
                                  scan_names = 'all', scans_after_epoch = scans_after_epoch,
                                  basedir = basedir, analysisdir = analysisdir, reportdir = reportdir,
                                  readfrom = sprintf('QA_Report%s.csv',p$suffix))
    }


  }




  myreport <- list()
  longreport <- list()

  for(p in phantoms){
    if (length(qa_measures[[p$name]]) == 1 && !is.na(qa_measures[[p$name]])){
      myreport[[p$name]] <- qa_measures[[p$name]]
    } else{
      myreport[[p$name]] <- file.path(reportdir,sprintf('QA_Report%s.csv',p$suffix))
    }
    if (dofigures || dohtmlreport){
      longreport[[p$name]] <- PROCESS.getTolerances(myreport[[p$name]])
    }
  }
  if (doservicereport){
    service <- LOAD.service_reports(file.path(basedir,'Reports','service'),'service_reports_categorized.txt')
  } else {
    service <- NA
  }
  if (dofigures){
      fBIRN_Figures(phantoms, myreport, service_reports = service)
  }

  if(dohtmlreport){
    fBIRN_html_Report(phantoms, report = myreport, longreport = longreport,  output_dir = reportdir)
  }
}




#https://ardata-fr.github.io/flextable-book/
#' @export
fBIRN_html_Report <- function(phantoms, system = 'synapse', report = 'import', longreport = 'calc', output_dir = 'auto'){

  if (system == 'synapse')
  {
    if(output_dir == 'auto'){
      output_dir <- '~/R-Drive/Bartolotti_J/QA'
    }
    figdir <- file.path(output_dir,'figures')
  } else if(system == 'Windows'){
    if(output_dir == 'auto'){
      output_dir <- '//kumc.edu/data/Research/Hoglund/Bartolotti_J/QA/'
    }
    figdir <- 'C:\\Users\\j186b025\\Documents\\local_qa\\figures'
  }

  if(length(report) == 1 && report == 'import'){
    report <- list()
    for(p in phantoms){
      report[[p$name]] <- file.path(output_dir, sprintf('QA_Report%s.csv',p$suffix))
    }
  }

  if(length(longreport)==1 && longreport == 'calc'){
    longreport <- list()
    for(p in phantoms){
    longreport[[p$name]] <- PROCESS.getTolerances(report[[p$name]])
    }
  }

  rmarkdown::render(system.file('extdata','report.Rmd', package = 'hbicQA'),
      output_dir = output_dir,
      output_file = sprintf('3T_QA_Report_%s.html',format(Sys.Date(), format = '%Y_%m_%d')),
      params = list(
        phantoms = phantoms,
        longreport = longreport,
        figdir = path.expand(figdir)
      )
  )
}

#' @export
fBIRN_Report <- function(scan_names = 'all',
                         measures = 'all',
                         phantom = NA,
                         scans_after_epoch = 'all', #after 11/22/16 to start after a big outlier #17167 = 1/1/2017 (epoch in days)
                         basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                         analysisdir = 'Analysis',
                         reportdir = '~/R-Drive/Bartolotti_J/QA',
                         writenewest = TRUE,
                         readfrom = NA
                         ){

  if(length(measures)==1 && measures == 'all'){measures <- UTIL.allMeasures()}

  if(!is.na(readfrom) && length(readfrom)>0 && file.exists(file.path(reportdir,readfrom))){
    read_qa_measures <- read.csv(file.path(reportdir,readfrom))
  }else{
      read_qa_measures <- NA
  }
  qa_measures <- LOAD.readQAMeasures(basedir, analysisdir, measures, phantom, read_qa_measures = read_qa_measures,
                                scan_names = scan_names, scans_after_epoch = scans_after_epoch, fixfoldernames = TRUE)
  mynow <- strftime(Sys.time(),format='%Y-%d-%m_%H-%M-%S')
  write.csv(qa_measures,file.path(reportdir,sprintf('QA_report%s_%s.csv',phantom$suffix, mynow)),row.names = FALSE)
  if(writenewest){file.copy(file.path(reportdir,sprintf('QA_report%s_%s.csv',phantom$suffix, mynow)),file.path(reportdir,sprintf('QA_report%s.csv', phantom$suffix)) , overwrite = TRUE)}
  return(qa_measures)
}

#' @export
fBIRN_Figures <- function(phantoms, reports, service_reports = NA, longreport = 'calc', reportdir = '~/R-Drive/Bartolotti_J/QA', figdir = 'figures', dosave = TRUE){
  dir.create(file.path(reportdir,figdir),showWarnings = FALSE)
  if(length(longreport)==1 && longreport == 'calc'){
    longreport <- list()
    for(p in phantoms){
      longreport[[p$name]] <- PROCESS.getTolerances(reports[[p$name]])
    }
  }
  FIGURES.makeFigures(phantoms, longreport, file.path(reportdir,figdir), dosave = dosave, service_reports = service_reports)

}
