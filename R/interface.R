#' @export
hbicqa <- function(datelist='lookup',
                   basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                   imagedir = 'Images',
                   analysisdir = 'Analysis',
                   rawdir = '/xnatdata/arch/9999/arc001',
                   reportdir = '~/R-Drive/Bartolotti_J/QA',
                   fBIRN_temp_dir = NA,
                   doreports = FALSE,
                   dofigures = FALSE,
                   dohtmlreport = FALSE,
                   scans_after_epoch = 'all'){
  #checks for availability of system functions, i.e. afni and bxh_xcede
  syspath <- checkPath(basedir,rawdir,reportdir)
  if (!is.na(fBIRN_temp_dir)){dir.create(fBIRN_temp_dir,showWarnings = FALSE)}
  if (datelist == 'lookup'){
     datelist <- findNewScans(rawdir, file.path(basedir,imagedir))
  } else if(typeof(datelist)=='character'){
     error("datelist must be either 'lookup' or a number/vector with format MMDDYY")
  }
  for(date in datelist){
    datestr <-  sprintf('%06d',date)
    #move new raw qa scans to storage location
    move_qc_notes <- move_qc(date, file.path(basedir,imagedir), rawdir)
    message(move_qc_notes$message)

    #visual inspection function goes here

    #fBIRN BOLD data processing
    fBIRN_scan4_input <- file.path(basedir,imagedir,sprintf('qc_%s',datestr),'SCANS','4','DICOM')
    if(!is.na(fBIRN_temp_dir)){
#      fBIRN_scan4_temp_input <- file.path(fBIRN_temp_dir,sprintf('qc_%s',datestr),'SCANS','4','DICOM')
      dir.create(fBIRN_scan4_temp_input,recursive = TRUE, showWarnings = FALSE)
    }
    fBIRN_scan4_output <- file.path(basedir,analysisdir,sprintf('QC_%s',datestr))
    runfBIRN_notes <- runfBIRN(date, fBIRN_scan4_input,fBIRN_scan4_output, fBIRN_temp_dir)
    message(runfBIRN_notes$message)

    #ASL processing script goes here

    #Monthly ADNI Gradient Nonlinearity goes here
  }
  message(sprintf('fBIRN done for: %s',paste(datelist,collapse = ' ')))


  qa_measures <- NA
  if (doreports){
    qa_measures <- fBIRN_Report(measures = 'all', scan_names = 'all', scans_after_epoch = scans_after_epoch,
                 basedir = basedir, analysisdir = analysisdir, reportdir = reportdir,
                 readfrom = 'QA_Report.csv')
  }
  if (dofigures){
    if (!is.na(qa_measures)){myreport <- qa_measures} else{myreport <- file.path(reportdir,'QA_Report.csv')}
    fBIRN_Figures(report = myreport)
  }
  if(dohtmlreport){
    if (!is.na(qa_measures)){myreport <- qa_measures} else{myreport <- file.path(reportdir,'QA_Report.csv')}
    fBIRN_html_Report(report = myreport, output_dir = report_dir)
  }
}


#https://ardata-fr.github.io/flextable-book/
#' @export
fBIRN_html_Report <- function(system = 'synapse', report = 'import', longreport = 'calc', output_dir = 'auto'){

  if (system == 'synapse')
  {
    if(output_dir == 'auto'){
      output_dir <- '~/R-Drive/Bartolotti_J/QA'
    }
    if(report == 'import'){
      report <- file.path(output_dir,'QA_Report.csv')
    }
    figdir <- file.path(output_dir,'figures')
  } else if(system == 'Windows'){
    if(output_dir == 'auto'){
      output_dir <- '//kumc.edu/data/Research/Hoglund/Bartolotti_J/QA/'
    }
    if(report == 'import'){
      report <- file.path(output_dir,'QA_Report.csv')
    }
    figdir <- 'C:\\Users\\j186b025\\Documents\\local_qa\\figures'
  }
  if(longreport == 'calc'){longreport <- getTolerances(report)}
    rmarkdown::render(system.file('extdata','report.Rmd', package = 'hbicQA'),
      output_dir = output_dir,
      output_file = 'Report.html',
      params = list(
        longreport = longreport,
        figdir = figdir
      )
)
}

#' @export
fBIRN_Report <- function(scan_names = 'all',
                         measures = 'all',
                         scans_after_epoch = 'all', #after 11/22/16 to start after a big outlier #17167 = 1/1/2017 (epoch in days)
                         basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                         analysisdir = 'Analysis',
                         reportdir = '~/R-Drive/Bartolotti_J/QA',
                         writenewest = TRUE,
                         readfrom = 'QA_Report.csv'
                         ){

  if(measures == 'all'){measures <- allMeasures()}
  if(!is.na(readfrom) && length(readfrom)>0 && file.exists(file.path(reportdir,readfrom))){read_qa_measures <- read.csv(file.path(reportdir,readfrom))}else{read_qa_measures <- NA}
  qa_measures <- readQAMeasures(basedir, analysisdir, measures, read_qa_measures = read_qa_measures,
                                scan_names = scan_names, scans_after_epoch = scans_after_epoch, fixfoldernames = TRUE)
  mynow <- strftime(Sys.time(),format='%Y-%d-%m_%H-%M-%S')
  write.csv(qa_measures,file.path(reportdir,sprintf('QA_report_%s.csv',mynow)),row.names = FALSE)
  if(writenewest){file.copy(file.path(reportdir,sprintf('QA_report_%s.csv',mynow)),file.path(reportdir,'QA_report.csv') , overwrite = TRUE)}
  return(qa_measures)
}

#' @export
fBIRN_Figures <- function(report, reportdir = '~/R-Drive/Bartolotti_J/QA', figdir = 'figures', dosave = TRUE){
  dir.create(file.path(reportdir,figdir),showWarnings = FALSE)
  longreport <- getTolerances(report)
  makeFigures(longreport, file.path(reportdir,figdir), dosave)

}
