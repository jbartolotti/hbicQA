#' @export
hbicqa <- function(datelist='lookup',
                   basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                   imagedir = 'Images',
                   analysisdir = 'Analysis',
                   rawdir = '/xnatdata/arch/9999/arc001',
                   reportdir = '~/R-Drive/Bartolotti_J/QA',
                   doreports = FALSE){
  #checks for availability of system functions, i.e. afni and bxh_xcede
  syspath <- checkPath(basedir,rawdir,reportdir)

# if (datelist == 'lookup'){datelist <- findNewScans(rawdir, file.path(basedir,imagedir))}
  for(date in datelist){
    datestr <-  sprintf('%06d',date)
    #move new raw qa scans to storage location
    move_qc_notes <- move_qc(date, file.path(basedir,imagedir), rawdir)
    message(move_qc_notes$message)

    #visual inspection function goes here

    #fBIRN BOLD data processing
    fBIRN_scan4_input <- file.path(basedir,imagedir,sprintf('qc_%s',datestr),'SCANS','4','DICOM')
    fBIRN_scan4_output <- file.path(basedir,analysisdir,sprintf('QC_%s',datestr))
    runfBIRN_notes <- runfBIRN(date, fBIRN_scan4_input,fBIRN_scan4_output)
    message(runfBIRN_notes$message)

    #ASL processing script goes here

    #Monthly ADNI Gradient Nonlinearity goes here
  }

  if (doreports){
    fBIRN_Report(measures = 'all', scan_names = 'all', scans_after_epoch = 'all',
                 basedir = basedir, analysisdir = analysisdir, reportdir = reportdir,
                 readfrom = 'QA_Report.csv')
  }

}


#' @export
fBIRN_Report <- function(measures = 'all',
                         scan_names = 'all',
                         scans_after_epoch = 'all',
                         basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                         analysisdir = 'Analysis',
                         reportdir = '~/R-Drive/Bartolotti_J/QA',
                         writenewest = TRUE,
                         readfrom = 'QA_Report.csv'
                         ){

  if(measures == 'all'){measures <- allMeasures()}
  if(!is.na(readfrom) & length(readfrom)>0){read_qa_measures <- read.csv(file.path(reportdir,readfrom))}else{read_qa_measures <- NA}
  qa_measures <- readQAMeasures(basedir, analysisdir, measures, read_qa_measures = read_qa_measures,
                                scan_names = scan_names, scans_after_epoch = scans_after_epoch, fixfoldernames = TRUE)
  write.csv(qa_measures,file.path(reportdir,sprintf('QA_report_%s.csv',Sys.Date())),row.names = FALSE)
  if(writenewest){file.copy(file.path(reportdir,sprintf('QA_report_%s.csv',Sys.Date())),file.path(reportdir,'QA_report.csv') , overwrite = TRUE)}
  return(qa_measures)
}
