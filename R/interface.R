#' @export
hbicqa <- function(datelist='lookup',
                   basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                   imagedir = 'Images',
                   analysisdir = 'Analysis',
                   rawdir = '/xnatdata/arch/9999/arc001',
                   reportdir = '~/R-Drive/Bartolotti_J/QA',
                   doreports = FALSE){
  #checks for availability of system functions, i.e. afni and bxh_xcede
  syspath <- checkPath()

# if (datelist == 'lookup'){datelist <- findNewScans()}
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
    fBIRN_Report('all', basedir, analysisdir, reportdir)
  }
}


#' @export
fBIRN_Report <- function(measures = 'all',
                         scans = 'all',
                         basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                         analysisdir = 'Analysis',
                         reportdir = '~/R-Drive/Bartolotti_J/QA'){

  if(measures == 'all'){measures <- allMeasures()}

  qa_measures <- readQAMeasures(basedir, analysisdir, measures, scans, fixfoldernames = TRUE)

  write.csv(qa_measures,file.path(reportdir,'report220314.csv'),row.names = FALSE)

}
