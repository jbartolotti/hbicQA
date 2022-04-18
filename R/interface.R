#' @export
hbicqa <- function(date,
                   basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                   imagedir = 'Images',
                   rawdir = '/xnatdata/arch/9999/arc001'){
  datestr <-  sprintf('%06d',date)

  #checks for availability of system functions, i.e. afni and bxh_xcede
  syspath <- checkPath()

  #move new raw qa scans to storage location
  move_qc_notes <- move_qc(date, file.path(basedir,imagedir), rawdir)
  message(move_qc_notes$message)

  #visual inspection function goes here

  #fBIRN BOLD data processing
  fBIRN_scan4_input <- file.path(basedir,imagedir,sprintf('qc_%s',datestr),'SCANS','4','DICOM')
  fBIRN_scan4_output <- file.path(basedir,'Analysis',sprintf('QC_%s',datestr))
  runfBIRN_notes <- runfBIRN(date, fBIRN_scan4_input,fBIRN_scan4_output)
  message(runfBIRN_notes$message)

  #ASL processing script goes here

  #Monthly ADNI Gradient Nonlinearity goes here

}
