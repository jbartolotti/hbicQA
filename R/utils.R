
checkPath <- function(){
  syspath <- list()
  syspath$afni <- suppressWarnings(system2('which', args = 'afni', stdout = TRUE,stderr = FALSE))
  syspath$bxh <- suppressWarnings(system2('which', args = 'dicom2bxh', stdout = TRUE,stderr = FALSE))

  missing_path <- FALSE
  missing_path_message = as.character()
  if (length(syspath$afni) == 0 ){
    missing_path <- TRUE
    missing_path_message <- c(missing_path_message,'hbicQA requires AFNI. Add AFNI to the path and rerun.\n  ')
  } else {message(sprintf('Using AFNI at %s',syspath$afni))}
  if (length(syspath$bxh) == 0 ){
    missing_path <- TRUE
    missing_path_message <- c(missing_path_message,'hbicQA requires BXH tools. Add bxh_xcede_tools to the path and rerun.\n  ')
  } else {message(sprintf('Using BXH xcede at %s',syspath$bxh))}
  if(missing_path){stop(missing_path_message)}
  return(syspath)

}

readMeasure <- function(measure,file)
{
  #read the value for a measure of interest from an xml document
  val <- system(sprintf('cat %s | grep \'"%s"\' ',file,measure),intern = TRUE)
  if (length(val) > 0)
  {
    close_tag <- which(strsplit(val, "")[[1]]==">")
    open_tag <- which(strsplit(val, "")[[1]]=="<")
    val <- (substr(val,close_tag[1]+1,open_tag[2]-1))
  } else (val <- NA)
  return(val)
}

allMeasures <- function(){
  return(c('mean','SNR','SFNR','std','percentFluc','drift','driftfit','rdc',
           'minCMassX','maxCMassX','meanCMassX','dispCMassX','driftCMassX',
           'minCMassY','maxCMassY','meanCMassY','dispCMassY','driftCMassY',
           'minCMassZ','maxCMassZ','meanCMassZ','dispCMassZ','driftCMassZ',
           'minFWHMX','maxFWHMX','meanFWHMX',
           'minFWHMY','maxFWHMY','meanFWHMY',
           'minFWHMZ','maxFWHMZ','meanFWHMZ',
           'meanGhost','meanBrightGhost'))
}

fixQAfoldernames <- function(qa_measures){
  qa_measures$folder[qa_measures$folder == 'QC_012317_rescan'] <- 'QC_012417'
  qa_measures$folder[qa_measures$folder == 'QC_07032019'] <- 'QC_070319'
  qa_measures$folder[qa_measures$folder == 'QC_07082019'] <- 'QC_070819'
  qa_measures$folder[qa_measures$folder == 'QC_07152019'] <- 'QC_071519'
  qa_measures <- qa_measures[qa_measures$folder != 'QC_07032019+RN',]
  return(qa_measures)
}
