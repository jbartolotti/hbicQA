
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
