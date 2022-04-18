runfBIRN <- function(date, indir4, outdir4){
  datestr <-  sprintf('%06d',date)
  already_processed <- FALSE

  if(file.exists(outdir4) && length(dir(outdir4, all.files=TRUE,no.. = TRUE)) > 0)
  {
    already_processed <- TRUE
    warning(sprintf('Directory %s is non-empty; skipping fmriqa_phantomqa.pl.',outdir4))
  } else {
    system2('dicom2bxh', args = c(
      '--xcede',
      file.path(indir4,'*.dcm'),
      file.path(indir4,sprintf('QC_%s_WRAPPED.xml',datestr))
    ), wait= TRUE
    )
    system2('fmriqa_phantomqa.pl', args = c(
      file.path(indir4, sprintf('QC_%s_WRAPPED.xml',datestr)),
      outdir4
    ), wait = TRUE
    )

  }
  notes <- list()
  mymessage <- as.character()
  if(file.exists(outdir4) && length(dir(outdir4, all.files=TRUE,no.. = TRUE)) > 0){

    mymessage <- sprintf('fBIRN complete. Proceeding with file %s\n',outdir4)
    if(already_processed){
      mymessage <- c(mymessage,'NB: data already existed and was not reprocessed.')
    }
  } else{stop(sprintf('FAIL: %s does not exist or is not readable.', outdir4))}

  notes$message <- mymessage
  return(notes)
}
