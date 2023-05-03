#' @export
hbicqa <- function(datelist='lookup_oneyear',
                   basedir = '~/R-Drive/Brooks_W/Skyra_QC',
                   imagedir = 'Images',
                   analysisdir = 'Analysis',
                   rawdir = '/xnatdata/arch/9999/arc001',
                   reportdir = '~/R-Drive/Bartolotti_J/QA',
                   fBIRN_temp_dir = NA,
                   docopyfromraw = TRUE,
                   dofbirn = FALSE,
                   doreports = FALSE,
                   dofigures = FALSE,
                   dohtmlreport = FALSE,
                   scans_after_epoch = 'all',
                   phantoms = list('bullet' = list(name = 'bullet',
                                                   prefix = 'qc_', postprocess_prefix = 'QC_',
                                                   suffix = '', postprocess_suffix = ''),
                                   'fbirn' = list(name = 'fbirn',
                                                  prefix = 'qc_', postprocess_prefix = 'QC_',
                                                  suffix = '_fbirn', postprocess_suffix = '_fbirn'))

                   ){
  # checks for availability of system functions, i.e. afni and bxh_xcede
  syspath <- UTILS.checkPath(basedir,rawdir,reportdir)
  # if files are copied to a tempdir for fbirn processing, create the dir
  if (!is.na(fBIRN_temp_dir)){dir.create(fBIRN_temp_dir,showWarnings = FALSE)}

  # Get the datelist, a list of MMDDYY qc scans to process.
  if (datelist == 'lookup'){
     datelist_list <- LOAD.findNewScans(rawdir, file.path(basedir,imagedir), phantoms)
  } else if(datelist == 'lookup_oneyear'){
    datelist_list <- LOAD.findNewScans(rawdir, file.path(basedir,imagedir), phantoms, dayrange = 365)
  } else if(typeof(datelist)=='character'){
     error("datelist must be either 'lookup', 'lookup_oneyear' a number/vector with format MMDDYY (to apply to all phantoms), or a list with a number/vector for each phantom")
  } else if (typeof(datelist) == 'list'){
    datelist_list <- datelist
  } else {
    datelist_list <- list()
    for(p in phantoms){ datelist_list[[p$name]] <- datelist}
  }
  qa_measures <- list()

  for(p in phantoms){
    for(date in datelist_list[[p$name]]){
      #convert numbers to strings, 50386 to '050386'
      if(typeof(date)=='double'){date <- sprintf('%06d',date)}

      #move new raw qa scans to storage location
      if(docopyfromraw){
        move_qc_notes <- LOAD.move_qc(date, file.path(basedir,imagedir), rawdir, p)
        message(move_qc_notes$message)
      }
      #visual inspection function goes here


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
    message(sprintf('fBIRN done for %s, phantom: %s',p$name, paste(datelist_list[[p$name]],collapse = ' ')))

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
    if (!is.na(qa_measures[[p$name]])){
      myreport[[p$name]] <- qa_measures[[p$name]]
    } else{
      myreport[[p$name]] <- file.path(reportdir,sprintf('QA_Report%s.csv',p$suffix))
    }
    if (dofigures || dohtmlreport){
      longreport[[p$name]] <- PROCESS.getTolerances(myreport[[p$name]])
    }
  }
  if (dofigures){
      fBIRN_Figures(phantoms, myreport)
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

  if(report == 'import'){
    report <- list()
    for(p in phantoms){
      report[[p$name]] <- file.path(output_dir, sprintf('QA_Report%s.csv',p$suffix))
    }
  }

  if(longreport == 'calc'){
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

  if(measures == 'all'){measures <- UTIL.allMeasures()}

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
fBIRN_Figures <- function(phantoms, reports, longreport = 'calc', reportdir = '~/R-Drive/Bartolotti_J/QA', figdir = 'figures', dosave = TRUE){
  dir.create(file.path(reportdir,figdir),showWarnings = FALSE)
  if(longreport == 'calc'){
    longreport <- list()
    for(p in phantoms){
      longreport[[p$name]] <- PROCESS.getTolerances(reports[[p$name]])
    }
  }
  FIGURES.makeFigures(phantoms, longreport, file.path(reportdir,figdir), dosave)

}
