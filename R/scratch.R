
notafun <- function(){

  #From Analysis/QC_070725_fbirn/summaryQA.xml

#  <!-- Location of static spatial noise image (i.e. difference image) is stored in 'diffimagefile'. -->
#    <observation name="diffimagefile" type="varchar">/home/sa-j186b025/R-Drive/Brooks_W/Skyra_QC/Analysis/QC_070725_fbirn/QC_070725_fbirn_WRAPPED_nave.bxh</observation>
#      <!-- Location of "signal image" (mean across time) is stored in 'meanimagefile'. -->
#      <observation name="meanimagefile" type="varchar">/home/sa-j186b025/R-Drive/Brooks_W/Skyra_QC/Analysis/QC_070725_fbirn/QC_070725_fbirn_WRAPPED_ave.bxh</observation>
#        <!-- Location of "temporal fluctuation noise image" (standard deviation image of voxel-by-voxel residuals of second-order polynomial fit across time) is stored in 'stdimagefile'. -->
#        <observation name="stdimagefile" type="varchar">/home/sa-j186b025/R-Drive/Brooks_W/Skyra_QC/Analysis/QC_070725_fbirn/QC_070725_fbirn_WRAPPED_sd.bxh</observation>
#          <!-- SFNR image is signal image divided by temporal fluctuation noise image. -->
#          <observation name="sfnrimagefile" type="varchar">/home/sa-j186b025/R-Drive/Brooks_W/Skyra_QC/Analysis/QC_070725_fbirn/QC_070725_fbirn_WRAPPED_sfnr.bxh</observation>
#            <!-- Signal summary value ('mean') as defined in Friedman, Glover (2006) is the mean across the ROI of the signal image ('meanimagefile'). -->
#            <observation name="mean" type="float">2218.3</observation>
#              <!-- SNR summary value as defined in Friedman, Glover (2006) is the signal summary value ('mean') divided by the standard deviation across the ROI of the static spatial noise image ('diffimagefile'). -->
#              <observation name="SNR" type="float">127.7</observation>
#                <!-- SFNR summary value as defined in Friedman, Glover (2006) is the mean across the ROI of the signal image ('meanimagefile') divided by the temporal fluctuation image ('stdimagefile'). -->
#                <observation name="SFNR" type="float">133.3</observation>


#  cd ~/R-Drive/Brooks_W/Skyra_QC/Analysis/QC_070725_fbirn
#  bxh2analyze --nii QC_070725_fbirn_WRAPPED_nave.bxh proc/QC_070725_fbirn_nave
#  bxh2analyze --nii QC_070725_fbirn_WRAPPED_ave.bxh proc/QC_070725_fbirn_ave
#  bxh2analyze --nii QC_070725_fbirn_WRAPPED_sd.bxh proc/QC_070725_fbirn_sd
#  bxh2analyze --nii QC_070725_fbirn_WRAPPED_sfnr.bxh proc/QC_070725_fbirn_sfnr


mydir <- "R:/Hoglund/Brooks_W/Skyra_QC/Analysis/QC_070725_fbirn"

# Static Spatial Noise Image
qc_nave <- readNifti(file.path(mydir, 'proc', 'QC_070725_fbirn_nave.nii'))

# Signal Image
qc_ave <- readNifti(file.path(mydir, 'proc', 'QC_070725_fbirn_ave.nii'))

# Temporal Fluctuation Noise Image
qc_sd <- readNifti(file.path(mydir, 'proc', 'QC_070725_fbirn_sd.nii'))

# SFNR Image
qc_sfnr <- readNifti(file.path(mydir, 'proc', 'QC_070725_fbirn_sfnr.nii'))




img_dim <- dim(qc_ave)

center_x <- floor(img_dim[1] / 2)
center_y <- floor(img_dim[2] / 2)
half_width <- floor(21 / 2)

# Compute ROI bounds
x_range <- (center_x - half_width):(center_x + half_width)
y_range <- (center_y - half_width):(center_y + half_width)

# Extract ROI
roi <- qc_ave[x_range, y_range]

mean(qc_ave[(x_range-1), (y_range-1)])
mean(qc_ave[(x_range-0), (y_range-0)])
mean(qc_ave[(x_range+1), (y_range+1)])

mean(qc_nave[(x_range-1), (y_range-1)])
mean(qc_nave[(x_range-0), (y_range-0)])
mean(qc_nave[(x_range+1), (y_range+1)])






}

