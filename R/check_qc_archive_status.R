library(lubridate)

#ON SYNAPSE

# Define a function to process the qc_dicoms.txt file
process_qc_dicoms <- function(file) {
  # Read in the text file and save to a data frame with "/" as a separator
  df <- read.table(file, sep = "/", stringsAsFactors = FALSE)
  # Make a new column called date
  df$date <- NA
  df$date_string <- NA

  # Loop through the rows of the data frame
  for (i in 1:nrow(df)) {
    # Extract the qc_MMDDYY value from the sixth column
    qc_value <- df[i, 6]
    # Check if the qc_value matches the qc_MMDDYY format using regular expression
    if (grepl("^qc_\\d{6}$", qc_value)) {
      # Convert the MMDDYY value to a date using lubridate package
      date_value <- mdy(substr(qc_value, 4, 9))
      # Assign the date value to the date column
      df[i, "date"] <- date_value
      df[i, "date_string"] <- as.character(date_value)
    }
  }
  # Discard the rows that do not have a valid date value
  df <- df[!is.na(df$date), ]
  # Create another new column called dicom_count
  df$dicom_count <- NA
  # Loop through the rows of the data frame
  for (i in 1:nrow(df)) {
    # Extract the number at the end of the seventh column using regular expression
    dicom_value <- df[i, 9]
    dicom_count <- as.numeric(gsub("DICOM: (\\d+)", "\\1", dicom_value))
    # Assign the dicom_count to the dicom_count column
    df[i, "dicom_count"] <- dicom_count
  }
  # Return the processed data frame
  return(df)
}

df <- process_qc_dicoms('qc_dicoms.txt')

df2 <- df[,c(6,8,10,11,12)]
colnames(df2) <- c('session','series','date','date_string','dicom_count')
saveRDS(df2, file.path('~/R-Drive/Bartolotti_J/qc_dicomcount.RDS'))

#ON PC
df <- readRDS('//kumc.edu/data/Research/Hoglund/Bartolotti_J/qc_dicomcount.RDS')

library(ggplot2)
library(ggbeeswarm)

# Loop through the unique values in series
for (s in unique(df$series)) {
  # Subset the data frame by the current series value
  sub_df <- df[df$series == s, ]
  # Make a table of the occurrences of each dicom_count value
  tab <- table(sub_df$dicom_count)
  # Print the table with a title
  cat("Table for series =", s, "\n")
  print(tab)
  cat("\n")
}

#113 sessions in this df.
target_dicomcount <- c( #1-11
  3,
  176,
  7,
  220,
  240,
  15,
  105,
  105,
  1,
  1,
  200
  )

df$target_dicomcount <- target_dicomcount[df$series]
df$target_delta <- df$dicom_count - df$target_dicomcount

ggplot(df, aes(x = series, y = target_delta)) + theme_bw() + geom_quasirandom()

aggdf <- aggregate(target_delta ~ session, data = df, FUN = sum)
#These sessions have less than the standard number of dicoms for at least one series
#session target_delta
#5   qc_011723          -44
#35  qc_032723          -87
#41  qc_041023           -1
#45  qc_042522           -2
#47  qc_050222          -85
#52  qc_051622          -52
#58  qc_060622          -53
#103 qc_112122          -24
#111 qc_121922          -81

aa <- df[df$session %in% aggdf$session[aggdf$target_delta == 0] & df$series == 1,]

