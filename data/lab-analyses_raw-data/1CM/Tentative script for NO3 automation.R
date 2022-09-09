######################################################################################
# Nitrate (NO3) and total dissolved nitrogen (TDN) via second derivative spectroscopy
######################################################################################

# by Sarah King 2017

# 'signal' package required for this script:

library(signal)

# The two files below must be prepared manually before loading and running the 
# automated part of the script. Both files must be placed in the working directory; 
# this will also be where the output file and second derivative plots will be created.

# Absorbance data setup:

#   (1) Save absorbance data as spreadsheet (.csv)
#   (2) Remove Baseline wavelength and absorbance columns
#   (3) Label sample absorbance columns with sample name. Wavelength columns can be 
#       left blank.
#   (4) Delete the top row. The top row should now be the column names (sample names)
#       with the data below.
#   (5) Save absorbance data as "TDN_AbsData.csv"

# Change Log setup:

#   (1) Make a column of the sample names used in the Absorbance data. These must match 
#       the column names in absorbance data exactly.
#   (2) Name this column "SAMPLE"
#   (3) Save as "TDN_ChangeLog.csv"

# NOTE: Don't start column names with numbers, R doesn't like this! 
#       Don't use commas in column names.
#       Column names in R are case-sensitive.


# Read absorbance and change log files:

TDN.AbsData <- read.csv("TDN_AbsData.csv")[1:383,]
TDN.ChangeLog <- read.csv("TDN_ChangeLog.csv", colClasses = "character")

######################################################################################

# Automated processing of data begins here.

TDN.Output <- NULL

for (j in 1:nrow(TDN.ChangeLog)){
  Abs.raw <- as.matrix(TDN.AbsData[,TDN.ChangeLog[j,]])
  
  # Calculate smoothed second derivative using second order Savitzky-Golay filter with filter size of 141 (~21 nm)  
  Abs.deriv2 <- sgolayfilt(Abs.raw, p = 2, n = 133, m = 2, ts = 1)
  Results <- cbind(as.vector(TDN.AbsData[,1]),Abs.deriv2)
  
  # Print plot of smoothed second derivative from 200 to 240 nm
  png(filename=paste0(TDN.ChangeLog[j,],".png"),
      units = "px",
      width = 400,
      height = 250
  )
  plot(Results[65:310,1], Results[65:310,2], type = "l", main = TDN.ChangeLog[j,1])
  dev.off()
      
  # Find max value of second derivative between 222 and 226 nm
  peak.range <- Results[155:181,]
  peak <- max(peak.range[,2])
  peak.WL <- peak.range[which(peak.range[,2] == peak),1]
  data.j <- as.data.frame(cbind((as.character(TDN.ChangeLog[j,])),peak,peak.WL))
  TDN.Output <- as.data.frame(rbind(TDN.Output,data.j))
  
}

# Remove objects, write output file and clear workspace

rm(j,Abs.raw,Abs.deriv2,Results,peak.range,peak,peak.WL,data.j)

write.table(TDN.Output,file="TDN_Output.csv",row.names=FALSE,sep=",")

rm(list=ls())
