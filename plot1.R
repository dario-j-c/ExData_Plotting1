plot1 <- function(datasource = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                           archivename = "exdata_data_household_power_consumption.zip", foldername = "exdata_data_household_power_consumption", filename = "household_power_consumption.txt") {
  
  ## This is the script for downloading the data, tidying it, and creating plot1.png
  ## Overview comments will be found with "##" preceding them.
  ## Granular comments will be found with "#" preceding them and where possible shall be to the side of the code.
  ## Important Note: This script is a function, meaning it must first be sourced then it can be called as any other function.
  ## The output of this script is the file "plot1.png".
  
  ## Load needed packages
  pkg <- c("tidyverse", "fs")                                                   # My list of desired packages
  suppressMessages(                                                             # Suppress messages while allowing warning and error messages
    lapply(pkg, require, character.only = TRUE))                                # Check and load desired packages
  
  
  ## Download raw data
  oldwd <- getwd()                                                              # Save the current working directory for later usage
  
  zippaths <- dir_ls(recurse = TRUE,                                            # Get paths for files in sub directories which match archive name
                     type = "file",
                     glob = paste0("*",archivename))       
  filepaths <- dir_ls(recurse = TRUE,                                           # Get paths for directories in sub directories which match folder name
                     type = "file",
                     glob = paste0("*",filename))
  
  
  if ( !length(zippaths) && !length(filepaths)){                                # If the archive name & folder name isn't found, download archive
    download.file(datasource, archivename, method="curl")
    unzip(archivename)
  } else if (!length(filepaths)) {                                              # If the folder name isn't found, unzip archive and change working directory
    unzip(archivename)                                                          # Go to directory to prepare to work with files
  } else {}
  
  
  ## Assemble raw data
  
  # Load files from folders
  rawdata <- read_delim(filename, 
                        ";",
                        escape_double = FALSE,
                        col_types = cols(Date = col_date(format = "%d/%m/%Y"),
                                         Time = col_time(format = "%H:%M:%S"),
                                         .default = "d"),
                        trim_ws = TRUE)
  
  setwd(oldwd)                                                                  # Return to the starting working directory
  
  ## filter for requested dates
  data <- rawdata %>%
    filter(between(Date,
                   lubridate::ymd("2007-02-01"),
                   lubridate::ymd("2007-02-02")))
  
  ## Plot "plot1" using base R
  png(filename="plot1.png",
      width=480, height=480, units="px")
  with(data, hist(Global_active_power,
                     #breaks = 14,
                     col = "red",
                     freq = TRUE,
                     main = "Global Active Power",
                     xlab = "Global Active Power (kilowatts)",
                     ylab = "Frequency"))
  dev.off()
}