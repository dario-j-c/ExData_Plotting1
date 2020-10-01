plot4 <- function(datasource = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                  archivename = "exdata_data_household_power_consumption.zip", filename = "household_power_consumption.txt") {
  
  ## This is the script for downloading the data, tidying it, and creating plot4.png
  ## Overview comments will be found with "##" preceding them.
  ## Granular comments will be found with "#" preceding them and where possible shall be to the side of the code.
  ## Important Note: This script is a function, meaning it must first be sourced then it can be called as any other function.
  ## The output of this script is the file "plot4.png".
  
  ## Load needed packages
  pkg <- c("tidyverse", "fs")                                                   # My list of desired packages
  suppressMessages(                                                             # Suppress messages while allowing warning and error messages
    lapply(pkg, require, character.only = TRUE))                                # Check and load desired packages
  
  
  ## Download raw data
  oldwd <- getwd()                                                              # Save the current working directory for later usage
  
  zippaths <- dir_ls(recurse = TRUE,                                            # Get paths for files in sub directories which match archive name
                     type = "file",
                     glob = paste0("*",archivename))       
  filepaths <- dir_ls(recurse = TRUE,                                            # Get paths for directories in sub directories which match folder name
                      type = "file",
                      glob = paste0("*",filename))
  
  
  if ( !length(zippaths) && !length(filepaths)){                                 # If the archive name & folder name isn't found, download archive
    download.file(datasource, archivename, method="curl")
    unzip(archivename)
  } else if (!length(filepaths)) {                                               # If the folder name isn't found, unzip archive and change working directory
    unzip(archivename)                                                           # Go to directory to prepare to work with files
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
  
  
  ## Confirmed using this code that filtering between 2007-02-01 and 2007-02-02
  ## will not give Thu, Fri and Sat., instead it will give only Thu and Fri.
  ## I'll have to add Sat manually
  data <- data %>%
    mutate(Day = lubridate::wday(Date, label = TRUE)) %>%
    select(Date, Day, Time, everything())
  ## print(table(data$Day))                                                    # code to check days.
  
  days <- group_by(data, Day) %>%
    summarise(count = n())
  
  ## Plot "plot4" using base R
  png(filename="plot4.png",
      width=480, height=480, units="px")
  
  par(mfrow=c(2,2))
  
  # Plot graph 1
  with(data, plot(Global_active_power,
                  type = "l",
                  xaxt = "n",
                  xlab = waiver(),
                  ylab = "Global Active Power (kilowatts)"))
  axis(side = 1,
       at=c(1, days[1,2] + 1, days[2,2] +  days[1,2]),
       labels=c("Thu", "Fri", "Sat"))
  
  # Plot graph 2
  with(data, plot(Voltage,
                  type = "l",
                  xaxt = "n",
                  xlab = "datetime",
                  ylab = "Voltage"))
  axis(side = 1,
       at=c(1, days[1,2] + 1, days[2,2] +  days[1,2]),
       labels=c("Thu", "Fri", "Sat"))  
  
  # Plot graph 3
  matplot( data[,8:10],
           type = "l",
           lty = 1,
           col = c( "black", "red", "blue"),
           xaxt = "n",
           xlab = waiver(),
           ylab = "Energy sub metering")
  axis(side = 1,
       at=c(1, days[1,2] + 1, days[2,2] +  days[1,2]),
       labels=c("Thu", "Fri", "Sat"))
  legend("topright",
         lty=1,
         col=c("black", "red", "blue"),
         bty="n",
         pt.bg,
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  # Plot graph 4
  with(data, plot(Global_reactive_power,
                  type = "l",
                  xaxt = "n",
                  xlab = "datetime",
                  ylab = "Global_reactive_power"))
  axis(side = 1,
       at=c(1, days[1,2] + 1, days[2,2] +  days[1,2]),
       labels=c("Thu", "Fri", "Sat"))
  
  dev.off()
}