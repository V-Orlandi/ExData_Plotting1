plot2 <- function(a = NULL) {
  
  ## Pass dataset to not read file again!!
  if (is.null(a)) {
    ## Read file - "Note that in this dataset missing values are coded as ?."
    data <- read.table(file="household_power_consumption.txt", header=TRUE, sep=";", na.strings="?")
  } else {
    if (is.data.frame(a)){
      if (nrow(a) == 2075259 & ncol(a) == 9) {
        data <- a
      }
    } else {
      stop("Invalid dataset")
    }
  }
  
  ## Subset data - "We will only be using data from the dates 2007-02-01 and 2007-02-02." 
  subset <- data[as.character(data$Date) %in% c('2/2/2007','1/2/2007'),]
  
  ## Create a new column with strings converted to dates and times 
  ## - "You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions."
  subset$DateTime <- strptime(paste(subset$Date,subset$Time),format="%d/%m/%Y %H:%M:%S")

  ## Initializing a new plot - "Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels."
  png(filename="ExData_Plotting1/plot2.png", width=480, height=480, units="px", bg="transparent")
  
  ## Make a plot
  plot(x=subset$DateTime, xlab="", y=subset$Global_active_power, ylab="Global Active Power (kilowatts)", type="l", axes=TRUE)
  
  ## Close graphics device and save file
  dev.off()
  
  ## Notes: "Qui" = "Thu", "Sex" = "Fri" and "Sab" = "Sat". System in Portuguese
}