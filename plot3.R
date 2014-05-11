plot3 <- function(a = NULL) {
  
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
  png(filename="ExData_Plotting1/plot3.png", width=480, height=480, units="px", bg="transparent")
  
  ## Make a plot
  # Sub_metering_1
  plot(x=subset$DateTime, xlab="", y=subset$Sub_metering_1, ylab="Energy sub metering", type="l", col="black")
  
  # Sub_metering_2
  lines(x=subset$DateTime, y=subset$Sub_metering_2, type="l", col="red")
  
  # Sub_metering_3
  lines(x=subset$DateTime, y=subset$Sub_metering_3, type="l", col="blue")
  
  # Draw a box with legend
  legend("topright", legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), lwd = 1, col = c("black", "red", "blue"))
  
  ## Close graphics device and save file
  dev.off()
  
  ## Notes: "Qui" = "Thu", "Sex" = "Fri" and "Sab" = "Sat". System in Portuguese
}