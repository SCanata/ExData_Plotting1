#Setting the working directory
setwd("~/Documents/RStudio/Electric Power Consumption")

# Load necessary packages
library(data.table) # For efficient data reading and manipulation
library(ggplot2)

# Define the UNM palette for later use in plots
UNM.palette <- c("#ba0c2f", "#007a86", "#a7a8aa", "#63666a", "#ffc600", "#ed8b00", "#c05131", "#d6a461", "#a8aa19", "#8a387c", "black")

# Function to read specific dates from a semi-colon separated file
read_specific_dates <- function(file_name, start_date, end_date) {
  # Define column names manually, based on dataset description
  column_names <- c("Date", "Time", "Global_active_power", "Global_reactive_power", 
                    "Voltage", "Global_intensity", "Sub_metering_1", 
                    "Sub_metering_2", "Sub_metering_3")
  
  # Read the entire dataset (consider setting nrows for testing)
  data <- fread(file_name, sep = ";", na.strings = "?", col.names = column_names)
  
  # Filter rows by converting 'Date' to Date format and checking if within the desired range
  data[, Date := as.Date(Date, format = "%d/%m/%Y")]
  filtered_data <- data[Date >= as.Date(start_date) & Date <= as.Date(end_date)]
  
  # Convert 'Time' into POSIXct DateTime format (combining Date and Time)
  filtered_data[, DateTime := as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S")]
  
  # Optionally, remove original 'Date' and 'Time' columns
  # filtered_data[, c("Date", "Time") := NULL]
  
  return(filtered_data)
}

#Load data
specific_dates_data <- read_specific_dates("household_power_consumption.txt", "2007-02-01", "2007-02-02")

# Print the first few rows to verify
head(specific_dates_data)


# Create the histogram plot
p <- ggplot(specific_dates_data, aes(x = Global_active_power)) +
  geom_histogram(fill = UNM.palette[1], binwidth = 0.5) + # Adjust binwidth as needed
  theme_classic() +
  labs(x = "Global Active Power (kilowatt)", y = "Frequency") +
  theme(text = element_text(size = 4)) # Adjust text size as needed to ensure legibility


# Save the plot to the 'output' subdirectory
ggsave("plot1.png", plot = p, width = 2, height = 2, dpi = 600, units = "in")

