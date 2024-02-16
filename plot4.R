#Setting the working directory
setwd("~/Documents/RStudio/Electric Power Consumption")

# Load necessary packages
library(data.table) # For efficient data reading and manipulation
library(ggplot2)
library(scales) # for formatting dates on x-axis
library(reshape2) # for melting data into long format


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

# First plot (top-left)
p_global_active_power <- ggplot(specific_dates_data, aes(x = DateTime, y = Global_active_power)) +
  geom_line(color = UNM.palette[1]) +
  scale_x_datetime(labels = date_format("%a"), breaks = date_breaks("1 day")) + # Format x-axis labels as abbreviated weekdays
  theme_classic() +
  labs(x = "", y = "Global Active Power (kilowatts)") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_text(size = 8))

# Second plot (top-right)
p_voltage <- ggplot(specific_dates_data, aes(x = DateTime, y = Voltage)) +
  geom_line(color = UNM.palette[2]) +
  scale_x_datetime(labels = date_format("%a"), breaks = date_breaks("1 day")) + # Format x-axis labels as abbreviated weekdays
  theme_classic() +
  labs(x = "DateTime", y = "Voltage (volts)") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_text(size = 8))

# Thirth plot (bottom-left)
p_sub_metering <- ggplot(specific_dates_data) +
  geom_line(aes(x = DateTime, y = Sub_metering_1, color = "Sub_metering_1")) +
  geom_line(aes(x = DateTime, y = Sub_metering_2, color = "Sub_metering_2")) +
  geom_line(aes(x = DateTime, y = Sub_metering_3, color = "Sub_metering_3")) +
  scale_color_manual(values = UNM.palette[c(3, 4, 5)]) +
  scale_x_datetime(labels = date_format("%a"), breaks = date_breaks("1 day")) + # Format x-axis labels as abbreviated weekdays
  theme_classic() +
  labs(x = "", y = "Energy sub metering") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = c(0.8, 0.8), # Position inside the plot area
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 8)
  )  
# Fourth plot (bottom-right)
p_global_reactive_power <- ggplot(specific_dates_data, aes(x = DateTime, y = Global_reactive_power)) +
  geom_line(color = UNM.palette[6]) +
  theme_classic() +
  labs(x = "DateTime", y = "Global Reactive Power (kilowatts)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), axis.text.y = element_text(size = 8))

# Combine the individual plots into a list
plot_list <- list(p_global_active_power, p_voltage, p_sub_metering, p_global_reactive_power)

# Use 'cowplot' to arrange the plots into a single panel
library(cowplot)

# Align the plots and combine them
combined_plot <- plot_grid(plotlist = plot_list, align = 'v', ncol = 2)

# Save the plot to the 'output' subdirectory
ggsave("plot4.png", plot = combined_plot, width = 8, height = 8, dpi = 600, units = "in")

