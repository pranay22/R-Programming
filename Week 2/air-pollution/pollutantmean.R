#!/usr/bin/Rscript
# *******************************************
# example usage:
#> setwd("[this source code directory path]")
#> source("pollutantmean.R")
#> pollutantmean("specdata", "sulfate", 1:10)
#> [1] 4.064128
#> pollutantmean("specdata", "nitrate", 70:72)
#> [1] 1.706047
#> pollutantmean("specdata", "sulfate", 34)
#> [1] 1.477143
#> pollutantmean("specdata", "nitrate")
#> [1] 1.702932
# *******************************************
# @author Pranay Sarkar

# Compute the mean of each type of pollutant
# 'directory' - location of the CSV files
# 'pollutant' - type of pollutant
# 'id' - id of each file
# Return the mean of the pollutant across all monitors list (no rounding off)

pollutantmean <- function(directory, pollutant, id = 1:332) {  
	means <- c()
	for(monitor in id){
        	path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        	monitor_data <- read.csv(path)
        	interested_data <- monitor_data[pollutant]
        	means <- c(means, interested_data[!is.na(interested_data)])
    	}
    
    	mean(means)
}
