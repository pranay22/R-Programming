#!/usr/bin/Rscript
# *******************************************
# example usage:
#> setwd("[this source code directory path]")
#> source("complete.R")
#> complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
#> complete("specdata", 54)
#> complete("specdata", 332:1)
#*******************************************
# @author Pranay Sarkar

# Compute the number of observations with complete result, with no 'NA' values
# 'directory' - location of the CSV files
# 'id' - id of each file
complete <- function(directory, id = 1:332) {
	results <- data.frame(id=numeric(0), nobs=numeric(0))
    	for(monitor in id){
        	path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        	monitor_data <- read.csv(path)
        	interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
        	interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
        	nobs <- nrow(interested_data)
        	results <- rbind(results, data.frame(id=monitor, nobs=nobs))
    	}
    	results
	#Return format will look like:
	# id nobs
	# 1  117
	# 2  1041
	# ...
	# (Where 'is' is monitor ID and 'nobs'is number of complete cases)
}
