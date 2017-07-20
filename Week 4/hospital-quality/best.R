#!/usr/bin/Rscript
# *******************************************
# example usage:
#> setwd("[source code directory path]")
#> source("best.R")
#> best("SC", "heart attack")
#> best("NY", "pneumonia")
#> best("AK", "pneumonia")
# *******************************************
# @author Pranay Sarkar

# Function to find the best hospital name in a state with lowest 30-day death rate
best <- function(state, outcome) {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	States <- levels(factor(data[, 7]))
	Outcomes <- c("heart attack", "heart failure", "pneumonia")
  
	# checking validity of the input: state, outcome
	if ((state %in% States) == FALSE) {
		stop(print("Invalid state."))
	}
	else if ((outcome %in% Outcomes) == FALSE) {
		stop(print("Invalid outcome."))
	}
  
	# defining mapping of outcome column w.r.t. input
	colNumber <- if (outcome == "heart attack") {11}
	else if (outcome == "heart failure") {17}
	else {23}
  
	# slicing & cleaning data based on the requested inputs
	# based on state
	selectedData <- subset(data, State == state)
	#based on outcome
	selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
    
	# re-selecting data based on requested input 'outcome' and clean it from all 'na' values
	selectedData <- selectedData[!(is.na(selectedColumns)), ]
  
	# select the necessary columns and rows based on cleaned data
	selectedColumns <- as.numeric(selectedData[, colNumber])
	selectedRows <- which(selectedColumns == min(selectedColumns))

	selectedHospital <- selectedData[selectedRows, 2]
	sortedHospital <- sort(selectedHospital)
  
	sortedHospital
  
}
