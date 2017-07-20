#!/usr/bin/Rscript
# *******************************************
# example usage:
#> setwd("[source code directory path]")
#> source("rankhospital.R")
#> rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
#> rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
#> rankhospital("MN", "heart attack", 5000)
#[1] NA
# *******************************************
# @author Pranay Sarkar

#Function to find the best hospital in a state with a given rank
rankhospital <- function(state, outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv")
	#Fetching distinct states from data
	States <- levels(factor(data[, 7]))
	# defining all possible outcomes
	Outcomes <- c("heart attack", "heart failure", "pneumonia")
	
	# validity check for state, outcome & num
	if ((state %in% States) == FALSE) {
		stop(print("Invalid state."))
	}
	else if ((outcome %in% Outcomes) == FALSE) {
		stop(print("Invalid outcome."))
	}
	if (is.numeric(num) == TRUE) {
		if (length(data[,2]) < num) { return(NA) }
	}
	# defining mapping of outcome column w.r.t. input
	colNumber <- if (outcome == "heart attack") {11}
	else if (outcome == "heart failure") {17}
	else {23}

	# slicing & cleaning data based on the requested inputs
	data[, colNumber] <- suppressWarnings(as.numeric(levels(data[, colNumber])[data[, colNumber]]))
	data[, 2] <- as.character(data[, 2])    
	# based on state
	selectedData <- subset(data, State == state)
	# re-selecting data based on requested input 'outcome' and clean it from all 'na' values
	selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
	selectedData <- selectedData[!(is.na(selectedColumns)), ]
	#based on outcome
	rankedHospital <- selectedData[order(selectedData[, colNumber], selectedData[, 2]), ]
    
	# if input 'num' == 'best' then then 'numRank' = first;
	if(num == "best") {
		numRank = 1
	}
    
	# if input 'num' == 'worst' then 'numRank' = last;
	else if(num == "worst") {
		numRank = nrow(rankedHospital)
	}
    
	# if input 'num' == number then 'numRank' = 'num'.
	else{ numRank = num }
    
	return(rankedHospital[numRank, 2])
}
