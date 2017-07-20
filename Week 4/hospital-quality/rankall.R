#!/usr/bin/Rscript
# *******************************************
# example usage:
#> setwd("[source code directory path]")
#> source("rankall.R")
#> head(rankall("heart attack", 20), 10)
#> tail(rankall("pneumonia", "worst"), 3)
#> tail(rankall("heart failure"), 10)
# *******************************************
# @author Pranay Sarkar

# Function to find the best hospital in all state for a given outcome
rankall <- function(outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv")
	#Fetching distinct states from data
	States <- levels(factor(data[, 7]))
	# defining all possible outcomes
	Outcomes <- c("heart attack", "heart failure", "pneumonia")

	# validity check for state, outcome & num
	if ((outcome %in% Outcomes) == FALSE) {
		stop(print("Invalid outcome."))
	}
	if (is.numeric(num) == TRUE) {
		if (length(data[,2]) < num) { return(NA) }
	}
	
	# defining mapping of outcome column w.r.t. input
	colNumber <- if (outcome == "heart attack") {11}
	else if (outcome == "heart failure") {17}
	else {23}
	# considering output as numeric and Hospital.Name as character
	data[, colNumber] <- suppressWarnings(as.numeric(levels(data[, colNumber])[data[, colNumber]]))
	data[, 2] <- as.character(data[, 2])
	
	# initializing empty AllStatesHospitalRanking:
	AllStatesHospitalRanking <- vector()
	
	# computing AllStatesHospitalRanking.
	for(i in 1:length(States)) {
		selectedData <- subset(data, State == States[i])
		selectedColumns <- suppressWarnings(as.numeric(selectedData[,colNumber]))
		selectedData <- selectedData[!(is.na(selectedColumns)), ]
		
		# rank hospital by 'outcome':
		HospitalRankPerState <- selectedData[order(selectedData[, colNumber], selectedData[, 2]), ]
		if(num == "best") {numRank = 1}
		else if(num == "worst") {
			numRank = nrow(HospitalRankPerState)
		}
		else{ numRank = num }
		HospitalsRanked <- HospitalRankPerState[numRank, 2]
		AllStatesHospitalRanking <- append(AllStatesHospitalRanking, c(HospitalsRanked, States[i]))
	}
	# AllStatesHospitalRanking as data frame along with column names and row names:
	AllStatesHospitalRanking <- as.data.frame(matrix(AllStatesHospitalRanking, length(States), 2, byrow = TRUE))
	colnames(AllStatesHospitalRanking) <- c("hospital", "state")
	rownames(AllStatesHospitalRanking) <- States

	return(AllStatesHospitalRanking)
}
