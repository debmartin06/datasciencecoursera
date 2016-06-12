best <- function(state, outcome) {
	outcomes <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', stringsAsFactors = FALSE)
	columns <- c(2, 7, 11, 17, 23)
	outcomes <- outcomes[columns]
	names(outcomes) <- c('hospital', 'states', 'heart attack', 'heart failure', 'pneumonia')
	
	if (!state %in% outcomes$states) {
		stop('invalid state')
	} else if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
		stop('invalid outcome')
	} else {
		statesubset <- subset(outcomes, states == state, select = c('hospital', 'states', outcome))
		statesubset <- statesubset[order(statesubset[[outcome]], na.last = NA, decreasing = FALSE), ]
		statesubset[1, 1]
	}
}