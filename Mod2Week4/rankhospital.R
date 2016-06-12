rankhospital <- function(state, outcome, num = 'best') {
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
		statesubset <- statesubset[order(statesubset[[outcome]], statesubset$hospital, na.last = NA, decreasing = FALSE), ]
		if (num == 'best') {
			num <- 1
		} else if (num == 'worst') {
			num <- nrow(statesubset)
		} else {
			num <- as.numeric(num)
		}
		
		statesubset[num, 1]
	}
}