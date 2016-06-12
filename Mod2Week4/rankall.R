rankall <- function(outcome, num = 'best') {
	outcomes <- read.csv('outcome-of-care-measures.csv', na.strings = 'Not Available', stringsAsFactors = FALSE)
	columns <- c(2, 7, 11, 17, 23)
	outcomes <- outcomes[columns]
	names(outcomes) <- c('hospital', 'states', 'heart attack', 'heart failure', 'pneumonia')
	
	if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
		stop('invalid outcome')
	} else {
		outspec <- outcomes[c('hospital', 'states', outcome)]
		outspec <- na.omit(outspec)
		outspec <- outspec[order(outspec$states, outspec[[outcome]], outspec$hospital), ]
		statenames <- sort(unique(outspec[ ,'states']))
	}
	
	stategrouping <- function(state) {
		statesubset <- subset(outspec, states == state)
		if (num == 'best') {
			num <- 1
			statesubset[num, c('hospital', 'states')]
		} else if (num == 'worst') {
			num <- nrow(statesubset)
			statesubset[num, c('hospital', 'states')]
		} else if (num > nrow(statesubset)) {
			c(NA, state)
		} else {
			num <- as.numeric(num)
			statesubset[num, c('hospital', 'states')]
		}
		
	}
	
	stateframe <- as.data.frame(do.call(rbind, lapply(statenames, stategrouping)), row.names = statenames)
	colnames(stateframe) <- c('hospital', 'state')
	stateframe
}