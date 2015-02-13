rankall <- function(outcome, num="best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        hospital_name_col <- 2
        if (outcome == "heart attack") {
                outcome_col <- 11
        } else if (outcome == "heart failure") {
                outcome_col <- 17
        } else if (outcome == "pneumonia") {
                outcome_col <- 23
        } else {
                stop("invalid outcome")
        }
        
        ## Convert outcome data to numeric and filter away NAs
        data[, outcome_col] <- as.numeric(data[, outcome_col])
        data <- data[!is.na(data[, outcome_col]), ]
        
        ## Split into per-state dataframes
        perstatedata <- split(data, data$State)
        statenames <- names(perstatedata)
        hospitalnames <- vector("character", length(statenames))
        
        for (i in 1:length(statenames)) {
                statedata <- perstatedata[[statenames[i]]]
                statedata <- statedata[order(statedata[outcome_col], statedata[hospital_name_col]), ]
                
                if (num == "best") {
                        hosname <- statedata[1, hospital_name_col]
                } else if (num == "worst") {
                        hosname <- statedata[nrow(statedata), hospital_name_col]
                } else {
                        hosname <- statedata[as.integer(num), hospital_name_col]
                }
                
                hospitalnames[i] <- hosname
        }
        
        data.frame(hospital = hospitalnames, state = statenames)
}