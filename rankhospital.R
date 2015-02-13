rankhospital <- function(state, outcome, num="best") {
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
        
        data[, outcome_col] <- as.numeric(data[, outcome_col])
        data <- data[data$State == state & !is.na(data[, outcome_col]),
                     c(hospital_name_col, outcome_col)]
        if (nrow(data) == 0) {
                stop("invalid state")
        }
        
        ## Return hospital name in that state with the given rank
        ## Now, the first column is the hospital name and the second the outcome rate
        data <- data[order(data[2], data[1]), ]
        
        if (num == "best") {
                return(data[1, 1])
        } else if (num == "worst") {
                return(data[nrow(data), 1])
        } else {
                return(data[as.integer(num), 1])
        }
}