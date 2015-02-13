best <- function(state, outcome) {
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
        
        data <- data[data$State == state & !is.na(data[, hospital_name_col]),
                     c(hospital_name_col, outcome_col)]
        if (nrow(data) == 0) {
                stop("invalid state")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## Now, the first column is the hospital name and the second the outcome rate
        data[, 2] <- as.numeric(data[, 2])
        data[order(-data[2], data[1], decreasing = TRUE), ][1, 1]
}