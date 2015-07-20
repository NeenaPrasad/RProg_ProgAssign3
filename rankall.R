rankall <- function(outcome, num = "best"){
        #Read outcome data
        
        #For each state, find the hospital of the given rank
        
        #Return a data frame with the hospital names and the
        #abbreviated state name
        
        valid_outcome <- c("heart attack", "heart failure","pneumonia")
        
        if (! outcome %in% valid_outcome){
                stop("invalid outcome")
        }
        
        outcome_meas <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character",na.strings = "Not Available")
        
        
        source("rankhospital.R")
        
        valid_states <- unique(outcome_meas$State)
        valid_states <- valid_states[order(valid_states)]
        
        dframe <- data.frame(hospital = character(),state = character())
        
        for (state_i in valid_states){
                result <-  rankhospital(state_i,outcome,num)
                dframe <- rbind(dframe, data.frame("hospital" = result, "state" = state_i))
        }
        
        row.names(dframe) <- dframe$state
        dframe
}