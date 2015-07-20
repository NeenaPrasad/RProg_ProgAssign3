rankhospital <- function(state, outcome, num = "best"){
        #read outcome data
        
        #check the state and outcome are valid
        
        #Return hospital name in that state with the given rank
        #30-day death rate
        
        outcome_meas <- read.csv("outcome-of-care-measures.csv",
                                 colClasses = "character",na.strings = "Not Available")
        
        
        valid_outcome <- c("heart attack", "heart failure","pneumonia")
        colNames_av <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                         "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        
        if (! state %in% outcome_meas$State){
                stop("invalid state")      
        }
        
        if (! outcome %in% valid_outcome){
                stop("invalid outcome")
        }
        
        data <- outcome_meas[outcome_meas$State == state,]
        
        
        colName_used <- colNames_av[match(outcome,valid_outcome)]
        data[,colName_used] <- as.numeric(data[,colName_used])
        sorted.data <- data[order(data[,colName_used],data[,"Hospital.Name"],na.last = NA),]
        
        
        if (num == "best") num = 1
        else if (num == "worst") num = nrow(sorted.data)
       
        
        sorted.data[num,]$Hospital.Name
        
        
        
}


## Problem occurs when returned hospitals with same rate
## Needs alphabetical ordering to prevent the problem

