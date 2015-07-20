leastVal <- function(datafile, colNum){
        minval <- min(as.numeric(as.character(datafile[,colNum])),na.rm = TRUE)
        row_val <- which(as.numeric(as.character(data[,colNum])) == minval)
        sort(as.character(data[row_val,2]))[1]
}

best <- function(state, outcome){
        ## read outcome data
        
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        outcome_meas <- read.csv("D:/Personal/RProg/outcome-of-care-measures.csv")
        
        
        
        if (! state %in% outcome_meas$State){
                stop(paste("Error in best(",state,", ",outcome,"): invalid state",sep = ""))      
        }
        else{
                data <- outcome_meas[outcome_meas$State == state,]
                
                
                if (outcome == "heart attack"){
                        result <- leastVal(data,11)        
                }
                else if (outcome == "heart failure"){
                        result <- leastVal(data,17)        
                }
                else if (outcome == "pneumonia"){
                        result <- leastVal(data,23)       
                }
                else {
                        stop(paste("Error in best(",state,", ",outcome,") : invalid outcome",sep=""))       
                }
        }
        
        
        
        result

}