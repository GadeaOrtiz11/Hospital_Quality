
## best() function takes two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.


best <- function(state, outcome){
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!any(unique(data$State) == state)){
        stop("invalid state")
    }
    if(!any(c("heart attack", "heart failure", "pneumonia") == outcome)) {
        stop("invalid outcome")
    }
    data_state <- data[which(data$State == state),]
    if(outcome == "heart attack"){
        
        Mortality_Heart_Attack <- as.numeric(data_state[, 11])
        Hospital_Lowest_Mortality <- data_state[which.min(Mortality_Heart_Attack), 2]
        Final <- sort(Hospital_Lowest_Mortality)[1]
    }
    if(outcome == "heart failure"){
        
        Mortality_Heart_Failure <- as.numeric(data_state[, 17])
        Hospital_Lowest_Mortality <- data_state[which.min(Mortality_Heart_Failure), 2]
        Final <- sort(Hospital_Lowest_Mortality)[1]
    }
    if (outcome == "pneumonia"){
        
        Mortality_Pneumonia <- as.numeric(data_state[, 23])
        Hospital_Lowest_Mortality <- data_state[which.min(Mortality_Pneumonia), 2]
        Final <- sort(Hospital_Lowest_Mortality)[1]
    }
    print(Final)
}

## Test Function

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")