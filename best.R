
## best() function takes two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.
 

best <- function(state, outcome){
    # Load data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Check for valid state
    if(!any(unique(data$State) == state)){
        stop("invalid state")
    }
    # Check for valid outcome
    if(!any(c("heart attack", "heart failure", "pneumonia") == outcome)) {
        stop("invalid outcome")
    }
    # Subset data ased on state
    data_state <- data[which(data$State == state),]
    # Determine hospital name with lowest mortality rate for the outcome of heart attack
    if(outcome == "heart attack"){
        Mortality_Heart_Attack <- as.numeric(data_state[, 11])
        Hospital_Lowest_Mortality <- data_state[which.min(Mortality_Heart_Attack), 2]
        Final <- sort(Hospital_Lowest_Mortality)[1]
    }
    # Determine hospital name with lowest mortality rate for the outcome of heart failure
    if(outcome == "heart failure"){
        Mortality_Heart_Failure <- as.numeric(data_state[, 17])
        Hospital_Lowest_Mortality <- data_state[which.min(Mortality_Heart_Failure), 2]
        Final <- sort(Hospital_Lowest_Mortality)[1]
    }
    # Determine hospital name with lowest mortality rate for the outcome of pneumonia
    if (outcome == "pneumonia"){
        Mortality_Pneumonia <- as.numeric(data_state[, 23])
        Hospital_Lowest_Mortality <- data_state[which.min(Mortality_Pneumonia), 2]
        Final <- sort(Hospital_Lowest_Mortality)[1]
    }
    # Print hospital name
    print(Final)
}

## Test Function

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")