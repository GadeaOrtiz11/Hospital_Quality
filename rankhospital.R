

rankhospital <- function(state, outcome, num){
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
    # Subset data based on state
    data_state <- data[which(data$State == state),]
    if(outcome == "heart attack"){
        data_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data_state[,11])
        Mortality_Heart_Attack <- data_state[complete.cases(data_state[, c(2,11)]),c(2,11)]
        Mortality_Heart_Attack <- Mortality_Heart_Attack[order(Mortality_Heart_Attack[,2], Mortality_Heart_Attack[,1]),]
        if(num == "best"){
            num = 1
        }
        if (num == "worst"){
            num = dim(Mortality_Heart_Attack)[1]
        }
        Final <- Mortality_Heart_Attack[num,1]
    }
    if(outcome == "heart failure"){
        data_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data_state[,17])
        Mortality_Heart_Failure <- data_state[complete.cases(data_state[, c(2,17)]), c(2,17)]
        Mortality_Heart_Failure <- Mortality_Heart_Failure[order(Mortality_Heart_Failure[,2], Mortality_Heart_Failure[,1]),]
        if(num == "best"){
            num = 1
        }
        if (num == "worst"){
            num = dim(Mortality_Heart_Failure)[1]
        }
        Final <- Mortality_Heart_Failure[num,1]
    }
    if(outcome == "pneumonia"){
        data_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data_state[,23])
        Mortality_Pneumonia <- data_state[complete.cases(data_state[, c(2,23)]), c(2,23)]
        Mortality_Pneumonia <- Mortality_Pneumonia[order(Mortality_Pneumonia[,2], Mortality_Pneumonia[,1]),]
        if(num == "best"){
            num = 1
        }
        if (num == "worst"){
            num = dim(Mortality_Pneumonia)[1]
        }
        Final <- Mortality_Pneumonia[num,1]
    }
    print(Final)
}


#Test 
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)