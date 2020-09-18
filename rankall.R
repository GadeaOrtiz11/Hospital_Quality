rankall <- function(outcome, num = "best"){
    rank <- num
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Check for valid outcome
    if(!any(c("heart attack", "heart failure", "pneumonia") == outcome)) {
        stop("invalid outcome")
    }
    Final_Data <- data.frame(HOSPITAL = rep(NA, length(unique(data$State))), STATE = rep(NA, length(unique(data$State))))
    rownames(Final_Data) <- sort(unique(data$State))
    for (state in sort(unique(data$State))) {
        data_state <- data[which(data$State == state),]
        if(outcome == "heart attack"){
            data_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(data_state[,11])
            Mortality_Heart_Attack <- data_state[complete.cases(data_state[, c(2,11)]),c(2,11)]
            Mortality_Heart_Attack <- Mortality_Heart_Attack[order(Mortality_Heart_Attack[,2], Mortality_Heart_Attack[,1]),]
            if(num == "best"){
                rank <- 1
            }
            if (num == "worst"){
                rank <- dim(Mortality_Heart_Attack)[1]
            }
            Final <- Mortality_Heart_Attack[rank,1]
        }
        if(outcome == "heart failure"){
            data_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(data_state[,17])
            Mortality_Heart_Failure <- data_state[complete.cases(data_state[, c(2,17)]), c(2,17)]
            Mortality_Heart_Failure <- Mortality_Heart_Failure[order(Mortality_Heart_Failure[,2], Mortality_Heart_Failure[,1]),]
            if(num == "best"){
                rank <- 1
            }
            if (num == "worst"){
                rank <- dim(Mortality_Heart_Failure)[1]
            }
            Final <- Mortality_Heart_Failure[rank,1]
        }
        if(outcome == "pneumonia"){
            data_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(data_state[,23])
            Mortality_Pneumonia <- data_state[complete.cases(data_state[, c(2,23)]), c(2,23)]
            Mortality_Pneumonia <- Mortality_Pneumonia[order(Mortality_Pneumonia[,2], Mortality_Pneumonia[,1]),]
            if(num == "best"){
                rank <- 1
            }
            if(num == "worst"){
                rank <- dim(Mortality_Pneumonia)[1]
            }
            Final <- Mortality_Pneumonia[rank,1]
        }
        Final_Data[state, "STATE"] <- state
        Final_Data[state, "HOSPITAL"] <- Final
    }
    return(Final_Data)
}


# Test Function
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)