rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Check that state and outcome are valid
    colIndexes <- list();
    colIndexes[["heart attack"]] <- 13;
    colIndexes[["heart failure"]] <- 18;
    colIndexes[["pneumonia"]] <- 25;
    
    outcomeIndex <- -1;
    ## Return an error in the outcome is not one of the previous ones
    error = tryCatch({
        outcomeIndex = get(outcome, colIndexes);
    }, error = function(e){
        return(TRUE);
    });
    
    if (outcomeIndex < 0){
        stop("invalid outcome");
    }
    
    ## Return an error if the state is not in the file
    states = unique(outcomeData[7])
    
    if (!state %in% states$State){
        stop("invalid state");
    }
    
    # Filter the data by state and outcome
    stateOutcomeData = outcomeData[outcomeData$State==state, ];
    stateOutcomeData[,outcomeIndex] <- as.numeric(stateOutcomeData[,outcomeIndex]);
    stateOutcomeData <- stateOutcomeData[!is.na(stateOutcomeData[,outcomeIndex]),];
    
    stateOutcomeData = stateOutcomeData[order(stateOutcomeData[outcomeIndex], stateOutcomeData$Hospital.Name),]
    
    if (num == "best"){
        num <- 1;
    } else if (num == "worst"){
        num <- nrow(stateOutcomeData);
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    return(stateOutcomeData$Hospital.Name[num])
    
}