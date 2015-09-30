rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
    
    ## Check that the outcome is valid
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
    
    
    ## For each state, find the hospital of the given rank
    states = unique(outcomeData[7]);
    
    hospitals = c();
    
    for (i in 1: nrow(states)){
        
        state <- states[i,1];
        
        # Filter the data by state and outcome
        stateOutcomeData = outcomeData[outcomeData$State==state, ];
        stateOutcomeData[,outcomeIndex] <- as.numeric(stateOutcomeData[,outcomeIndex]);
        stateOutcomeData <- stateOutcomeData[!is.na(stateOutcomeData[,outcomeIndex]),];
        
        numIteration <- num;
        if (numIteration == "best"){
            numIteration <- 1;
        } else if (numIteration == "worst"){
            numIteration <- nrow(stateOutcomeData);
        }
        
        stateOutcomeData = stateOutcomeData[order(stateOutcomeData[outcomeIndex], stateOutcomeData$Hospital.Name),];
        
        hospitals <- c(hospitals, stateOutcomeData$Hospital.Name[numIteration]);
        
    }
    
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    ret = data.frame(hospitals, as.list(states));
    colnames(ret)<-c("hospital", "state");
    return(ret);

}
