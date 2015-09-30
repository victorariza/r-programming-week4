 best <- function(state, outcome) {
     
     ## Read outcome data
     outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
          
     ## Check that state and outcome are valid
     
     colIndexes <- list();
     colIndexes[["heart attack"]] <- 13;
     colIndexes[["heart failure"]] <- 18;
     colIndexes[["pneumonia"]] <- 24;
     
     outcomeIndex <- -1;
     ## Return an error in the outcome is not one of the previous ones
     error = tryCatch({
         outcomeIndex = get(outcome, colIndexes);
     }, error = function(e){
         return(TRUE);
     });
     
     if (outcomeIndex < 0){
         return(paste0("Error in best(", state, ", ", outcome, ") : invalid outcome"));
     }
     
     ## Return an error if the state is not in the file
     states = unique(outcomeData[7])
     
     if (!state %in% states$State){
         return(paste0("Error in best(", state, ", ", outcome, ") : invalid state"));
     }
     
     stateOutcomeData = outcomeData[outcomeData$State==state, ];
     
     stateOutcomeData[,outcomeIndex] <- as.numeric(stateOutcomeData[,outcomeIndex]);
     
     stateOutcomeData <- stateOutcomeData[!is.na(stateOutcomeData[,outcomeIndex]),];
     
     ## Return hospital name in that state with lowest 30-day death
     ## rate
     min <- min(stateOutcomeData[,outcomeIndex]);
     minStates <- stateOutcomeData[stateOutcomeData[,outcomeIndex] == min,]$Hospital.Name;
     minState = sort(minStates)[1];
     
     return(minState);
     
 }
 
