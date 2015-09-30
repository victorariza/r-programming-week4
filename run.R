outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcomeData)
ncol(outcomeData)
nrow(outcomeData)
names(outcomeData)
outcome[,11] <- as.numeric(outcomeData[,11])
hist(outcomeData[,11])
outcomeData[,11]



state <- "AL"
outcome <-"heart attack"

stateOutcomeData = outcomeData[outcomeData$State=="AL", ]

head(stateOutcomeData)
ncol(stateOutcomeData)
nrow(stateOutcomeData)

source("functions.R")
index = best("AL", "heart attack")
index
