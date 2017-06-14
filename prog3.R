setwd("D:\\coursera")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

best <- function(state, outcomes)
{
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (outcomes == "heart attack")
  {
    no.col = 11
  }else if (outcomes == "heart failure")
  {
    no.col = 17
  }else if (outcomes == "pneumonia")
  {
    no.col = 23
  }else
  {
    cat("Error in best(\"",state,"\", ","\"",outcomes,"\") : invalid outcome", sep = '')
    no.col = NA
  }
  if (!is.na(no.col))
  {
    data <- outcome[outcome$State == state, c(2, no.col)]
    if (nrow(data) != 0)
    {
      data[data[,2]=="Not Available",2] = NA
      data <- na.omit(data)
      data[,2] <- as.numeric(data[,2])
      data1 <- data[data[,2] == min(data[,2]), 1]
      return(data1)
    }else
    {
      cat("Error in best(\"",state,"\", ","\"",outcomes,"\") : invalid state", sep = '')
    }
  }
}

best("TX", "heart attack")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

rankhospital <- function(state, outcomes, num = "best") 
{
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (outcomes == "heart attack")
  {
    no.col = 11
  }else if (outcomes == "heart failure")
  {
    no.col = 17
  }else if (outcomes == "pneumonia")
  {
    no.col = 23
  }else
  {
    cat("Error in rankhospital(\"",state,"\", \"",outcomes,"\", \"num = ",num,"\") : invalid outcome", sep = '')
    no.col = NA
  }
  if (!is.na(no.col))
  {
    data <- outcome[outcome$State == state, c(2, no.col)]
    if (nrow(data) != 0)
    {
      data[data[,2]=="Not Available",2] = NA
      data <- na.omit(data)
      data[,2] <- as.numeric(data[,2])
      data1 <- data[order(data[,2]),]
      colnames(data1) <- c('Hospital.Name','Rate')
      data1['Rank'] <- c(1:nrow(data1))
      if (num == 'best')
      {return(data1[1,])}
      else if (num == 'worst')
      {return(tail(data1,1))}
      else if (is.numeric(num))
      {return(data1[num,])}
      else
      {cat("Error in rankhospital(\"",state,"\", \"",outcomes,"\", \"num = ",num,"\") : invalid num", sep = '')}
    }else
    {
      cat("Error in rankhospital(\"",state,"\", \"",outcomes,"\", \"num = ",num,"\") : invalid state", sep = '')
    }
  }
}

rankhospital("TX", "heart failure", 4)
rankhospital("MN", "heart attack", 5000)
rankhospital("MN", "heart attack", "worst")
rankhospital("BB", "heart attack", 5000)
rankhospital("MN", "hert attack", 5000)
rankhospital("MN", "heart attack", 'ds')

rankall <- function(outcomes, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (outcomes == "heart attack")
  {
    no.col = 11
  }else if (outcomes == "heart failure")
  {
    no.col = 17
  }else if (outcomes == "pneumonia")
  {
    no.col = 23
  }else
  {
    cat("Error in rankhospital(\"",state,"\", \"",outcomes,"\", \"num = ",num,"\") : invalid outcome", sep = '')
    no.col = NA
  }
  if (!is.na(no.col))
  {
    data <- outcome[, c(2, 7, no.col)]
    data[data[,3]=="Not Available",3] = NA
    data <- na.omit(data)
    data[,3] <- as.numeric(data[,3])

    colnames(data) <- c('hospital','state','rate')
    if (num == 'best')
    {
      data1 <- data[order(data[,3]),]
      return(data1[,1:2])
    }
    else if (num == 'worst')
    {
      data1 <- data[order(data[,3], decreasing = TRUE),]
      return(data1[,1:2])
    }
    else if (is.numeric(num))
    {
      levelss <- levels(as.factor(data[order(data[,3]),3]))
      levelss <- as.numeric(levelss[num])
      levelss[20]
      return(data[data[,3] == levelss,1:2])
    }
    else
    {cat("Error in rankhospital(\"",state,"\", \"",outcomes,"\", \"num = ",num,"\") : invalid num", sep = '')}
  }
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
