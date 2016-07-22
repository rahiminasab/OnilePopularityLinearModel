library(RecordLinkage)

camelCaseSplit <- function(x) {
  return(gsub("([a-z])([A-Z])", "\\1 \\L\\2", x, perl = TRUE))
}

punc_remove <- function(x){
  return(gsub("[[:punct:]///' ]", " ", x))
}

clearString <- function(x){
  return(tolower(punc_remove(camelCaseSplit(x))))
}

colNames <- list(c("Best.RT","best retention time", "retention time","rt","best ret time"),
                 c("FWHM","max fwhm"))


guessColumnName <- function(x){
  x <- clearString(x)
  max_index <- 0
  max <- -1
  for(i in 1:length(colNames)){
    col <- colNames[[i]]
    for(j in 1:length(col)){
      sim <- levenshteinSim(x,col[j])
      if(sim > max){
        max <- sim
        max_index <- i
      }
    }
  }
  return(colNames[[max_index]][1])
}

colnames(data) <- unlist(lapply(X = colnames(data), FUN = function(x) guessColumnName(x)))

data[data==""] <- NA