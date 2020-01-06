ParseParams <- function (query, params) {
  for(i in 1:length(params)) {
    query <- gsub(paste("@param", i, sep = ""), params[i], query)
  }
  return (query)
}

GenerateInsertHeader <- function(data, db.table) {
  colNames <- names(data)
  query <- paste("INSERT INTO", db.table, " (")
  for (colNames in colNames) {
    query <- paste(query, colNames, ",", sep = "")
  }
  query <- substr(query, 1, nchar(query) - 1)
  query <- paste(query, ") VALUE ", sep = "")
  return(query)
}
