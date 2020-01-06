ManagementDBOperationsErrors <- function(type, connection) {
  message(paste("===================================================="))
  message(paste("Wrong operation:", type))
  message(paste("Connection name:", connection))
  message(paste("===================================================="))
}

ValidateSlaveQuery <- function(query) {
  patron.c <- c("create", "truncate", "drop", "insert", "optimize", "update", "delete")
  if (TRUE %in% stringr::str_detect(tolower(query), patron.c)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

ValidateInsertColumns <- function(data.df, db.table, db.name) {
  query.df <- SelectByParamsDB("SELECT * FROM @param1", db.table, db.name)
  query.df <- query.df[, colnames(query.df) %in% colnames(data.df), ]
  result.df = data.frame(query = unlist(lapply(query.df, class)), 
                         data = unlist(lapply(data.df, class)),
                         stringsAsFactors = FALSE)
  
  result.df$flag <- ifelse(result.df$query == result.df$data, 1, 0)
  
  if (min(result.df$flag) == 0) {
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}
