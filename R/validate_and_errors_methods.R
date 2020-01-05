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
