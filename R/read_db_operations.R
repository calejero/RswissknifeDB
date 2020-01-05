SelectByParamsDB <- function(query, params, database, slave = FALSE, is.pool = FALSE) {
  if (ValidateSlaveQuery(query) == FALSE) {
    ManagementDBOperationsErrors(type = 'Write operation into slave database', 
                                 connection = DatabaseConnection(database, FALSE))
  }
  
  if (isTRUE(is.pool)) {
  pool <- get(GetDBConnection(database, FALSE), db.pool.conn.env)
  con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(database, FALSE), db.conn.env)
  }
  
  query <- ParseParams(query, params)
  use.database <- dbSendQuery(con, paste("USE", database))
  query.con <- try(dbSendQuery(con, query))
  if (class(query.con) == 'try-error') {
    if (isTRUE(is.pool)) pool::poolReturn(con)
    ManagementDBOperationsErrors(type = 'SelectByParams', connection = DatabaseConnection(database, FALSE))
  } else {
    query.df <- dbFetch(query.con, n = -1)
    dbClearResult(query.con)
    if (isTRUE(is.pool)) pool::poolReturn(con)
    return(query.df)
  }
}

SelectDB <- function(query, database, slave = FALSE, is.pool = FALSE) {
  if (ValidateSlaveQuery(query) == FALSE) {
    ManagementDBOperationsErrors(type = 'Write operation into slave database', 
                                 connection = DatabaseConnection(database, FALSE))
  }
  
  if (isTRUE(is.pool)) {
  pool <- get(GetDBConnection(database, FALSE), db.pool.conn.env)
  con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(database, FALSE), db.conn.env)
  }
  
  use.database <- dbSendQuery(con, paste("USE", database))
  query.con <- try(dbSendQuery(con, query))
  if (class(query.con) == 'try-error') {
    if (isTRUE(is.pool)) pool::poolReturn(con)
    ManagementDBOperationsErrors(type = 'Select', connection = DatabaseConnection(database, FALSE))
  } else {
    query.df <- dbFetch(query.con, n = -1)
    dbClearResult(query.con)
    if (isTRUE(is.pool)) pool::poolReturn(con)
    return(query.df)
  }
}