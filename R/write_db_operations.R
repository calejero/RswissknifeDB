DeleteDB  <- function(table, database, is.pool = FALSE) {

  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(database, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(database, FALSE), db.conn.env)
  }

  query <- paste("DELETE FROM", table)
  use.database <- dbSendStatement(con, paste("USE", database))
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') {
    ManagementDBOperationsErrors(type = 'Delete', connection = GetDBConnection(database, TRUE))
    if (isTRUE(is.pool)) pool::poolReturn(con)
  } else {
    cat("Delete operation into table", table, "... OK \n")
    dbClearResult(res)
    if (isTRUE(is.pool)) pool::poolReturn(con)
  }
}

DeleteByParamsDB <- function(table, colnames, params, database, is.pool = FALSE ) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(database, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(database, FALSE), db.conn.env)
  }

  query <-  paste("DELETE FROM", table,"WHERE ")

  for (i in 1:length(colnames)) {
    colname <- colnames[i]
    value <- params[i]
    query <- paste(query, colname, "='",as.character(value), "' ", sep = "")
    if (i < length(colnames)) {
      query <- paste(query," AND ", sep = "")
    }
  }

  use.database <- dbSendStatement(con, paste("USE", database))
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') {
    ManagementDBOperationsErrors(type = 'DeletebyParams', connection = GetDBConnection(database, TRUE))
    if (isTRUE(is.pool)) pool::poolReturn(con)
  } else {
    cat("Delete operation by params... OK \n")  
    rows.afected.c <- dbGetRowsAffected(res)
    dbClearResult(res)
    if (isTRUE(is.pool)) pool::poolReturn(con)
    cat("Number of rows affected:\t")
    return(rows.afected.c)
  }
}

TruncateTableDB <- function(table, database, is.pool = FALSE) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(database, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(database, FALSE), db.conn.env)
  }

  query <- paste("TRUNCATE TABLE", table)
  use.database <- dbSendStatement(con, paste("USE", database))
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') ManagementDBOperationsErrors(type = 'Truncate', 
                                                              connection = GetDBConnection(database, TRUE))
  else {
    cat("El contenido de la tabla",table, "se ha eliminado con exito \n")
  }
  dbClearResult(res)
  if (isTRUE(is.pool)) pool::poolReturn(con)
}

DeleteByQueryDB <- function(query, database, is.pool = FALSE) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(database, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(database, FALSE), db.conn.env)
  }

  use.database <- dbSendStatement(con, paste("USE", database))
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') {
    ManagementDBOperationsErrors(type = 'Delete', connection = GetDBConnection(database, TRUE))
    if (isTRUE(is.pool)) pool::poolReturn(con)
  }
  else {
    cat("Delete by query... OK \n")
    rows.afected.c <- dbGetRowsAffected(res)
    dbClearResult(res)
    if (isTRUE(is.pool)) pool::poolReturn(con)
    cat("Number of rows affected:\t")
    return(rows.afected.c)
  }
}
