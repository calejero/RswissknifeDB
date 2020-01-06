DeleteDB  <- function(table, db.name, is.pool = FALSE) {

  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
  }

  query <- paste("DELETE FROM", table)
  use.database <- dbSendStatement(con, paste("USE", db.name))
  dbClearResult(use.database)
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') {
    ManagementDBOperationsErrors(type = 'Delete', connection = GetDBConnection(db.name, TRUE))
    if (isTRUE(is.pool)) pool::poolReturn(con)
  } else {
    cat("Delete operation into table", table, "... OK \n")
    dbClearResult(res)
    if (isTRUE(is.pool)) pool::poolReturn(con)
  }
}

DeleteByParamsDB <- function(table, colnames, params, db.name, is.pool = FALSE ) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
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

    use.database <- dbSendStatement(con, paste("USE", db.name))
    dbClearResult(use.database)
    res <- try(dbSendStatement(con, query))
    
    if (class(res) == 'try-error') {
      ManagementDBOperationsErrors(type = 'DeletebyParams', connection = GetDBConnection(db.name, TRUE))
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

TruncateTableDB <- function(table, db.name, is.pool = FALSE) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
  }

  query <- paste("TRUNCATE TABLE", table)
  use.database <- dbSendStatement(con, paste("USE", db.name))
  dbClearResult(use.database)
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') ManagementDBOperationsErrors(type = 'Truncate', 
                                                              connection = GetDBConnection(db.name, TRUE))
  else {
    cat("El contenido de la tabla",table, "se ha eliminado con exito \n")
  }
  dbClearResult(res)
  if (isTRUE(is.pool)) pool::poolReturn(con)
}

DeleteByQueryDB <- function(query, db.name, is.pool = FALSE) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
  }

  use.database <- dbSendStatement(con, paste("USE", db.name))
  dbClearResult(use.database)
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') {
    ManagementDBOperationsErrors(type = 'Delete', connection = GetDBConnection(db.name, TRUE))
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

InsertByQueryDB <- function(query, db.name, is.pool = FALSE) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
  }

  use.database <- dbSendStatement(con, paste("USE", db.name))
  dbClearResult(use.database)
  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') ManagementDBOperationsErrors(type = 'insert', 
                                                              connection = GetDBConnection(db.name, TRUE))
  else {
    cat("Congratulations, insert procedure without errors\n")
  }
  
  dbClearResult(res)
  if (isTRUE(is.pool)) pool::poolReturn(con)
}

InsertDB <- function(data, db.table, db.name) {
  query <- GenerateInsertHeader(data, db.table)
  type.validation <- ValidateInsertColumns(data, db.table, db.name)
  if (type.validation == TRUE) {
    for (i in 1:nrow(data)) {
      query = paste(query, " (", sep = "")
      for (j in 1:ncol(data)) {
        query <- paste(query, "'", data[i,j], "'", sep = "")
        if (j < ncol(data)) query <- paste(query, ", ", sep = "")
      }
      query <- paste(query, ")", sep = "")
      if (i < nrow(data) & i %% 500 != 0) query <- paste(query, ",", sep = "")
      if (i %% 500 == 0) {
        InsertByQueryDB(query, db.name)
        query <- GenerateInsertHeader(data, db.table)
      }
      else if (i == nrow(data)) {
        InsertByQueryDB(query, db.name)
      }
    }
  }
  
  else if (type.validation == FALSE) {
    cat("Houston, we have a problem:\n Type Validation Procedure with errors: \n")
  }                                       
}

CreateTempTableDB <- function(tablename, db.name, fields.df, is.pool = FALSE) {
   if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
  }

  use.database <- dbSendStatement(con, paste("USE", db.name))
  dbClearResult(use.database)
  
  res <- try(dbCreateTable(con,tablename,fields.df,row.names = NULL, temporary = FALSE))
  if (class(res) == 'try-error') ManagementDBOperationsErrors(type = 'Create', 
                                                              connection = GetDBConnection(db.name, TRUE))
  else {
    cat("Table", tablename, "created without errors", db.name,"\n")
  }
  if (isTRUE(is.pool)) pool::poolReturn(con)
}

CreateTableByQueryDB <- function(tablename, db.name, query, is.pool = FALSE) {
   if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, FALSE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, FALSE), db.conn.env)
  }

  use.database <- dbSendStatement(con, paste("USE", db.name))
  dbClearResult(use.database)

  res <- try(dbSendStatement(con, query))
  if (class(res) == 'try-error') ManagementDBOperationsErrors(type = 'create table', 
                                                              connection = GetDBConnection(db.name, TRUE))
  
  else {
    cat("Table", tablename, "created without errors", db.name,"\n")
  }
  dbClearResult(res)
  if (isTRUE(is.pool)) pool::poolReturn(con)
}  

InsertByLoadDataDB <- function(data.df, db.table, db.name, cushion = 100000, is.pool = FALSE, ...) {
  if (isTRUE(is.pool)) {
    pool <- get(GetDBConnection(db.name, TRUE), db.pool.conn.env)
    con <- pool::poolCheckout(pool)
  } else {
    con <- get(GetDBConnection(db.name, TRUE), db.conn.env)
  }
  
  N <-  nrow(data.df)
  index.inf <- 1
  index.sup <- N
  if (N > cushion) n <- ceiling(N / cushion)  else n <- 1
  
  autocommit.start <- dbSendStatement(con, "SET AUTOCOMMIT = 0;")
  dbClearResult(autocommit.start)
  
  for (i in 1:n) {
    # WRITE THE DATA TO A LOCAL FILE
    TempCSV <- paste0(tempfile(fileext = '.csv', tmpdir = tempdir()))
    index.sup <- ifelse(i != n, cushion * i, N)
    write.table(data.df[index.inf:index.sup, ], TempCSV, row.names = FALSE, fileEncoding = 'UTF-8', sep = ';')
    index.inf <- index.sup + 1
    # SUBMIT THE UPDATE QUERY AND DISCONNECT
    query  <-  sprintf(paste0(
      "LOAD DATA LOCAL INFILE '%s' REPLACE INTO TABLE %s CHARACTER SET UTF8 FIELDS ",
      "TERMINATED BY ';' ENCLOSED BY '", '"',"' ESCAPED BY '' LINES TERMINATED ",
      "BY '\n' IGNORE 1 LINES"),
      TempCSV, db.table
    )
    transaction.commit <- dbSendStatement(con, "START TRANSACTION;")
    dbClearResult(transaction.commit)
    transaction.query <- dbSendStatement(con, query)
    dbClearResult(transaction.query)
    transaction.commit <- dbSendStatement(con, "COMMIT;")
    dbClearResult(transaction.commit)
    # REMOVE THE TEMP DATA FROM LOCAL DIRECTORY
    file.remove(TempCSV)
  }
  autocommit.end <- dbSendStatement(con, "SET AUTOCOMMIT = 1;")
  dbClearResult(autocommit.end)
  dbDisconnect(con)
}
