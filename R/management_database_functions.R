db.env <- new.env()
db.conn.env <- new.env()
db.pool.conn.env <- new.env()

LoadDBConfigInformation <- function(platform.c, slave.flag) {
  db.servers.list <- config_db[config_db$platform == platform.c, ]
  if (slave.flag == TRUE) db.servers.list <- db.servers.list[db.servers.list$TYPE == "slave", ]
  if (nrow(db.servers.list) == 0) cat("Any db configuration for this platform:", platform.c, "\n")
  return(db.servers.list)
}

#' @name BuildDBEnvirontment
#' @title Build DB Environment.
#'
#' @description Method to generate all basic information to connect into server.
#'
#' @param platform.c Database type.
#' @param slave.flag TRUE/FALSE flag to determinate if it is a queries slave server.
#' @param pool.flag TRUE/FALSE flag to determinate if it is a pool connection.
#'
#' @return None
#'
#' @export
BuildDBEnvirontment <- function(platform.c, slave.flag = FALSE, pool.flag = FALSE) {
  db.servers.list <- LoadDBConfigInformation(platform.c, slave.flag)
  if (nrow(db.servers.list) == 0) return(NULL)

  GenerateSchemaList(db.servers.list, pool.flag)
  GetEnvironmentConnectionsAvailable()
}

#' @name DBSwitchTechnology
#' @title Get a database connection
#'
#' @description Get a connection depending on driver technology.
#'
#' @param server.config Server configuration.
#' @param pool.flag TRUE/FALSE flag to determine if it is a pool connection.
#'
#' @return None
#'
#' @export
DBSwitchTechnology <- function(server.config, pool.flag) {
  connection.method <- try(switch(as.character(server.config$platform),
                                  mysql = GetMySQLConnection(db.host = server.config$DB_HOST,
                                                             db.user = server.config$USER,
                                                             db.pass = server.config$PASS,
                                                             db.port = server.config$PORT,
                                                             db.default = server.config$DEFAULT,
                                                             db.pool = pool.flag)))

  if ('try-error' %in% class(connection.method)){
    cat("Houston, we have a problem:", as.character(server.config$DB_HOST), " Connection is not available.\n")
    return (NULL)
  } else{
    cat("Hello", as.character(server.config$DB_HOST), "-", as.character(server.config$platform), "\n")
    con.name <- paste0("con_", as.character(server.config$DB_HOST), "_",
                       as.character(server.config$TYPE), collapse = '')

    if (pool.flag == TRUE) {
      assign(con.name, connection.method, db.pool.conn.env)
    } else {
      assign(con.name, connection.method, db.conn.env)
    }
    return(connection.method)
  }
}

#' @name GetSchemaInfo
#' @title Get Schema info
#'
#' @description This function generates a list of availables schemas from an estabished connection.
#'
#' @param connection.method a MariaDBConnection object, produced by DBI::dbConnect().
#' @param pool.flag TRUE/FALSE flag to determine if it is a pool connection.
#'
#' @return None
#'
#' @export
GetSchemaInfo <- function(connection.method, pool.flag) {

  if (isTRUE(pool.flag)) {
    con <- pool::poolCheckout(connection.method)
    db.server.schemas.con <- dbSendQuery(con, "SHOW DATABASES")
    db.server.schemas <- dbFetch(db.server.schemas.con)
    dbClearResult(db.server.schemas.con)
    pool::poolReturn(con)
    pool::poolClose(connection.method)
  } else {
    db.server.schemas.con <- dbSendQuery(connection.method, "SHOW DATABASES")
    db.server.schemas <- dbFetch(db.server.schemas.con)
    dbClearResult(db.server.schemas.con)
  }

  return(db.server.schemas)
}

#' @name GenerateSchemaList
#' @title Build schema info into environtment variables
#'
#' @description Assign availables Schemas into environment variables.
#'
#' @param db.servers.list List of all availables servers.
#' @param pool.flag TRUE/FALSE flag to determine if it is a pool connection.
#'
#' @return None
#'
#' @export
GenerateSchemaList <- function(db.servers.list, pool.flag) {

  list.server.schemas.df <- NULL

  for (i in 1:nrow(db.servers.list)) {

    if (isTRUE(pool.flag) & db.servers.list$AVAILABLEPOOL[i] == TRUE) {
      server.pool.flag <- TRUE
      connection.result <- try(DBSwitchTechnology(db.servers.list[i, ], server.pool.flag))
      cat("Now,", as.character(db.servers.list$DB_HOST[i]), "the pool environment is available\n")

    } else if (isTRUE(pool.flag) & db.servers.list$AVAILABLEPOOL[i] == FALSE) {
      cat("Please, you are selectect a pool connection but server don't allow it.\n",
          "Info:", as.character(db.servers.list$DB_HOST[i]))
    } else {
      server.pool.flag = FALSE
      connection.result <- try(DBSwitchTechnology(db.servers.list[i, ], server.pool.flag))
      cat("Now,", as.character(db.servers.list$DB_HOST[i]), "the connection is available\n")
    }

    if (!is.null(connection.result)) {
      part.server.schemas.df <- GetSchemaInfo(connection.result, server.pool.flag)
      part.server.schemas.df$server <- as.character(db.servers.list$DB_HOST[i])
      part.server.schemas.df$platform <- as.character(db.servers.list$platform[i])
      part.server.schemas.df$type <- as.character(db.servers.list$TYPE[i])
      if (pool.flag == TRUE) {
        list.server.schemas.df <- rbind(list.server.schemas.df, part.server.schemas.df)
      } else {
        list.server.schemas.df <- rbind(list.server.schemas.df, part.server.schemas.df)
      }
    }
  }

  assign("schemas.db", list.server.schemas.df, db.env)
}
