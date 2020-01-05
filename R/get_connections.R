#' @name GetMySQLConnection
#' @title Get MySQL connection
#'
#' @description This function establishes a mysql connection.
#'
#' @param db.host string identifying the host machine running the MariaDB server.
#' @param db.user Username
#' @param db.pass Password .
#' @param db.port Integer of the tcp/ip port to establish a connection.
#' @param db.compression Default: 32.
#' @param db.pool Pool flag. By Default: False. If it is true, function generates a pool connection to mysql server.
#'
#' @return None
#'
#' @export
GetMySQLConnection <- function(db.host, db.user, db.pass, db.port, db.default,
                               db.compression = 32, db.pool = FALSE) {

  if (isTRUE(db.pool)) {
    cat("database", db.default, "DB HOST", db.host, "\n")
    db.connection <- pool::dbPool(drv = RMariaDB::MariaDB(),
                                  dbname = as.character(db.default),
                                  host = as.character(db.host),
                                  username = as.character(db.user),
                                  password = as.character(db.pass),
                                  port = as.character(db.port),
                                  minSize = 1,
                                  maxSize = 5,
                                  idleTimeout = 120)
    dbGetInfo(db.connection)
  } else {
    db.connection <- dbConnect(RMariaDB::MariaDB(), username = as.character(db.user),
                               password = as.character(db.pass), dbname = as.character(db.default),
                               host = as.character(db.host), port = as.character(db.port),
                               client.flag = db.compression)
  }

  return(db.connection)
}

GetEnvironmentConnectionsAvailable <- function() {
  list.active.connections.c <- ls(db.conn.env)
  if(length(list.active.connections.c) == 0) {
    cat("No connections established \n")
  }
  else {
    cat("Active conections availables:\n")
    for (i in 1:length(list.active.connections.c)) {
      cat(list.active.connections.c[i], "\n")
    }
  }
}

#' @name ClosePullConnectionDB
#' @title Close Pull Connections
#'
#' @description This function closes a pool of mysql connections.
#'
#' @return None
#'
#' @export
ClosePullConnectionDB <- function() {
  list.active.connections <- ls(db.pool.conn.env)
  cat("Active Pool Connections: ", length(list.active.connections), "\n")
  if (length(list.active.connections) > 0) {
    for (i in 1:length(list.active.connections)) {
      active.con <- get(list.active.connections[i], db.pool.conn.env)
      pool::poolClose(active.con)
      remove(list = (list.active.connections[i]), envir = db.pool.conn.env)
    }
  }
}

#' @name CloseConnectionDB
#' @title Close MySQL Connections
#'
#' @description This function closes all mysql connections.
#'
#' @return None
#'
#' @export
CloseConnectionDB <- function() {
  list.active.connections <- ls(db.conn.env)
  cat("Active Connections: ", length(list.active.connections), "\n")
  if (length(list.active.connections) > 0) {
    for (i in 1:length(list.active.connections)) {
      active.con <- get(list.active.connections[i], db.conn.env)
      RMariaDB::dbDisconnect(active.con)
      remove(list = (list.active.connections[i]), envir = db.conn.env)
    }
  }
}

#' @name GetDBConnection
#' @title Get active connection
#'
#' @description This function obtains a connection from environment depends on database affected.
#'
#' @param db.name string identifying the database selected.
#' @param flag.write.operation Boolean value. If it is true, method search a connection thats admits write operations
#'
#' @return None
#'
#' @export
GetDBConnection <- function(db.name, flag.write.operation) {
  available.machines.df <- tryCatch(
                                    get("schemas.db", db.env), 
                                        error = function(e) return ("No DB connections availables for this database.")
                                    )
  
  machine.df <- available.machines.df[available.machines.df$Database == db.name,]
  db.servers.list <- config_db[config_db$DB_HOST %in% machine.df$server & 
                                config_db$platform %in% machine.df$platform, ]
  
  if (nrow(db.servers.list) > 1) {
    type.c <- ifelse(flag.write.operation == TRUE, "master", "slave")
    db.servers.list <- config_db[config_db$TYPE == type.c, ]
  } 
  
  con.name <- paste0("con_", as.character(db.servers.list$DB_HOST), "_",
                       as.character(db.servers.list$TYPE), collapse = '')
  return(con.name)
}
