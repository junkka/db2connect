#' Connect to DB2
#'
#' Set up connection to DB2 server
#'
#' @param db_name name of database
#' @param username name of user
#' @param password user password
#' @param server_address address to server
#' @param driver_path Path to JDBC DB2 driver
#'
#' @import RJDBC
#' @export


db2_connect <- function(
  db_name = NULL,
  username = NULL,
  password = NULL,
  server_address = NULL,
  driver_path = NULL
){

  
  db_name <- db_name %||% 
    getOption('db2.db_name') %||% 
    stop("db_name is NULL. Requires a db_name")
  
  
  username <- username %||% 
    getOption('db2.username') %||% 
    stop("username is NULL. Requires a username")

  server_address = server_address %||%
    getOption("db2.server_address") %||% 
    stop("server_address is NULL. Requires a server address")

  password <- password %||%
    getOption('db2.password') %||%
    rstudioapi::askForPassword("Please enter your password")
  
  if (inherits(password, "call")) {
    password <- eval(password)
  }
  
  driver_path <- driver_path %||%
    getOption("db2.driver_path") %||%
    stop("driver_path required. No path to DB2 java driver specified.")

  jcc = JDBC("com.ibm.db2.jcc.DB2Driver", driver_path)


  conn = dbConnect(jcc,
                   file.path("jdbc:db2:/", server_address, db_name),
                   user=username,
                   password=password)

  conn@identifier.quote <- paste(db_name, server_address)
  hostn <- file.path("jdbc:db2:/", server_address, db_name)

  code <- 'conn <- db2connect::db2_connect(db_name = "' %+% db_name %+% '", 
    username = "' %+% username %+%'",
    server_address = "' %+% server_address %+% '",
    driver_path = "' %+% driver_path %+% '"
  )'

  on_conn_open(conn, code)

  conn

}


on_conn_open <- function(conn, code){

  observer <- getOption("connectionObserver")
  if (!is.null(observer))
    observer$connectionOpened(
      type = "DB2JDBC",
      displayName = paste("DB2-JDBC", conn@identifier.quote),
      host = conn@identifier.quote,
      icon = system.file("img", "db2-ui.png", package="db2connect"),
      disconnect = function(){
        db2_close(conn)
      },
      connectCode = code,
      listObjects = function(...) db2_list_objects(conn, ...),
      listObjectTypes = function(){
        list(
          schema = list(
            contains = list(
              table = list(
                contains = "data"
              )
            )
          )
        )
      },
      listColumns = function(...) db2_list_columns(conn, ...),
      previewObject = function(...) db2_preview_object(conn, ...),
      connectionObject = conn


    )
}

db2_preview_object <- function(conn, schema, table, limit = 10){
  db2_query(conn, paste0("SELECT * FROM ", schema, ".", table, " LIMIT ", limit))
}

#' List columns in DB2 table
#'
#' Lists columns and colums types in a DB2 table
#'
#' @param conn database connection object
#' @param schema name of schema
#' @param table name of table
#'
#' @export

db2_list_columns <- function(conn, schema, table){
  columns <- db2_query(
    conn, 
    paste0(
      "Select distinct(name), ColType from Sysibm.syscolumns where tbname = '", 
      table,
      "' and tbcreator = '", 
      schema,"'"
    )
  )
  names(columns) <- c("name", "type")
  columns
}

#' Close db2 connection
#'
#' Close connection to db2 server
#'
#' @param conn database connection object
#'
#' @export

db2_close <- function(conn){
  closed <- dbDisconnect(conn)

  if (closed | !is.na(conn@identifier.quote)){
    close_pane(conn@identifier.quote)
  }
}

close_pane <- function(host){

  observer <- getOption("connectionObserver")
  if (!is.null(observer))
    observer$connectionClosed("DB2JDBC", host)
}

#' List objects in DB2 database
#'
#' Returns list of objects (schemas or tables) in a DB2 database.
#'
#' @param conn database connection object
#' @param schema name of schema, specified to list tables in a schema. 
#'   Leave as NULL to show list of shemas in database.
#'
#' @export

db2_list_objects <- function(conn, schema = NULL){

  if (!is.null(schema)){
    tables <- dbGetQuery(conn, paste0("select tabname from syscat.tables where tabschema='", schema, "'"))
    names(tables) <- "name"
    tables$type <- "table"
    return(tables)

  }
  schemas <- dbGetQuery(conn, "select schemaname from syscat.schemata")
  schemas$type = "schema"
  names(schemas) <- c("name", "type")
  return(schemas)


}
#' Get Query
#'
#' Get data from DB2 connection using SQL query
#'
#' @param conn db2 connection
#' @param qu character string with SQL query
#'
#' @importFrom DBI dbSendQuery dbFetch
#' @importFrom tibble as_tibble
#' @export

db2_query <- function(conn, qu){
  rs = dbSendQuery(conn, qu)
  df = dbFetch(rs)
  as_tibble(df)
}

`%||%` <- function (x, y) {
  if (is.null(x)) {
    y
  }
  else {
    x
  }
}


`%+%` <- function(x, y){
  paste0(x, y)
}

