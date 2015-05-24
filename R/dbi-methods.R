#' @include dbi-classes.R
NULL

# Drivers ----------------------------------------------------------------

#' @param drv a \code{\linkS4class{SQLServerDriver}} object
#' @param ... other parameters which are not passed on further but necessary to
#' match generic signature
#' @rdname SQLServerDriver-class
#' @export

setMethod(f = 'dbListConnections', signature = 'SQLServerDriver',
  definition = function (drv, ...) {
    warning ("JDBC driver maintains no list of active connections.")
    list()
  }
)

#' @param  dbObj a \code{\linkS4class{SQLServerDriver}} object
#' @rdname SQLServerDriver-class
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerDriver',
  definition = function (dbObj, ...) {
    list(name = 'RSQLServer (jTDS)',
      driver.version = rJava::.jcall(dbObj@jdrv, "S", "getVersion"))
  }
)

#' @param  object a \code{\linkS4class{SQLServerDriver}} object
#' @rdname SQLServerDriver-class
#' @export

setMethod(f = "show", signature = "SQLServerDriver",
  definition = function (object) {
    cat("<SQLServerDriver>\n")
  }
)

#' @param drv a \code{\linkS4class{SQLServerDriver}} object
#' @rdname SQLServerDriver-class
#' @export

setMethod("dbUnloadDriver", "SQLServerDriver", function(drv, ...) FALSE)


# Connections ------------------------------------------------------------

#' Connect to/disconnect from a SQL Server database.
#'
#' @param drv An objected of class \code{\linkS4class{SQLServerDriver}}, or an
#' existing \code{\linkS4class{SQLServerConnection}}. If a connection,
#' the connection will be cloned.
#' @template sqlserver-parameters
#' @return a \code{\linkS4class{SQLServerConnection}}
#' @examples
#' # View sql.yaml file bundled in package
#' file <- system.file("extdata", "sql.yaml", package = "RSQLServer")
#' readLines(file)
#' # Connect using ~/sql.yaml file
#' if (have_test_server()) {
#'  dbConnect(SQLServer(), "TEST")
#' }
#' # Don't use file argument:
#' \dontrun{
#' dbConnect(SQLServer(), server="11.1.111.11", port=1434,
#'    properties=list(useNTLMv2="true", domain="myco", user="me",
#'      password="asecret"))
#' }
#' @rdname SQLServer
#' @export

setMethod(f = 'dbConnect', signature = "SQLServerDriver",
  definition = function (drv, server, file = NULL, database = "",
    type = "sqlserver", port = "", properties = list()) {
    # Use sql.yaml file if file is not missing. Note this will then ignore
    # the paramaters type, port and connection properties will be ignored and the
    # information in sql.yaml will be used instead
    sd <- get_server_details(server, file)
    if (!identical(sd, list())) {
      # Server details must include type and port otherwise get_server_file
      # fails
      server <- sd$server
      sd$server <- NULL
      type <- sd$type
      sd$type <- NULL
      port <- sd$port
      sd$port <- NULL
      properties <- sd
    }
    url <- jtds_url(server, type, port, database, properties)
    properties <- rJava::.jnew('java/util/Properties')
    jc <- rJava::.jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", url,
      properties)
    new("SQLServerConnection", jc = jc, identifier.quote = drv@identifier.quote)
  }
)

#' @rdname SQLServerConnection-class
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerConnection',
  definition = function (dbObj, ...) {
    meta <- rJava::.jcall(dbObj@jc, "Ljava/sql/DatabaseMetaData;",
      "getMetaData")
    list(db.product.name = rJava::.jcall(meta, "S", "getDatabaseProductName"),
      db.version = rJava::.jcall(meta, "I", "getDatabaseMajorVersion"),
      user = rJava::.jcall(meta, "S","getUserName"))
  }
)

#' @param  object a \code{\linkS4class{SQLServerConnection}} object
#' @rdname SQLServerConnection-class
#' @export

setMethod(f = "show", signature = "SQLServerConnection",
  definition = function (object) {
    info <- dbGetInfo(object)
    cat("SQLServerConnection to", info$db.product.name,
      "version", info$db.version)
  }
)

#' @rdname SQLServerConnection-class
#' @export

setMethod(f = 'dbIsValid', signature = 'SQLServerConnection',
  definition = function (dbObj, ...) !rJava::.jcall(dbObj@jc, "Z", "isClosed")
)

#' Send query to SQL Server
#'
#' This is basically a copy of RJDBC's \code{\link[RJDBC:JDBCConnection-methods]{dbSendQuery}}
#' method for JDBCConnection, except that this returns a
#' \code{\linkS4class{SQLServerResult}} rather than a JDBCResult.
#'
#' @param statement SQL statement to execute
#' @param ... additional arguments to prepared statement substituted for "?"
#' @param list undocumented
#' @return a \code{\linkS4class{SQLServerResult}} object
#' @rdname SQLServerConnection-class
#' @export

setMethod("dbSendQuery",
  signature(conn = "SQLServerConnection", statement = "character"),
  def = function (conn, statement, ..., list=NULL) {
    statement <- as.character(statement)[1L]
    ## if the statement starts with {call or {?= call then we use CallableStatement
    if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement)))) {
      s <- rJava::.jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall",
        statement, check=FALSE)
      RJDBC:::.verify.JDBC.result(s,
        "Unable to execute JDBC callable statement ", statement)
      if (length(list(...)))
        RJDBC:::.fillStatementParameters(s, list(...))
      if (!is.null(list))
        RJDBC:::.fillStatementParameters(s, list)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      RJDBC:::.verify.JDBC.result(r,
        "Unable to retrieve JDBC result set for ", statement)
    } else if (length(list(...)) || length(list)) {
      ## use prepared statements if there are additional arguments
      s <- rJava::.jcall(conn@jc, "Ljava/sql/PreparedStatement;",
        "prepareStatement", statement, check=FALSE)
      RJDBC:::.verify.JDBC.result(s,
        "Unable to execute JDBC prepared statement ", statement)
      if (length(list(...)))
        RJDBC:::.fillStatementParameters(s, list(...))
      if (!is.null(list))
        RJDBC:::.fillStatementParameters(s, list)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      RJDBC:::.verify.JDBC.result(r,
        "Unable to retrieve JDBC result set for ", statement)
    } else {
      ## otherwise use a simple statement some DBs fail with the above)
      s <- rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
      RJDBC:::.verify.JDBC.result(s,
        "Unable to create simple JDBC statement ", statement)
      r <- rJava::.jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
        as.character(statement)[1], check=FALSE)
      RJDBC:::.verify.JDBC.result(r,
        "Unable to retrieve JDBC result set for ", statement)
    }
    md <- rJava::.jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData",
      check=FALSE)
    RJDBC:::.verify.JDBC.result(md,
      "Unable to retrieve JDBC result set meta data for ", statement,
      " in dbSendQuery")
    new("SQLServerResult", jr=r, md=md, stat=s, pull=rJava::.jnull())
  }
)

# Will be called by dplyr::db_begin.DBIConnection

setMethod(f = "dbBegin", signature = "SQLServerConnection",
  definition = function (conn, ...) {
    # https://technet.microsoft.com/en-us/library/aa225983(v=sql.80).aspx
    # https://msdn.microsoft.com/en-us/library/ms188929.aspx
    dbGetQuery(con, "BEGIN TRANSACTION")
  }
)

# dbDisconnect: Inherits from JDBCConnection
# dbGetQuery: Inherits from JDBCConnection
# dbGetException: Inherits from JDBCConnection
# dbListResults: Inherits from JDBCConnection
# dbListTables: Inherits from JDBCConnection
# dbReadTable: Inherits from JDBCConnection
# dbWriteTable: Inherits from JDBCConnection
# dbExistsTable: Inherits from JDBCConnection
# dbRemoveTable: Inherits from JDBCConnection
# dbListFields: Inherits from JDBCConnection
# dbCommit: Inherits from JDBCConnection
# dbRollback: Inherits from JDBCConnection
# dbCallProc: Not yet implemented

# Results ----------------------------------------------------------------

#' @param dbObj a \code{\linkS4class{SQLServerResult}} object
#' @rdname SQLServerResult-class
#' @export

setMethod (f = 'dbIsValid', signature = 'SQLServerResult',
  definition = function (dbObj) {
    rJava::.jcall(dbObj@jr, "Z", "isClosed")
  }
)

# Based on:
# http://stackoverflow.com/a/3818712/1193481

sqlServerListFields <- function (res) {
  column_count <- rJava::.jcall(res@md, "I", "getColumnCount")
  column_names <- vector("character", column_count)
  for (i in seq_along(column_names)) {
    column_names[i] <- rJava::.jcall(res@md, "S", "getColumnName", i)
  }
  column_names
}


# Per DBI documentation:
# "fetch is provided for compatibility with older DBI clients - for all new
# code you are strongly encouraged to use dbFetch."
# JDBC does not currently have a dbFetch method.

#' @export
setMethod("dbFetch", "SQLServerResult", function (res, n = -1, ...) {
  RJDBC::fetch(res, n)
})

# JDBC does not currently have a dbHasCompleted method
# Does not appear as though such a method is available in the JDBC API

# fetch: Inherits from JDBCResult
# dbClearResult: Inherits from JDBCResult
# dbGetInfo: Inherits from JDBCResult
# dbColumnInfo: Inherits from JDBCResult

