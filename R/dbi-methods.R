#' @include dbi-classes.R
NULL

# Drivers ----------------------------------------------------------------

#' @param drv a \code{\linkS4class{SQLServerDriver}} object
#' @param ... other parameters which are not passed on further but necessary to
#' match generic signature
#' @rdname SQLServerDriver-class
#' @importMethodsFrom RJDBC dbListConnections
#' @export

setMethod(f = 'dbListConnections', signature = 'SQLServerDriver',
  definition = function (drv, ...) {
    warning ("JDBC driver maintains no list of active connections.")
    list()
  }
)

#' @param  dbObj a \code{\linkS4class{SQLServerDriver}} object
#' @rdname SQLServerDriver-class
#' @importMethodsFrom RJDBC dbGetInfo
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerDriver',
  definition = function (dbObj, ...) {
    list(name = 'RSQLServer (jTDS)',
      driver.version = .jcall(dbObj@jdrv, "S", "getVersion"))
  }
)

#' @rdname SQLServerDriver-class
#' @importMethodsFrom RJDBC dbUnloadDriver
#' @export

setMethod("dbUnloadDriver", "SQLServerDriver", function(drv, ...) TRUE)


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
#' @importMethodsFrom RJDBC dbConnect
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
    properties <- .jnew('java/util/Properties')
    jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", url, properties)
    new("SQLServerConnection", jc = jc, identifier.quote = drv@identifier.quote)
  }
)

#' @rdname SQLServerConnection-class
#' @importMethodsFrom RJDBC dbGetInfo
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerConnection',
  definition = function (dbObj, ...) {
    meta <- .jcall(dbObj@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData")
    list(db.product.name = .jcall(meta, "S", "getDatabaseProductName"),
      db.version = .jcall(meta, "I", "getDatabaseMajorVersion"),
      user = .jcall(meta, "S","getUserName"),
      tables = dbListTables(dbObj),
      temp_tables = .dbListTempTables(dbObj))
  }
)

#' @rdname SQLServerConnection-class
#' @importMethodsFrom DBI dbIsValid
#' @export

setMethod(f = 'dbIsValid', signature = 'SQLServerConnection',
  definition = function (dbObj, ...) !.jcall(dbObj@jc, "Z", "isClosed")
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
#' @importMethodsFrom RJDBC dbSendQuery
#' @export

setMethod("dbSendQuery",
  signature(conn = "SQLServerConnection", statement = "character"),
  def = function (conn, statement, ..., list=NULL) {
    statement <- as.character(statement)[1L]
    ## if the statement starts with {call or {?= call then we use CallableStatement
    if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement)))) {
      s <- .jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall",
        statement, check=FALSE)
      RJDBC:::.verify.JDBC.result(s,
        "Unable to execute JDBC callable statement ", statement)
      if (length(list(...)))
        RJDBC:::.fillStatementParameters(s, list(...))
      if (!is.null(list))
        RJDBC:::.fillStatementParameters(s, list)
      r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      RJDBC:::.verify.JDBC.result(r,
        "Unable to retrieve JDBC result set for ", statement)
    } else if (length(list(...)) || length(list)) {
      ## use prepared statements if there are additional arguments
      s <- .jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement",
        statement, check=FALSE)
      RJDBC:::.verify.JDBC.result(s,
        "Unable to execute JDBC prepared statement ", statement)
      if (length(list(...)))
        RJDBC:::.fillStatementParameters(s, list(...))
      if (!is.null(list))
        RJDBC:::.fillStatementParameters(s, list)
      r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      RJDBC:::.verify.JDBC.result(r,
        "Unable to retrieve JDBC result set for ", statement)
    } else {
      ## otherwise use a simple statement some DBs fail with the above)
      s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
      RJDBC:::.verify.JDBC.result(s,
        "Unable to create simple JDBC statement ", statement)
      r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
        as.character(statement)[1], check=FALSE)
      RJDBC:::.verify.JDBC.result(r,
        "Unable to retrieve JDBC result set for ", statement)
    }
    md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
    RJDBC:::.verify.JDBC.result(md,
      "Unable to retrieve JDBC result set meta data for ", statement,
      " in dbSendQuery")
    new("SQLServerResult", jr=r, md=md, stat=s, pull=.jnull())
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
#' @importMethodsFrom DBI dbIsValid
#' @export

setMethod (f = 'dbIsValid', signature = 'SQLServerResult',
  definition = function (dbObj) {
    .jcall(dbObj@jr, "Z", "isClosed")
  }
)

# fetch: Inherits from JDBCResult
# dbClearResult: Inherits from JDBCResult
# dbGetInfo: Inherits from JDBCResult
# dbColumnInfo: Inherits from JDBCResult

