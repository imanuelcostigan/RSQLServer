#' @include DBDriver.R
NULL

#' An S4 class to represent a SQL Server connection
#'
#' This class extends the \code{\link[RJDBC:JDBCConnection-class]{JDBCConnection}}
#' class to represent a SQL Server connection.
#'
#' @param dbObj a \code{\linkS4class{SQLServerConnection}}
#' @param conn a \code{\linkS4class{SQLServerConnection}}
#'
#' @slot jc Java object representing the connection.
#' @slot identifier.quote quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]}), or a
#' double quotation mark (\code{\"}). Usually inherited from
#' \code{\linkS4class{SQLServerDriver}}.
#' @aliases dbGetInfo,SQLServerConnection-method
#' dbIsValid,SQLServerConnection-method
#' dbSendQuery, SQLServerConnection-method
#' @export

setClass("SQLServerConnection", contains = 'JDBCConnection')

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
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerConnection',
  definition = function (dbObj, ...)
  {
    meta <- .jcall(dbObj@jc, "Ljava/sql/DatabaseMetaData;", "getMetaData")
    list(db.product.name = .jcall(meta, "S", "getDatabaseProductName"),
      db.version = .jcall(meta, "I", "getDatabaseMajorVersion"),
      user = .jcall(meta, "S","getUserName"))
  }
)

#' @rdname SQLServerConnection-class
#' @export

setMethod(f = 'dbIsValid', signature = 'SQLServerConnection',
  definition = function (dbObj, ...) !.jcall(dbObj@jc, "Z", "isClosed")
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

#' Send query to SQL Server
#'
#' This is basically a copy of RJDBC's \code{\link[RJDBC:JDBCConnection-methods]{dbSendQuery}}
#' method for JDBCConnection.
#'
#' @param statement SQL statement to execute
#' @param ... additional arguments to prepared statement substituted for "?"
#' @param list undocumented
#' @return a \code{\linkS4class{SQLServerResult}} object
#' @rdname SQLServerConnection-class
#' @export

setMethod("dbSendQuery",
  signature(conn = "SQLServerConnection", statement = "character"),
  def = function (conn, statement, ..., list=NULL)
  {
    statement <- as.character(statement)[1L]
    if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement))))
    {
      s <- .jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall",
        statement, check=FALSE)
      .verify.JDBC.result(s, "Unable to execute JDBC callable statement ",
        statement)
      if (length(list(...)))
        .fillStatementParameters(s, list(...))
      if (!is.null(list))
        .fillStatementParameters(s, list)
      r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
        statement)
    } else if (length(list(...)) || length(list))
    {
      s <- .jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement",
        statement, check=FALSE)
      .verify.JDBC.result(s, "Unable to execute JDBC prepared statement ",
        statement)
      if (length(list(...)))
        .fillStatementParameters(s, list(...))
      if (!is.null(list))
        .fillStatementParameters(s, list)
      r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
        statement)
    } else
    {

      s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
      .verify.JDBC.result(s, "Unable to create simple JDBC statement ",
        statement)
      r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
        as.character(statement)[1], check=FALSE)
      .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
        statement)
    }
    md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
    .verify.JDBC.result(md, "Unable to retrieve JDBC result set meta data for ",
      statement, " in dbSendQuery")
    new("SQLServerResult", jr=r, md=md, stat=s, pull=.jnull())
  }
)


# Copied from RJDBC:
# https://github.com/s-u/RJDBC/blob/01c55dfe76e039a37ccda732d7332325222da8c8/R/class.R
.verify.JDBC.result <- function (result, ...) {
  if (is.jnull(result)) {
    x <- .jgetEx(TRUE)
    if (is.jnull(x))
      stop(...)
    else
      stop(...," (",.jcall(x, "S", "getMessage"),")")
  }
}
.fillStatementParameters <- function(s, l) {
  for (i in 1:length(l)) {
    v <- l[[i]]
    if (is.na(v)) { # map NAs to NULLs (courtesy of Axel Klenk)
      sqlType <- if (is.integer(v)) 4 else if (is.numeric(v)) 8 else 12
      .jcall(s, "V", "setNull", i, as.integer(sqlType))
    } else if (is.integer(v))
      .jcall(s, "V", "setInt", i, v[1])
    else if (is.numeric(v))
      .jcall(s, "V", "setDouble", i, as.double(v)[1])
    else
      .jcall(s, "V", "setString", i, as.character(v)[1])
  }
}
