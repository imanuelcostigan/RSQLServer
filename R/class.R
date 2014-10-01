##=== Add sqljdbc4.jar to classpath

##=== SQLServerDriver

setClass("SQLServerDriver", contains = "JDBCDriver")

SQLServer <- function (identifier.quote=NA)
{
  require(RJDBC)
  drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
    system.file("java", "sqljdbc4.jar", package="RSQLServer"))
  new("SQLServerDriver", identifier.quote=as.character(identifier.quote),
    jdrv= drv@jdrv)
}

setMethod("dbConnect", "SQLServerDriver",
  def = function (drv, url, user='', password='', ...)
  {
    jc <- .jcall("java/sql/DriverManager", "Ljava/sql/Connection;",
      "getConnection", as.character(url)[1], as.character(user)[1],
      as.character(password)[1], check=FALSE)
    if (is.jnull(jc) && !is.jnull(drv@jdrv))
    {
      # ok one reason for this to fail is its interaction with rJava's
      # class loader. In that case we try to load the driver directly.
      oex <- .jgetEx(TRUE)
      p <- .jnew("java/util/Properties")
      if (length(user)==1 && nchar(user))
        .jcall(p, "Ljava/lang/Object;", "setProperty", "user", user)
      if (length(password)==1 && nchar(password))
        .jcall(p, "Ljava/lang/Object;", "setProperty", "password", password)
      l <- list(...)
      if (length(names(l))) {
        for (n in names(l))
          .jcall(p, "Ljava/lang/Object;", "setProperty", n, as.character(l[[n]]))
      }
      jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect",
        as.character(url)[1], p)
    }
    .verify.JDBC.result(jc, "Unable to connect SQL Server to ", url)
    new("SQLServerConnection", jc = jc, identifier.quote = drv@identifier.quote)
  },
  valueClass = "SQLServerConnection"
)

### SQLServerConnection

setClass("SQLServerConnection",
  representation("JDBCConnection", jc = "jobjRef",
    identifier.quote = "character")
)

setMethod("dbSendQuery",
  signature(conn = "SQLServerConnection", statement = "character"),
  def = function (conn, statement, ..., list=NULL)
  {
    statement <- as.character(statement)[1L]
    ## if the statement starts with {call or {?= call then we use
    ## CallableStatement
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
      ## use prepared statements if there are additional arguments
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
      ## otherwise use a simple statement some DBs fail with the above)
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

setMethod("dbExistsTable", "SQLServerConnection",
  def=function (conn, name, ...)
  {
    s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    .verify.JDBC.result(s, "Unable to create simple JDBC statement ", statement)
    r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
      paste0("SELECT TOP 0 * FROM ", name), check=FALSE)
    .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ", statement)
    TRUE
  }
)

##=== SQLServerResult
## jr - result set, md - metadata, stat - statement
## Since the life of a result set depends on the life of the statement, we have to explicitly
## save the later as well (and close both at the end)

setClass("SQLServerResult",
  representation("JDBCResult", jr = "jobjRef", md = "jobjRef", stat = "jobjRef",
    pull = "jobjRef")
)

setMethod("dbHasCompleted", "SQLServerResult",
  def = function(res, ...) TRUE,
  valueClass = "logical"
)

.verify.JDBC.result <- function (result, ...) {
  if (is.jnull(result))
  {
    x <- .jgetEx(TRUE)
    if (is.jnull(x))
      stop(...)
    else
      stop(..., " (", .jcall(x, "S", "getMessage"), ")")
  }
}
