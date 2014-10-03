# ################################################################################
# ## Class: DBIDriver
# ################################################################################
#
# #' An S4 class to represent a SQL Server driver
# #'
# #' This class extends the \code{\link[RJDBC:JDBCDriver-class]{JDBCDriver}} class
# #' to represent a SQL Server driver used to access SQL Server databases.
# #'
# #' @slot identifier.quote quote character for a SQL Server identifier can be a
# #' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
# #' defaults to \code{[}), or a double quotation mark (\code{\"}).
# #' @slot jdrv Java object reference to an instance of the SQL Server driver if
# #' the driver can be instantiated by a default constructor. This object is only
# #' used as a fall-back when the driver manager fails to find a driver.
# #' @references
# #' \href{http://msdn.microsoft.com/en-us/library/ms175874.aspx}{SQL Server identifiers}
# #' @export
# setClass("SQLServerDriver", contains = "JDBCDriver")
#
#
#
# #' Initialise the SQL Server engine
# #'
# #' This function initialises the SQL Server engine using the
# #' \code{\link[RJDBC]{JDBC}} interface.
# #'
# #' @param identifier.quote  quote character for a SQL Server identifier can be a
# #' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
# #' defaults to \code{[}), or a double quotation mark (\code{\"}).
# #' @return An object of class \linkS4class{SQLServerDriver}.
# #' @references
# #' \href{http://msdn.microsoft.com/en-us/library/ms378526(v=sql.110).aspx}{Using the JDBC (SQL Server) Driver}
# #' @examples
# #' \dontrun{
# #' SQLServer()
# #' }
# #' @export
# SQLServer <- function (identifier.quote="[")
# {
#   drv <- RJDBC::JDBC(
#     driverClass = "com.microsoft.sqlserver.jdbc.SQLServerDriver",
#     classPath = jdbc_class_path())
#   new("SQLServerDriver", identifier.quote=as.character(identifier.quote),
#     jdrv= drv@jdrv)
# }
#
#
# ################################################################################
# ## Class: DBIConnection
# ################################################################################
#
# #' An S4 class to represent a SQL Server connection
# #'
# #' This class extends the \code{\link[RJDBC:JDBCConnection-class]{JDBCConnection}}
# #' class to represent a DBI compliant SQL Server connection.
# #'
# #' @slot jc Java object representing the connection.
# #' @slot identifier.quote quote character for a SQL Server identifier can be a
# #' single quotation mark (\code{\'}), a left or right bracket (\code{[]}), or a
# #' double quotation mark (\code{\"}). Usually inherited from
# #' \code{\linkS4class{SQLServerDriver}}.
# #' @references
# #' \href{http://msdn.microsoft.com/en-us/library/ms175874.aspx}{SQL Server identifiers}
# #' @export
# setClass("SQLServerConnection", contains = 'JDBCConnection')
#
# build_ss_connection_url <- function (server, port = NULL, instance = NULL)
# {
#   # http://msdn.microsoft.com/en-us/library/ms378428(v=sql.110).aspx
#   # General form:
#   # jdbc:sqlserver://[serverName[\instanceName][:portNumber]][;property=value[;property=value]]
#   instance <- if (is.null(instance)) '' else paste0('\\', instance)
#   port <- if (is.null(port)) '' else paste0(':', port)
#   paste0('jdbc:sqlserver://', server, instance, port, ';')
# }
#
# #' @export
# setMethod(f = 'dbConnect', signature = "SQLServerDriver",
#   definition = function (drv, server, port = NULL, instance = NULL, ...)
#   {
#     dots <- list(...)
#     properties <- .jnew('java/util/Properties')
#     for (property in names(dots))
#     {
#       .jcall(properties, "Ljava/lang/Object;", "setProperty", property,
#         dots[[property]])
#     }
#     url <- build_ss_connection_url(server, port, instance)
#     jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", url[1], properties)
#     new("SQLServerConnection", jc = jc, identifier.quote = drv@identifier.quote)
#   }
# )
#
# # setMethod("dbSendQuery",
# #   signature(conn = "SQLServerConnection", statement = "character"),
# #   def = function (conn, statement, ..., list=NULL)
# #   {
# #     statement <- as.character(statement)[1L]
# #     ## if the statement starts with {call or {?= call then we use
# #     ## CallableStatement
# #     if (isTRUE(as.logical(grepl("^\\{(call|\\?= *call)", statement))))
# #     {
# #       s <- .jcall(conn@jc, "Ljava/sql/CallableStatement;", "prepareCall",
# #         statement, check=FALSE)
# #       .verify.JDBC.result(s, "Unable to execute JDBC callable statement ",
# #         statement)
# #       if (length(list(...)))
# #         .fillStatementParameters(s, list(...))
# #       if (!is.null(list))
# #         .fillStatementParameters(s, list)
# #       r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
# #       .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
# #         statement)
# #     } else if (length(list(...)) || length(list))
# #     {
# #       ## use prepared statements if there are additional arguments
# #       s <- .jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement",
# #         statement, check=FALSE)
# #       .verify.JDBC.result(s, "Unable to execute JDBC prepared statement ",
# #         statement)
# #       if (length(list(...)))
# #         .fillStatementParameters(s, list(...))
# #       if (!is.null(list))
# #         .fillStatementParameters(s, list)
# #       r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
# #       .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
# #         statement)
# #     } else
# #     {
# #       ## otherwise use a simple statement some DBs fail with the above)
# #       s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
# #       .verify.JDBC.result(s, "Unable to create simple JDBC statement ",
# #         statement)
# #       r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
# #         as.character(statement)[1], check=FALSE)
# #       .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",
# #         statement)
# #     }
# #     md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
# #     .verify.JDBC.result(md, "Unable to retrieve JDBC result set meta data for ",
# #       statement, " in dbSendQuery")
# #     new("SQLServerResult", jr=r, md=md, stat=s, pull=.jnull())
# #   }
# # )
# #
# # setMethod("dbExistsTable", "SQLServerConnection",
# #   def=function (conn, name, ...)
# #   {
# #     s <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
# #     .verify.JDBC.result(s, "Unable to create simple JDBC statement ", statement)
# #     r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery",
# #       paste0("SELECT TOP 0 * FROM ", name), check=FALSE)
# #     .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ", statement)
# #     TRUE
# #   }
# # )
# #
# # ##=== SQLServerResult
# # ## jr - result set, md - metadata, stat - statement
# # ## Since the life of a result set depends on the life of the statement, we have to explicitly
# # ## save the later as well (and close both at the end)
# #
# # setClass("SQLServerResult",
# #   representation("JDBCResult", jr = "jobjRef", md = "jobjRef", stat = "jobjRef",
# #     pull = "jobjRef")
# # )
# #
# # setMethod("dbHasCompleted", "SQLServerResult",
# #   def = function(res, ...) TRUE,
# #   valueClass = "logical"
# # )
# #
# # .verify.JDBC.result <- function (result, ...) {
# #   if (is.jnull(result))
# #   {
# #     x <- .jgetEx(TRUE)
# #     if (is.jnull(x))
# #       stop(...)
# #     else
# #       stop(..., " (", .jcall(x, "S", "getMessage"), ")")
# #   }
# # }
#
# jre_version <- function ()
# {
#   key <- readRegistry('SOFTWARE\\JavaSoft\\Java Runtime Environment\\')
#   as.numeric(key$CurrentVersion)
# }
#
# jdbc_root_path <- function ()
#   system.file('java', package = 'RSQLServer')
#
# jdbc_class_path <- function ()
# {
#   if (jre_version() <= 1.5)
#     file.path(jdbc_root_path(), 'sqljdbc.jar')
#   else
#     file.path(jdbc_root_path(), 'sqljdbc4.jar')
# }
