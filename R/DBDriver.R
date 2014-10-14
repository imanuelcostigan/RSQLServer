#' An S4 class to represent a SQL Server driver
#'
#' This class extends the \code{\link[RJDBC:JDBCDriver-class]{JDBCDriver}} class
#' to represent a SQL Server driver used to access SQL Server databases. This
#' should always be initialised with \code{SQLServer()}. JDBCDriver extends
#' DBIDriver. The package uses the jTDS driver set.
#'
#' @slot identifier.quote quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
#' defaults to \code{[}), or a double quotation mark (\code{\"}).
#' @slot jdrv Java object reference to an instance of the SQL Server driver if
#' the driver can be instantiated by a default constructor. This object is only
#' used as a fall-back when the driver manager fails to find a driver.
#' @references
#' \href{http://jtds.sourceforge.net/}{jTDS project}
#' @export

setClass("SQLServerDriver", contains = "JDBCDriver")

#' @param identifier.quote  quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
#' defaults to \code{[}), or a double quotation mark (\code{\"}).
#' @return An object of class \linkS4class{SQLServerDriver}.
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @rdname SQLServerDriver-class
#' @aliases SQLServer
#' @export

SQLServer <- function (identifier.quote="[")
{
  drv <- RJDBC::JDBC(driverClass = "net.sourceforge.jtds.jdbc.Driver",
    classPath = jdbc_class_path())
  new("SQLServerDriver", identifier.quote = identifier.quote, jdrv= drv@jdrv)
}

#' List active connections
#'
#' @param drv Object created by \code{\link{SQLServer}}
#' @param ... Other arguments passed on to methods. Not used here.
#' @return An empty list as JDBC driver doesn't maintain a list of active
#' connections
#' @examples
#' \dontrun{
#' dbListConnections(SQLServer())
#' }
#' @export

setMethod(f = 'dbListConnections', signature = 'SQLServerDriver',
  definition = function (drv, ...)
  {
    warning ("JDBC driver maintains no list of active connections.")
    list()
  }
)

#' Get driver info
#'
#' @param dbObj Object created by \code{\link{SQLServer}}
#' @param ... other arguments to methods. Not used here.
#' @return A list containing the name and driver version used by \code{drv}
#' @examples
#' \dontrun{
#' dbGetInfo(SQLServer())
#' }
#' @export

setMethod(f = 'dbGetInfo', signature = 'SQLServerDriver',
  definition = function (dbObj, ...)
  {
    list(name = 'RSQLServer (jTDS)', driver.version = dbObj@jdrv$getVersion())
  }
)

#' Unload SQLServer driver
#'
#' Not implemented. Simply returns \code{TRUE}. See
#' \code{\link[RJDBC]{JDBCDriver-methods}}.
#'
#' @param drv Object created by \code{\link{SQLServer}}
#' @param ... any other arguments are passed to the driver. Not used here.
#' @return Always logical \code{TRUE}.
#' @examples
#' \dontrun{
#' drv <- SQLServer()
#' dbUnloadDriver(drv)
#' }
#' @export

setMethod("dbUnloadDriver", "SQLServerDriver", function(drv, ...) TRUE)
