# Driver -----------------------------------------------------------------

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
#' @aliases  dbListConnections,SQLServerDriver-method
#' dbGetInfo,SQLServerDriver-method
#' dbUnloadDriver,SQLServerDriver-method
#' @references
#' \href{http://jtds.sourceforge.net/}{jTDS project}
#' @importClassesFrom RJDBC JDBCDriver
#' @export

setClass("SQLServerDriver", contains = "JDBCDriver")

#' @param identifier.quote  quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
#' defaults to \code{[}), or a double quotation mark (\code{\"}), the last of
#' which is the default.
#' @return An object of class \linkS4class{SQLServerDriver}.
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @rdname SQLServerDriver-class
#' @aliases SQLServer
#' @export

SQLServer <- function (identifier.quote="\"") {
  drv <- RJDBC::JDBC(driverClass = "net.sourceforge.jtds.jdbc.Driver",
    classPath = jdbc_class_path())
  new("SQLServerDriver", identifier.quote = identifier.quote, jdrv= drv@jdrv)
}

# Connection -------------------------------------------------------------

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
#' @importClassesFrom RJDBC JDBCConnection
#' @export

setClass("SQLServerConnection", contains = 'JDBCConnection')


# Result -----------------------------------------------------------------

#' An S4 class to represent a SQL Server result set
#'
#' This class extends the \code{\link[RJDBC:JDBCResult-class]{JDBCResult}}
#' class to represent a SQL Server result set
#'
#' @slot jr Java reference to the JDBC result set
#' @slot md Java reference to the JDBC result set meta data
#' @slot pull Java reference to the JDBC result pull helper class (can be null
#' reference before first pull)
#' @slot stat Java reference to the JDBC statement which generated this result
#' @importClassesFrom RJDBC JDBCResult
#' @export

setClass ("SQLServerResult", contains = 'JDBCResult')


