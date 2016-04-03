# Driver -----------------------------------------------------------------

#' An S4 class to represent a SQL Server driver
#'
#' Extends the \code{\link[RJDBC:JDBCDriver-class]{JDBCDriver}} class and
#' uses jTDS driver.
#'
#' @seealso \code{\link{SQLServer}}
#' @keywords internal
#' @importClassesFrom RJDBC JDBCDriver
#' @export

setClass("SQLServerDriver", contains = "JDBCDriver")

#' Create a SQLServer driver
#'
#' This creates a SQL Server driver used to access SQL Server databases and is
#' based on the jTDS driver.
#'
#' @return An object of class \code{SQLServerDriver}.
#' @references
#' \href{http://jtds.sourceforge.net/}{jTDS project}
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @export

SQLServer <- function () {
  drv <- RJDBC::JDBC(driverClass = "net.sourceforge.jtds.jdbc.Driver",
    classPath = jdbc_class_path())
  new("SQLServerDriver", jdrv= drv@jdrv)
}

# Connection -------------------------------------------------------------

#' An S4 class to represent a SQL Server connection
#'
#' Extends the \code{\link[RJDBC:JDBCConnection-class]{JDBCConnection}}
#' class.
#'
#' @keywords internal
#' @importClassesFrom RJDBC JDBCConnection
#' @export

setClass("SQLServerConnection", contains = 'JDBCConnection')


# Result -----------------------------------------------------------------

#' An S4 class to represent a SQL Server result set
#'
#' Extends the \code{\link[RJDBC:JDBCResult-class]{JDBCResult}}
#' class.
#'
#' @keywords internal
#' @importClassesFrom RJDBC JDBCResult
#' @export

setClass ("SQLServerResult", contains = 'JDBCResult')


