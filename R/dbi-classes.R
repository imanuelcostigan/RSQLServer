# Driver -----------------------------------------------------------------

#' SQL Server Driver class
#'
#' @seealso \code{\link{SQLServer}}
#' @keywords internal
#' @export

setClass("SQLServerDriver", contains = "DBIDriver", slots = c(jdrv = "jobjRef"))

#' Create SQLServerDriver
#'
#' This creates a SQLServerDriver object used to access SQL Server databases
#' and is based on the jTDS driver.
#'
#' @return An object of class \code{SQLServerDriver}.
#' @references
#' \href{http://jtds.sourceforge.net/doc/net/sourceforge/jtds/jdbc/Driver.html}{jTDS API doc}
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @export

SQLServer <- function () {
  rJava::.jinit(jdbc_class_path())
  drv <- rJava::.jnew("net.sourceforge.jtds.jdbc.Driver", check = FALSE)
  rJava::.jcheck(TRUE)
  if (rJava::is.jnull(drv)) drv <- rJava::.jnull()
  new("SQLServerDriver", jdrv = drv)
}

# Connection -------------------------------------------------------------

#' An S4 class to represent a SQL Server connection
#'
#' Extends the \code{\link[RJDBC:JDBCConnection-class]{JDBCConnection}}
#' class.
#'
#' @keywords internal
#' @export

setClass("SQLServerConnection", contains = 'DBIConnection')


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


