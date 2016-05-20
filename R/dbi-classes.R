# Driver -----------------------------------------------------------------

#' SQL Server Driver class
#'
#' @seealso \code{\link{SQLServer}}
#' @keywords internal
#' @export

setClass("SQLServerDriver", contains = "DBIDriver",
  slots = c(jdrv = "jobjRef"))

#' SQLServerDriver class and methods
#'
#' \code{SQLServer()} creates a \code{SQLServerDriver} object and is based on
#' the jTDS driver. Methods dispatching on \code{SQLServerDriver} are also
#' documented herein.
#'
#' @references
#' \href{http://jtds.sourceforge.net/doc/net/sourceforge/jtds/jdbc/Driver.html}{jTDS API doc for Driver class}
#' @examples
#' \dontrun{
#' SQLServer()
#' }
#' @rdname SQLServer
#' @export

SQLServer <- function () {
  rJava::.jinit(jdbc_class_path())
  drv <- rJava::.jnew("net.sourceforge.jtds.jdbc.Driver", check = FALSE)
  rJava::.jcheck(TRUE)
  if (rJava::is.jnull(drv)) drv <- rJava::.jnull()
  new("SQLServerDriver", jdrv = drv)
}

# Connection -------------------------------------------------------------

#' SQL Server Connection class
#'
#' @keywords internal
#' @export

setClass("SQLServerConnection", contains = 'DBIConnection',
  slots = c(jc = "jobjRef"))


# Result -----------------------------------------------------------------

#' SQL Server Result class
#'
#' @keywords internal
#' @export

setClass ("SQLServerResult", contains = 'DBIResult',
  slots = c(jr = "jobjRef", md = "jobjRef", stat = "jobjRef", pull = "jobjRef"))


