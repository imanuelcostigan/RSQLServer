# Driver -----------------------------------------------------------------

#' SQL Server Driver class
#'
#' @seealso \code{\link{SQLServer}}
#' @keywords internal
#' @export

setClass("SQLServerDriver", contains = "DBIDriver",
  slots = c(jdrv = "jobjRef"))


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


