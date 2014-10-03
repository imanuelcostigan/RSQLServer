#' An S4 class to represent a SQL Server driver
#'
#' This class extends the \code{\link[RJDBC:JDBCDriver-class]{JDBCDriver}} class
#' to represent a SQL Server driver used to access SQL Server databases. The
#' JDBCDriver class itself extends DBIDriver.
#'
#' @slot identifier.quote quote character for a SQL Server identifier can be a
#' single quotation mark (\code{\'}), a left or right bracket (\code{[]},
#' defaults to \code{[}), or a double quotation mark (\code{\"}).
#' @slot jdrv Java object reference to an instance of the SQL Server driver if
#' the driver can be instantiated by a default constructor. This object is only
#' used as a fall-back when the driver manager fails to find a driver.
#' @references
#' \href{http://msdn.microsoft.com/en-us/library/ms175874.aspx}{SQL Server identifiers}
#' @export
setClass("SQLServerDriver", contains = "JDBCDriver")
