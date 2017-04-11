# Driver -----------------------------------------------------------------

#' SQL Server Driver class
#'
#' @seealso \code{\link{SQLServer}}
#' @references
#' [Microsoft's implementation of JDBC SQLServerDriver class](https://docs.microsoft.com/en-us/sql/connect/jdbc/reference/sqlserverdriver-class)
#' @keywords internal
#' @importClassesFrom rJava jobjRef
#' @export

setClass("SQLServerDriver", contains = "DBIDriver",
  slots = c(jdrv = "jobjRef"))


# Connection -------------------------------------------------------------

#' SQL Server Connection class
#'
#' @keywords internal
#' @references
#' [Microsoft's implementation of JDBC SQLServerDataSource class](https://docs.microsoft.com/en-us/sql/connect/jdbc/reference/sqlserverdatasource-class)
#' @export

setClass("SQLServerConnection", contains = 'DBIConnection',
  slots = c(jds = "jobjRef", jc = "jobjRef"))


# Result -----------------------------------------------------------------

#' SQL Server Result classes
#'
#' The \code{SQLServerPreResult} class extends the \code{DBIResult} class, the
#' \code{SQLServerResult} class extends the \code{SQLServerPreResult} class,
#' while the \code{SQLServerUpdateResult} class extends the
#' \code{SQLServerResult} class. The \code{SQLServerUpdateResult} class is
#' created by a call to \code{dbSendStatement} as the JDBC API does not return a
#' ResultSet but rather an integer value for non-query statements. The
#' \code{dbGetRowsAffected} called on \code{SQLServerUpdateResult} returns the
#' value produced by the JDBC API. The \code{SQLServerPreResult} class wraps a
#' JDBC PreparedStatement that has yet to be executed and is necessary for
#' \code{dbBind} to function per the DBI spec (taking a DBIResult as an input
#' and returning another as output). This contrasts to the JDBC interface where
#' binding occurs on Statement classes and not on ResultSet classes.
#'
#' @keywords internal
#' @rdname SQLServerResult-class
#' @export

setClass ("SQLServerPreResult", contains = 'DBIResult',
  slots = c(stat = "jobjRef"))

#' @keywords internal
#' @rdname SQLServerResult-class
#' @references
#' [Microsoft's implementation of JDBC SQLServerResultSet class](https://docs.microsoft.com/en-us/sql/connect/jdbc/reference/sqlserverresultset-class)
#' @export

setClass ("SQLServerResult", contains = 'SQLServerPreResult',
  slots = c(jr = "jobjRef", md = "jobjRef", pull = "jobjRef",
    # Use RC so we can keep track of records fetched within fetch() even
    # is calling multiple times.
    rows_fetched = "RowCounter"))

#' @keywords internal
#' @rdname SQLServerResult-class
#' @export

setClass ("SQLServerUpdateResult", contains = 'SQLServerResult',
  slots = c(rows_affected = "numeric"))

