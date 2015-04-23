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
#' @export

setClass ("SQLServerResult", contains = 'JDBCResult')

#' @rdname SQLServerResult-class
#' @export

setMethod (f = 'dbIsValid', signature = 'SQLServerResult',
  definition = function (dbObj)
  {
    dbObj@jr$isClosed()
  }
)

# fetch: Inherits from JDBCResult
# dbClearResult: Inherits from JDBCResult
# dbGetInfo: Inherits from JDBCResult
# dbColumnInfo: Inherits from JDBCResult
