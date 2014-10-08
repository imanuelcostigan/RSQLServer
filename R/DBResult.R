#' An S4 class to represent a SQL Server connection
#'
#' This class extends the \code{\link[RJDBC:JDBCConnection-class]{JDBCConnection}}
#' class to represent a SQL Server connection.
#'
#' @slot jr Java reference to the JDBC result set
#' @slot md Java reference to the JDBC result set meta data
#' @slot pull Java reference to the JDBC result pull helper class (can be null
#' reference before first pull)
#' @slot stat Java reference to the JDBC statement which generated this result
#' @export

setClass("SQLServerResult", contains = 'JDBCResult')
