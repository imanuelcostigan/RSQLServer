#' @importFrom dplyr db_list_tables
#' @export
db_list_tables.SQLServerConnection <- function (con) {
  RJDBC::dbListTables(con)
}

#' @importFrom dplyr db_has_table
#' @export
db_has_table.SQLServerConnection <- function (con, table) {
  table %in% db_list_tables(con)
}

# Following shim written partly because:
# https://github.com/hadley/dplyr/issues/1107
# But this may be more efficent than dplyr's DBIConnection method
#' @importFrom dplyr db_query_fields
#' @export
db_query_fields.SQLServerConnection <- function (con, sql, ...) {
  # Condition WHERE 0 = 1 will force query to return 0 records.
  fields <- build_sql("SELECT * FROM ", sql, " WHERE 0=1", con = con)
  qry <- dbSendQuery(con, fields)
  on.exit(RJDBC::dbClearResult(qry))
  sqlServerListFields(qry)
}

#' @importFrom dplyr sql_escape_ident
#' @export
sql_escape_ident.SQLServerConnection <- function (con, x) {
  dplyr::sql_quote(x, "[")
}

#
# #' @importFrom dplyr db_query_rows
# #' @export
# db_query_rows.SQLServerConnection <- function(con, sql, ...) {
#   qry <- dplyr::build_sql(sql, con = con)
#   dbSendQuery(con, qry)
#   qry <- dplyr::build_sql("SELECT @@ROWCOUNT")
#   as.integer(dbGetQuery(con, qry))
# }
#
# #' @importFrom dplyr db_save_query
# #' @export
# db_save_query.SQLServerConnection <- function (con, sql, name, temporary = TRUE,
#   ...) {
#   tt_sql <- dplyr::build_sql("SELECT * ", "INTO ", dplyr::ident(name),
#     " FROM (", sql, ") AS MASTER")
#   js <- J(con@jc, "createStatement")
#   J(js, "execute", as.character(tt_sql)[1])
#   name
# }
#
# #' @importFrom dplyr db_explain
# #' @export
# db_explain.SQLServerConnection <- function (con, sql, ...) {
#   message('SQL Server does not provide an EXPLAIN statement.')
#   # Though may be possible to use SHOWPLAN
#   # http://msdn.microsoft.com/en-us/library/ms187735.aspx
#   # http://stackoverflow.com/questions/7359702/how-do-i-obtain-a-query-execution-plan
#   # Maybe use same strategy as db_query_rows
# }
#
