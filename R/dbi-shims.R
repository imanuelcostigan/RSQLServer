#' @importFrom dplyr db_list_tables
#' @export
db_list_tables.SQLServerConnection <- function (con) {
  dbListTables(con)
}

#' @importFrom dplyr db_has_table
#' @export
db_has_table.SQLServerConnection <- function (con, table) {
  # Like method for MySQL, RSQLServer has no way to list temporary tables, so we
  # always NA to skip any local checks and rely on the database to throw
  # informative errors
  # See: https://github.com/imanuelcostigan/RSQLServer/issues/29
  NA
}

# This is needed because DBI method doesn't name the resulting count and
# the RSQLServer dbGetQuery->dbFetch query expects a name for each column.

#' @importFrom dplyr db_query_rows
#' @export
db_query_rows.SQLServerConnection <- function(con, sql, ...) {
  from <- sql_subquery(con, sql, "master")
  rows <- build_sql("SELECT count(*) AS COUNT FROM ", from, con = con)
  as.integer(dbGetQuery(con, rows)[[1]])
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
  on.exit(dbClearResult(qry))
  jdbcColumnNames(qry@md)
}

# https://technet.microsoft.com/en-us/library/aa224033(v=sql.80).aspx
# Using [ doens't work
# https://github.com/hadley/dplyr/issues/1164
# # @importFrom dplyr sql_escape_ident
# # @export
# sql_escape_ident.SQLServerConnection <- function (con, x) {
#   dplyr::sql_quote(x, "[")
# }

#' @importFrom dplyr db_save_query
#' @export
db_save_query.SQLServerConnection <- function (con, sql, name, temporary = TRUE,
  ...) {
  # https://msdn.microsoft.com/en-us/library/ms174979.aspx
  prefix <- if (temporary) "#" else ""
  name <- paste0(prefix, name)
  tt_sql <- dplyr::build_sql("CREATE TABLE ", dplyr::ident(name), " AS ",
    sql, con = con)
  dbGetQuery(con, tt_sql)
  name
}

#
# db_query_row method inherits from DBIConnection method.
#

#' @importFrom dplyr db_create_table
#' @export
db_create_table.SQLServerConnection <- function (con, table, types,
  temporary = FALSE, ...) {
  # https://technet.microsoft.com/en-us/library/aa258255(v=sql.80).aspx
  # https://msdn.microsoft.com/en-us/library/ms174979.aspx
  assertthat::assert_that(assertthat::is.string(table), is.character(types))
  # Don't overwrite because this isn't default SQL Server behaviour. User
  # should drop table before creating table with same name.
  if (temporary) table <- paste0("#", table)
  dbWriteTable(con, table, types, overwrite = FALSE, append = FALSE)
}

#' @importFrom dplyr db_insert_into
#' @export
db_insert_into.SQLServerConnection <- function (con, table, values, ...) {
  # Assumes table already exists (at least dplyr's SQLite method assumes so)
  # So don't overwrite and simply append.
  dbWriteTable(con, table, values, overwrite = FALSE, append = TRUE)
}

#' @importFrom dplyr db_drop_table
#' @export
db_drop_table.SQLServerConnection <- function (con, table, force = FALSE, ...) {
  message("The 'force' argument is ignored.")
  # Work around RJDBC bug #20
  # https://github.com/s-u/RJDBC/issues/20
  # https://github.com/imanuelcostigan/RSQLServer/issues/30
  res <- dbRemoveTable(con, table)
  identical(res, logical(0)) || isTRUE(res)
}

#' @importFrom dplyr db_analyze
#' @export
db_analyze.SQLServerConnection <- function (con, table, ...) {
#   https://msdn.microsoft.com/en-us/library/ms188038(v=sql.90).aspx
#   http://ss64.com/sql/stats_c.html
#   name <- paste0("stat_", random_ident_name())
#   sql <- build_sql("CREATE STATISTICS ", name, " ON ", ident(table), con = con)
#   dbGetQuery(con, sql)
#   Not enabled at present so just return TRUE
  TRUE
}

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
