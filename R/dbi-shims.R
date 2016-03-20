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

#' @importFrom dplyr db_query_rows sql_subquery
#' @export
db_query_rows.SQLServerConnection <- function(con, sql, ...) {
  # This is needed because DBI method doesn't name the resulting count and
  # the RSQLServer dbGetQuery->dbFetch query expects a name for each column.
  from <- sql_subquery(con, sql, "master")
  rows <- build_sql("SELECT count(*) AS COUNT FROM ", from, con = con)
  as.integer(dbGetQuery(con, rows)[[1]])
}


#' @importFrom dplyr db_query_fields
#' @export
db_query_fields.SQLServerConnection <- function (con, sql, ...) {
  # Using MSFT recommendation linked here:
  # https://github.com/imanuelcostigan/RSQLServer/issues/23
  fields <- build_sql("SELECT TOP 0 * FROM ", sql, con = con)
  qry <- dbSendQuery(con, fields)
  on.exit(dbClearResult(qry))
  jdbcColumnNames(qry@md)
}

#' @importFrom dplyr db_save_query
#' @export
db_save_query.SQLServerConnection <- function (con, sql, name, temporary = TRUE,
  ...) {
  # http://smallbusiness.chron.com/create-table-query-results-microsoft-sql-50836.html
  if (temporary) name <- paste0("#", name)
  tt_sql <- build_sql("SELECT * INTO ", ident(name), " FROM ",
    sql_subquery(con, sql), con = con)
  dbSendUpdate(con, tt_sql)
  name
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

#' @importFrom dplyr db_analyze ident build_sql
#' @export
db_analyze.SQLServerConnection <- function (con, table, ...) {
  # https://msdn.microsoft.com/en-us/library/ms188038(v=sql.90).aspx
  # "Only the table owner can create statistics on that table....
  # Requires ALTER permission on the table or view."
  # http://ss64.com/sql/stats_c.html
  name <- paste0("STAT_", random_ident_name())
  cols <- db_query_fields(con, ident(table))
  cols <- sql_vector(cols, collapse = ', ', con = con)
  sql <- build_sql("CREATE STATISTICS ", ident(name),
    " ON ", ident(table), " ", cols, con = con)
  dbSendUpdate(con, sql)
}

# Inherited db_create_index.DBIConnection method from dplyr

#' @importFrom dplyr db_explain
#' @export
db_explain.SQLServerConnection <- function (con, sql, ...) {
  # http://msdn.microsoft.com/en-us/library/ms187735.aspx
  # http://stackoverflow.com/a/7359705/1193481
  # dbSendUpdate(con, "SET SHOWPLAN_TEXT ON")
  # on.exit(dbSendUpdate(con, "SET SHOWPLAN_TEXT OFF"))
  # dbGetQuery(con, sql)
  message("SHOWPLAN will be supported in a future release of RSQLServer.")
}

