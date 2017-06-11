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

#' @importFrom dplyr db_save_query sql_subquery
#' @export
db_save_query.SQLServerConnection <- function (con, sql, name, temporary = TRUE,
  ...) {
  # http://smallbusiness.chron.com/create-table-query-results-microsoft-sql-50836.html
  if (temporary) name <- paste0("#", name)
  qry <- build_sql("SELECT * INTO ", ident(name), " FROM ",
    sql_subquery(con, sql), con = con)
  dbExecute(con, qry)
  name
}

#' @importFrom dplyr db_create_table
#' @importFrom dbplyr build_sql ident sql_vector
#' @export

db_create_table.SQLServerConnection <- function(con, table, types,
  temporary = FALSE, ...) {
  assertthat::assert_that(assertthat::is.string(table), is.character(types))
  sql <- sqlCreateTable(con, table, types, temporary = temporary)
  dbExecute(con, sql)
  # Needs to return table name as temp tables are prefixed by `#` in SQL Server
  if (temporary) prefix <- "#" else prefix <- ""
  paste0(prefix, table)
}

#' @importFrom dplyr db_write_table
#' @export
db_write_table.SQLServerConnection <- function(con, table, types, values,
  temporary = FALSE, ...) {
  # Implementation in dbplyr escapes `table` using `dbi_quote` which returns the
  # value `'<table>'` which causes MSSQL to throw an "Incorrect syntax near
  # `'<tablename>'`" error.
  dbWriteTable(
    con,
    name = dbQuoteIdentifier(con, table),
    value = values,
    field.types = types,
    temporary = temporary,
    row.names = FALSE
  )
}


#' @importFrom dplyr db_insert_into
#' @export

db_insert_into.SQLServerConnection <- function(con, table, values, temporary, ...) {
  # Temp tables cannot be appended to in SQL Server as their existence cannot
  # be checked by the API.
  if (temporary) {
    append <- FALSE
    overwrite <- TRUE
  } else {
    append <- TRUE
    overwrite <- FALSE
  }
  dbWriteTable(con, table, values, overwrite = overwrite, append = append)
}

#' @importFrom dplyr db_drop_table
#' @export

db_drop_table.SQLServerConnection <- function(con, table, force = FALSE, ...) {
  # IF EXISTS only supported by SQL Server 2016 (v. 13) and above.
  qry <- paste0("DROP TABLE ",
    if (force && dbGetInfo(con)$db.version > 12) "IF EXISTS ",
    dbQuoteIdentifier(con, table))
  assertthat::is.number(dbExecute(con, qry))
}

#' @importFrom dplyr db_analyze
#' @export
db_analyze.SQLServerConnection <- function (con, table, ...) {
  TRUE
}

# Inherited db_create_index.DBIConnection method from dplyr

#' @importFrom dplyr db_explain %>%
#' @export
db_explain.SQLServerConnection <- function (con, sql, ...) {
  # SET SHOWPLAN_ALL available from SQL Server 2000 on.
  # https://technet.microsoft.com/en-us/library/aa259203(v=sql.80).aspx
  # http://msdn.microsoft.com/en-us/library/ms187735.aspx
  # http://stackoverflow.com/a/7359705/1193481
  dbSendStatement(con, "SET SHOWPLAN_ALL ON")
  on.exit(dbSendStatement(con, "SET SHOWPLAN_ALL OFF"))
  res <- dbGetQuery(con, sql) %>%
    dplyr::select_("StmtId", "NodeId", "Parent", "PhysicalOp", "LogicalOp",
      "Argument", "TotalSubtreeCost")
  paste(utils::capture.output(print(res)), collapse = "\n")
}

#' @export
#' @importFrom dbplyr db_copy_to
#' @importFrom dplyr db_write_table db_data_type
db_copy_to.SQLServerConnection <- function(con, table, values,
  overwrite = FALSE, types = NULL, temporary = TRUE,
  unique_indexes = NULL, indexes = NULL,
  analyze = TRUE, ...) {

  # Modified version of dbplyr method.
  # SQL Server doesn't have ANALYZE TABLE support so this part of
  # db_copy_to() has been dropped

  types <- types %||% db_data_type(con, values)
  names(types) <- names(values)
  if (overwrite) {
    db_drop_table(con, table, force = TRUE)
  }
  db_write_table(con, table, types = types, values = values,
    temporary = temporary)
  db_create_indexes(con, table, unique_indexes, unique = TRUE)
  db_create_indexes(con, table, indexes, unique = FALSE)
  table
}

#' @importFrom dplyr db_create_indexes
#' @importFrom dbplyr db_compute
#' @export
db_compute.SQLServerConnection <- function(con, table, sql, temporary = TRUE,
  unique_indexes = list(), indexes = list(), ...) {
  # Modified from dbplyr because db_save_query returns a temp table name which
  # must be used by subsequent method calls.
  if (!is.list(indexes)) {
    indexes <- as.list(indexes)
  }
  if (!is.list(unique_indexes)) {
    unique_indexes <- as.list(unique_indexes)
  }

  table <- db_save_query(con, sql, table, temporary = temporary)
  db_create_indexes(con, table, unique_indexes, unique = TRUE)
  db_create_indexes(con, table, indexes, unique = FALSE)

  table
}

