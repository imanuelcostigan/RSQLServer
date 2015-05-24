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
  tt_sql <- dplyr::build_sql("CREATE TABLE ", dplyr::ident(name), " AS ", sql,
    con = con)
  dbGetQuery(con, tt_sql)
  name
}

#' @importFrom dplyr db_query_rows
#' @export
db_query_rows.SQLServerConnection <- function(con, sql, ...) {
  # https://msdn.microsoft.com/en-us/library/ms187316.aspx
  qry <- dplyr::build_sql(sql, con = con)
  dbSendQuery(con, qry)
  qry <- dplyr::build_sql("SELECT @@ROWCOUNT")
  as.integer(dbGetQuery(con, qry))
}

#' @importFrom dplyr db_create_table
#' @export
db_create_table.SQLServerConnection <- function (con, table, types,
  temporary = FALSE, ...) {
  # https://technet.microsoft.com/en-us/library/aa258255(v=sql.80).aspx
  # https://msdn.microsoft.com/en-us/library/ms174979.aspx
  assertthat::assert_that(assertthat::is.string(table), is.character(types))
  field_names <- dplyr::escape(dplyr::ident(names(types)),
    collapse = NULL, con = con)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE,
    collapse = ", ", con = con)
  sql <- dplyr::build_sql("CREATE TABLE ", if (temporary) dplyr::sql("#"),
    dplyr::ident(table), " ", fields, con = con)
  dbGetQuery(con, sql)
}

#' @importFrom dplyr db_insert_into
#' @export
db_insert_into.SQLServerConnection <- function (con, table, values, ...) {
  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)
  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString)
  # Write data frame to disk and then bulk insert into SQL
  tmp <- tempfile(fileext = ".csv")
  # https://msdn.microsoft.com/en-us/library/ms188365.aspx
  # https://technet.microsoft.com/en-us/library/aa225968(v=sql.80).aspx
  # ALSO see this for SS2000 eol: http://stackoverflow.com/posts/479916/revisions
  # Use Windows line endings as this is default in BULK INSERT (even for SS2000)
  write.table(values, tmp, sep = "\t", quote = FALSE, qmethod = "escape",
    row.names = FALSE, col.names = FALSE, eol = '\r\n')
  sql <- dplyr::build_sql("BULK INSERT ", dplyr::ident(table),
    " FROM ", encodeString(tmp), " WITH (TABLOCK)", con = con)
  dbGetQuery(con, sql)
  invisible()
}

#' @export
db_drop_table.DBIConnection <- function (con, table, force = FALSE, ...) {
  message("The 'force' argument is ignored.")
  dbRemoveTable(con, table)
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
