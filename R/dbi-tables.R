#' @export
#' @rdname SQLServerConnection-class
setMethod("sqlCreateTable", "SQLServerConnection",
  function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
    if (temporary) table <- paste0("#", table)
    table <- dbQuoteIdentifier(con, table)
    if (is.data.frame(fields)) {
      fields <- sqlRownamesToColumn(fields, row.names)
      fields <- vapply(fields, function(x) dbDataType(con, x), character(1))
    }
    field_names <- dbQuoteIdentifier(con, names(fields))
    field_types <- unname(fields)
    fields <- paste0(field_names, " ", field_types)
    SQL(paste0("CREATE TABLE ", table, " (\n",
      "  ", paste(fields, collapse = ",\n  "), "\n)\n"))
})

# Modified from DBI v0.4.2
sqlAppendTableTemplate <- function (con, table, values, row.names = NA,
  prefix = "?", ...)  {
  table <- dbQuoteIdentifier(con, table)
  values <- sqlRownamesToColumn(values[0, , drop = FALSE],
    row.names)
  fields <- dbQuoteIdentifier(con, names(values))
  SQL(paste0("INSERT INTO ", table, "\n", "  (",
    paste(fields, collapse = ", "), ")\n", "VALUES\n",
    paste0("  (", paste0(rep(prefix, length(fields)), collapse = ", "), ")",
      collapse = ",\n")))
}
