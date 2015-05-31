jdbcColumnMeta <- function (md, meta, jtype) {
  n <- rJava::.jcall(md, "I", "getColumnCount")
  rtype <- j_to_r_type(jtype)
  res <- vector(rtype, length = n)
  for (i in seq_along(n)) {
    res[i] <- rJava::.jcall(md, jtype, paste0("getColumn", meta), i)
  }
  res
}

jdbcColumnNames <- function (md) {
  jdbcColumnMeta(md, "Name", "S")
}

jdbcColumnTypeNames <- function (md) {
  jdbcColumnMeta(md, "TypeName", "S")
}

jdbcColumnTypes <- function (md) {
  jdbcColumnMeta(md, "Type", "I")
}

j_to_r_type <- function (jtype) {
  mapping <- c("I" = "integer", "D" = "double", "J" = "integer",
    "F" = "double", "Z" = "logical", "C" = "integer", "B" = "raw",
    "S" = "character")
  assertthat::assert_that(assertthat::has_name(mapping, jtype))
  unname(mapping[jtype])
}

jdbcToSqlServerType <- function (jtype) {
  # http://jtds.sourceforge.net/typemap.html
  mapping <- c("TINYINT" = "tinyint", "SMALLINT" = "smallint",
    "INTEGER" = "int", "BIGINT" = "bigint", "DECIMAL" = "decimal",
    "NUMERIC" = "numeric", "REAL" = "real", "DOUBLE" = "float", "BIT" = "bit",
    "CHAR" = "char", "VARCHAR" = "varchar", "CLOB" = "text",
    "BINARY" = "binary", "VARBINARY" = "varbinary", "BLOB" = "image",
    "TIMESTAMP" = "datetime", "DATE" = "date", "TIME" = "time")
  assertthat::assert_that(assertthat::has_name(mapping, jtype))
  unname(mapping[jtype])
}

