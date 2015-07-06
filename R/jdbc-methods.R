jdbcColumnMeta <- function (md, meta, jtype) {
  n <- rJava::.jcall(md, "I", "getColumnCount")
  rtype <- j_to_r_type(jtype)
  res <- vector(rtype, length = n)
  for (i in 1:n) {
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
  mapping <- c("S" = "character", "I" = "integer", "D" = "double",
    "J" = "integer", "F" = "double", "Z" = "logical", "C" = "integer",
    "B" = "raw")
  assertthat::assert_that(assertthat::has_name(mapping, jtype))
  unname(mapping[jtype])
}

r_to_j_type <- function (rtype) {
  mapping <- c("S" = "character", "I" = "integer", "D" = "double",
    "J" = "integer", "F" = "double", "Z" = "logical", "C" = "integer",
    "B" = "raw")
  # Default return value is "S" (string)
  names(mapping)[match(rtype, mapping, 1)]
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

jdbcToRType <- function (type) {
  # http://docs.oracle.com/javase/7/docs/api/constant-values.html#java.sql.Types
  # BIGINT (-5) is -2^63 to 2^63-1 which corresponds to Java's long. However,
  # R does not have an integer type correspond to Java's long and rJava
  # uses numeric / double instead. See footnote: http://www.rforge.net/rJava/
  jnumeric <- c(-5, 2, 3, 6, 7, 8)
  names(jnumeric) <- rep_len("numeric", length(jnumeric))
  jinteger <- c(-6, 4, 5)
  names(jinteger) <- rep_len("integer", length(jinteger))
  jlogical <- c(-7, 16)
  names(jlogical) <- rep_len("logical", length(jlogical))
  jdate <- 91
  names(jdate) <- rep_len("Date", length(jdate))
  jdatetime <- 93
  names(jdatetime) <- rep_len("POSIXct", length(jdatetime))
  jraw <- c(-4, -3, -2)
  names(jraw) <- rep_len("raw", length(jraw))
  jother <- 1 # CHAR
  names(jother) <- "character"
  mapping <- c(jother, jnumeric, jinteger, jlogical, jdate, jdatetime, jraw)
  # If no match, mapping to 1 = CHAR
  res <- names(mapping)[match(type, mapping, 1)]
  if (length(res)) {
    return(res)
  } else {
    return(rep_len("character", length(type)))
  }
}

rToJdbcType <- function (type) {
  # JDBC API:
  # http://docs.oracle.com/javase/7/docs/api/constant-values.html#java.sql.Types
  mapping <- c("character" = 12, "numeric" = 8, "integer" = 4, "logical" = 16,
    "Date" = 91, "POSIXct" = 93, "raw" = -3)
  # Default map of R type is to "character" unless specific mapping is available
  mapping[match(type, names(mapping), 1)]
}

jdbcGetter <- function (type) {
  mapping <- c("Double" = "numeric", "Int" = "integer", "Boolean" = "logical",
    "Date" = "Date", "Date" = "POSIXct", "Byte" = "raw", "String" = "character")
  rtype <- jdbcToRType(type)
  paste0("get", names(mapping)[match(rtype, mapping, 0)])
}
