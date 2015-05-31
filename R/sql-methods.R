sqlServerToRType <- function (type) {
  # https://msdn.microsoft.com/en-us/library/ms187752(v=sql.90).aspx
  mapping <- c("bigint" = "integer", "int" = "integer", "smallint" = "integer",
    "tinyint" = "integer", "bit" = "logical", "decimal" = "double",
    "numeric" = "double", "money" = "double", "smallmoney" = "double",
    "float" = "double", "real" = "double", "datetime" = "POSIXct",
    "smalldatetime" = "POSIXct", "char" = "character", "varchar" = "character",
    "text" = "character", "nchar" = "character", "nvarchar" = "character",
    "ntext" = "character", "binary" = "raw", "varbinary" = "raw",
    "image" = "raw")
  assertthat::assert_that(assertthat::has_name(mapping, type))
  unname(mapping[type])
}
