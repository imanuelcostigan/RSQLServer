start_driver <- function() {
  rJava::.jaddClassPath(msft_class_path())
  rJava::.jnew("com.microsoft.sqlserver.jdbc.SQLServerDriver")
}

driver_version <- function(driver, check = TRUE) {
  major <- rJava::.jcall(driver@jdrv, "I", "getMajorVersion", check = check)
  minor <- rJava::.jcall(driver@jdrv, "I", "getMinorVersion", check = check)
  numeric_version(paste0(major, ".", minor))
}

new_connection <- function(driver, url, properties = NULL, check = TRUE) {
  properties <- properties %||%  rJava::.jnew('java/util/Properties')
  rJava::.jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url,
    properties, check = check)
}

close_connection <- function (conn, check = TRUE) {
  rJava::.jcall(conn@jc, "V", "close", check = check)
}

connection_info <- function (conn, info) {
  switch(info,
    username = rJava::.jfield(conn@jc, "S", "user"),
    host = rJava::.jfield(conn@jc, "S", "serverName"),
    port = rJava::.jfield(conn@jc, "I", "portNumber"),
    dbname = rJava::.jfield(conn@jc, "S", "currentDatabase"),
    db.version = numeric_version(rJava::.jfield(conn@jc, "S",
      "databaseProductVersion")))
}

create_statement <- function(conn, check = FALSE) {
  rJava::.jcall(conn@jc, "Ljava/sql/Statement;", "createStatement", check = check)
}

create_prepared_statement <- function(conn, statement, check = FALSE) {
  rJava::.jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement",
    statement, check = check)
}

close_statement <- function(statement, check = TRUE) {
  rJava::.jcall(statement, "V", "close", check = check)
}

execute_query <- function(statement, string = NULL, check = FALSE) {
  if (is.null(string)) {
    # A prepared statement
    rJava::.jcall(statement, "Ljava/sql/ResultSet;", "executeQuery",
      check = check)
  } else {
    rJava::.jcall(statement, "Ljava/sql/ResultSet;", "executeQuery",
      string, check = check)
  }
}

execute_update <- function(statement, string = NULL, check = FALSE) {
  if (is.null(string)) {
    rJava::.jcall(statement, "I", "executeUpdate", check = check)
  } else {
    rJava::.jcall(statement, "I", "executeUpdate", string, check = check)
  }
}

execute_batch <- function(statement, check = FALSE) {
  rJava::.jcall(statement, "[I", "executeBatch", check = check)
}

rs_metadata <- function(res, check = FALSE) {
  rJava::.jcall(res, "Ljava/sql/ResultSetMetaData;", "getMetaData",
    check = check)
}

catch_exception <- function (object, ...) {
  # Based on RJDBC .verify.JDBC.result()
  # https://github.com/s-u/RJDBC/blob/1b7ccd4677ea49a93d909d476acf34330275b9ad/R/class.R#L18
  if (rJava::is.jnull(object)) {
    x <- rJava::.jgetEx(TRUE)
    if (!rJava::is.jnull(x))
      stop(..., ": ", rJava::.jcall(x, "S", "getMessage"), call. = FALSE)
  }
}

is_autocommit <- function(conn, check = FALSE) {
  rJava::.jcall(conn@jc, "Z", "getAutoCommit", check = check)
}


# Fetch helpers ----------------------------------------------------------

ff <- function (res, empty_vector) {
  # Where rp_* return null because for example, you have requested to fetch
  # n = 0 records, DBItest expects an empty data frame will all the right
  # column names and types. This function ensures this is the case, and
  if (rJava::is.jnull(res)) {
    rJava::.jclear()
    empty_vector
  } else {
    res
  }
}

rp_getDoubles <- function (rp, i) {
  res <- rJava::.jcall(rp, "[D", "getDoubles", i, check = FALSE)
  ff(res, double())
}

rp_getInts <- function (rp, i) {
  res <- rJava::.jcall(rp, "[I", "getInts", i, check = FALSE)
  ff(res, integer())
}
rp_getDates <- function (rp, i) {
  res <- rJava::.jcall(rp, "[Ljava/lang/String;", "getStrings", i, check = FALSE)
  ff(res, character())
}
rp_getTimestamps <- function (rp, i) {
  res <- rJava::.jcall(rp, "[Ljava/lang/String;", "getStrings", i, check = FALSE)
  ff(res, character())
}
rp_getStrings <- function (rp, i) {
  res <- rJava::.jcall(rp, "[Ljava/lang/String;", "getStrings", i, check = FALSE)
  ff(res, character())
}

fetch_rp <- function (rp, out, cts = NULL) {
  cts <- cts %||% rJava::.jcall(rp, "[I", "mapColumns")
  for (i in seq_along(cts)) {
    new_res <- switch(as.character(cts[i]),
      "1" = rp_getDoubles(rp, i),
      "2" = rp_getInts(rp, i),
      "3" = rp_getDates(rp, i),
      "4" = rp_getTimestamps(rp, i),
      rp_getStrings(rp, i))
    out[[i]] <- c(out[[i]], new_res)
  }
  out
}

rp_to_r_type_map <- function (ctypes) {
  rp_to_r <- purrr::set_names(0:5,
    c("character", "numeric", "integer", "character", "character", "logical"))
  assertthat::assert_that(all(ctypes %in% rp_to_r))
  names(rp_to_r)[match(ctypes, rp_to_r)]
}

create_empty_lst <- function (types, names, n = 0L) {
  assertthat::assert_that(length(types) == length(names),
    n == 0L || assertthat::is.count(n))
  purrr::map(types, vector, length = n) %>%
    purrr::set_names(names)
}

# Bindings ----------------------------------------------------------------

rs_bind_all <- function(params, rs, batch = TRUE) {
  ps_bind_all(params, rs@stat, batch = batch)
}

ps_bind_all <- function(params, ps, batch = TRUE) {
  nparams <- length(params)
  if (is.null(params) || nparams == 0L) return()
  qry_nparams <- num_parameters(ps)
  if (qry_nparams == 0L) return()
  paramlengths <- vapply(params, length, integer(1L))
  if (!batch && any(paramlengths > 1L)) {
    warning("'batch' disabled with multi-row params, only first of each param applied",
      call. = FALSE)
  }

  is_na <- lapply(params, is.na)
  is_logical <- vapply(params, is.logical, logical(1))
  is_integer <- vapply(params, is.integer, logical(1))
  is_numeric <- vapply(params, is.numeric, logical(1))
  is_date <- vapply(params, inherits, logical(1), "Date")
  is_posix <- vapply(params, inherits, logical(1), "POSIXct")
  is_other <- !(is_logical | is_numeric | is_date | is_posix)

  jtypes <- rToJdbcType(vapply(params, function(a) class(a)[1], character(1)))

  for (j in seq_len(max(1L, batch * max(paramlengths)))) {
    for (i in seq_len(min(nparams, qry_nparams))) {
      if (is_na[[i]][[j]]) {
        ps_bind_null(i, NULL, ps, jtype = as.integer(jtypes[i]))
      } else if (is_integer[i]) {
        ps_bind_int(i, params[[i]][[j]], ps)
      } else if (is_numeric[i]) {
        ps_bind_dbl(i, params[[i]][[j]], ps)
      } else if (is_logical[i]) {
        ps_bind_bln(i, params[[i]][[j]], ps)
      } else if (is_other[i]) {
        ps_bind_str(i, params[[i]][[j]], ps)
      } else if (is_posix[i]) {
        ps_bind_tme(i, params[[i]][[j]], ps)
      } else if (is_date[i]) {
        ps_bind_dte(i, params[[i]][[j]], ps)
      }
    }
    if (batch) rJava::.jcall(ps, "V", "addBatch")
  }
}

ps_bind_null <- function(i, param, ps, jtype = NULL) {
  if (is.null(jtype)) jtype <- as.integer(rToJdbcType(class(param)))
  rJava::.jcall(ps, "V", "setNull", i, jtype)
}

ps_bind_int <- function(i, param, ps) {
  rJava::.jcall(ps, "V", "setInt", i, param)
}

ps_bind_dbl <- function(i, param, ps) {
  rJava::.jcall(ps, "V", "setDouble", i, param)
}

ps_bind_bln <- function(i, param, ps) {
  rJava::.jcall(ps, "V", "setBoolean", i, param)
}

ps_bind_dte <- function(i, param, ps) {
  # as.POSIXlt sets time to midnight UTC whereas as.POSIXct sets time to
  # local timezone. The tz argument is ignored when a Date is passed to
  # either function
  ms <- as.numeric(as.POSIXlt(param)) * 1000
  rJava::.jcall(ps, "V", "setDate", i,
    rJava::.jnew("java/sql/Date", rJava::.jlong(ms)))
}

ps_bind_tme <- function(i, param, ps) {
  # as.integer converts POSIXct to seconds since epoch. Timestamp
  # constructor needs milliseconds so multiply by 1000
  # http://docs.oracle.com/javase/7/docs/api/java/sql/Timestamp.html
  ms <- as.numeric(param) * 1000
  rJava::.jcall(ps, "V", "setTimestamp", i,
    rJava::.jnew("java/sql/Timestamp", rJava::.jlong(ms)))
}

ps_bind_raw <- function(i, param, ps) {
  rJava::.jcall(ps, "V", "setByte", i, rJava::.jbyte(as.raw(param)))
}

ps_bind_str <- function(i, param, ps) {
  rJava::.jcall(ps, "V", "setString", i, as.character(param))
}

is_parameterised <- function(ps) {
  num_parameters(ps) > 0
}

num_parameters <- function(ps) {
  md <- rJava::.jcall(ps, "Ljava/sql/ParameterMetaData;", "getParameterMetaData")
  rJava::.jcall(md, "I", "getParameterCount")
}
