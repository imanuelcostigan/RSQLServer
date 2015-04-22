# jTDS FAQ
# http://jtds.sourceforge.net/faq.html
# Accessed: 8 October 2014
# General form:
# jdbc:jtds:<server_type>://<server>[:<port>][/<database>][;<property>=<value>[;...]]
# where:
# <server_type> is one of either 'sqlserver' or 'sybase' (their meaning is
#    quite obvious),
# <port> is the port the database server is listening to (default is 1433 for
#    SQL Server and 7100 for Sybase)
# <database> is the database name -- JDBC term: catalog -- (if not specified,
# the user's default database is used).
# More on FAQ about available properties.

REQUIRED_JTDS_FIELDS <- c("server")
OPTIONAL_JTDS_FIELDS <-  c("port", "database", "appName", "autoCommit",
  "batchSize", "bindAddress", "bufferDir", "bufferMaxMemory", "bufferMinPackets",
  "cacheMetaData", "charset", "domain", "instance", "lastUpdateCount",
  "lobBuffer", "loginTimeout", "macAddress", "maxStatements", "namedPipe",
  "packetSize", "password", "prepareSQL", "progName", "processId",
  "sendStringParametersAsUnicode", "socketTimeout", "socketKeepAlive",
  "ssl", "tcpNoDelay", "TDS", "useCursors", "useJCIFS", "useLOBs",
  "useNTLMv2", "user", "wsid", "xaEmulation")

jtds_url <- function (server, type = "sqlserver", port = "", database = "", ...) {
  assertthat::assert_that(type %in% c("sqlserver", "sybase"))
  url <- paste0('jdbc:jtds:sqlserver://', server,
    paste0(':', port), paste0('/', database), ';')
  properties <- list(...)
  assertthat::assert_that(all(names(properties) %in% OPTIONAL_JTDS_FIELDS))
  paste0(url, paste0(names(properties), '=',
    unlist(properties, use.names = FALSE), collapse=';'))
}

get_server_details <- function (server, file = NULL) {
  if (is.null(file)) {
    file <- file.path(Sys.getenv("HOME"), "sql.yaml")
  }
  yaml::yaml.load_file(file)[[server]]
}

jdbc_class_path <- function () {
  file.path(system.file('java', package = 'RSQLServer'), 'jtds-1.3.1.jar')
}
