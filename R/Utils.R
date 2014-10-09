build_url <- function (server, ...)
{
  # jTDS FAQ
  # http://jtds.sourceforge.net/faq.html
  # Accessed: 8 Oct 2014
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
  opts <- list(...)
  if (is.null(opts$port))
    port <- ''
  else {
    port <- paste0(':', opts$port)
    opts$port <- NULL
  }
  if (is.null(opts$database))
    database <- ''
  else {
    database <- paste0('/', opts$database)
    opts$database <- NULL
  }
  url <- paste0('jdbc:jtds:sqlserver://', server, port, database, ';')
  paste0(url, paste0(names(opts), '=',
    unlist(opts, use.names = FALSE), collapse=';'))
}

