#' @param server the server address or recognised alias thereof.
#' @param file defaults to using the server details file in
#'   \code{$HOME/sql.yaml}. The server details including \code{type},
#'   \code{port} and any optional \code{properties} can be sourced from this
#'   file. If the \code{server} name is found in \code{file}, the details
#'   therein are used (and in particular, those provided in other arguments to
#'   this function are ignored). The connection method prefers server details
#'   to be provided in a \code{"sql.yaml"} file rather than provided as
#'   arguments to this function. If you wish to specify the details as
#'   parameters, ensure that either the \code{file} does not exist or that the
#'   \code{server} details are not in the YAML file.
#' @param type the server type. Must be either \code{"sqlserver"} or
#'   \code{"sybase"}. Defaults to \code{"sqlserver"} when set to \code{NULL}.
#' @param port the TCP/IP default port. This will be coerced to a string.
#'   Defaults to \code{1433} if set to \code{NULL} or an empty string (jTDS
#'   behaviour).
#' @param database the name of the database hosted on the \code{server}. If an
#'   empty string or \code{NULL} (default), a connection to the default
#'   database on \code{server} is assumed.
#' @param properties One or more
#'   \href{http://jtds.sourceforge.net/faq.html}{optional connection
#'   properties.} in a named list (defaults to empty list). Note if you intend
#'   to set the \code{useNTLMv2} property to \code{'true'} from the default API
#'   value of \code{'false'}, you will need to make a specific authentication
#'   driver available to the SQL Server driver, although this has not worked
#'   particularly well in testing. See \code{\link{RSQLServer}} for more
#'   details. Should you wish to use Windows authentication to connect to the
#'   server, I recommend you set the following optional parameters: set
#'   \code{useNTLMv2} to \code{'true'}, \code{domain} to your domain and
#'   \code{user} and \code{password} to your username and password on
#'   \code{domain}. jTDS' SSO functionality is flaky.
