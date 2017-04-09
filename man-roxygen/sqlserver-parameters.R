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
#'   Defaults to \code{1433} if set to \code{NULL} or an empty string.
#' @param database the name of the database hosted on the \code{server}. If an
#'   empty string or \code{NULL} (default), a connection to the default
#'   database on \code{server} is assumed.
#' @param properties One or more [optional connection
#'   properties](https://docs.microsoft.com/en-us/sql/connect/jdbc/setting-the-connection-properties).
#'   in a named list (defaults to empty list).
