# ################################################################################
# ## Class: DBIConnection
# ################################################################################

# # ##=== SQLServerResult
# # ## jr - result set, md - metadata, stat - statement
# # ## Since the life of a result set depends on the life of the statement, we have to explicitly
# # ## save the later as well (and close both at the end)
# #
# # setClass("SQLServerResult",
# #   representation("JDBCResult", jr = "jobjRef", md = "jobjRef", stat = "jobjRef",
# #     pull = "jobjRef")
# # )
# #
# # setMethod("dbHasCompleted", "SQLServerResult",
# #   def = function(res, ...) TRUE,
# #   valueClass = "logical"
# # )
# #
# # .verify.JDBC.result <- function (result, ...) {
# #   if (is.jnull(result))
# #   {
# #     x <- .jgetEx(TRUE)
# #     if (is.jnull(x))
# #       stop(...)
# #     else
# #       stop(..., " (", .jcall(x, "S", "getMessage"), ")")
# #   }
# # }
#
