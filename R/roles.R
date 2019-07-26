# Wrappers for roles API

ol_roles_get <- function(con, ...) {
  con$GET("api/1/roles", ...)
}

ol_role_get_by_id <- function(con, role_id) {
 con$GET(glue::glue("api/1/roles/{role_id}"))
}