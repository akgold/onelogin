# Provide wrappers around User resources

###### GET

#' Get Users from OneLogin
#'
#' You can filter the user by various parameters in onelogin. See the
#' [page in the API docs](https://developers.onelogin.com/api-docs/1/users/get-users) for filter options.
#'
#' @inheritParams ol_token_get
#' @param ... filter parameters, optional; see API documentation
#'
#' @return a tibble of users and their attributes
#' @export
ol_users_get <- function(con,  ...) {
  con$GET("api/1/users", ...)
}

#' Get a User by their ID
#'
#' @inheritParams ol_token_get
#' @param id user id
#'
#' @return a tibble of user data
#' @export
ol_user_get_by_id <- function(con, id) {
  con$GET(glue::glue("api/1/users/{id}"))
}

#' Title
#'
#' @inheritParams ol_token_get
#'
#' @return a tibble of user data
#' @export
ol_user_get_apps <- function(con, id) {
  con$GET(glue::glue("api/1/users/{id}/apps"))
}

ol_user_get_roles <- function(con, id) {
  res <- con$GET(glue::glue("api/1/users/{id}/roles"), res_to_df = FALSE)

  data.frame(roles = unlist(res$data))
}

ol_user_get_custom_fields <- function(con) {
  con$GET(glue::glue("api/1/users/custom_attributes"))
}

##### POST

ol_user_create <- function(con, firstname, lastname, email, username, ...) {
  extra_args <- list(...)

  con$POST("api/1/users",
           body = c(list(firstname = firstname,
                         lastname = lastname,
                         email = email,
                         username = username),
                    list(...)))
}

###### PUT

#' Update User Information by ID
#'
#' @inheritParams ol_user_get_by_id
#' @param ... named parameters to change in request
#'
#' @return tibble of user data
#' @export
ol_user_update <- function(con, id, ...) {
  con$PUT(glue::glue("api/1/users/{id}"), body = list(...))
}

ol_user_assign_role <- function(con, id, role_id_array) {
  stopifnot(is.numeric(role_id_array))

  con$PUT(glue::glue("api/1/users/{id}/add_roles"),
                     body = list(role_id_array =
                                   jsonlite::toJSON(role_id_array, auto_unbox = TRUE)))
}

ol_user_remove_role <- function(con, id, role_id_array) {
  stopifnot(is.numeric(role_id_array))

  con$PUT(glue::glue("api/1/users/{id}/remove_roles"),
                     body = list(role_id_array = role_id_array))
}

ol_user_pwd_cleartext <- function(con, id,
                                  password, password_confirmation,
                                  validate_policy = FALSE) {

  con$PUT(glue::glue("api/1/users/set_password_clear_text/{id}"),
          body = list(password = password,
                      password_confirmation = password_confirmation,
                      validate_policy = validate_policy))

}

ol_user_pwd_sha256_salt <- function(con, id,
                                  password,
                                  password_confirmation,
                                  password_algorithm = "salt+sha256",
                                  password_salt = "") {

  con$PUT(glue::glue("api/1/users/set_password_using_salt/{id}"),
          body = list(password = password,
                      password_confirmation = password_confirmation,
                      password_algorithm = password_algorithm,
                      password_salt = password_salt))
}

ol_user_set_custom_attr <- function(con, id, ...) {
  con$PUT(glue::glue("/api/1/users/{id}/set_custom_attributes"),
          body = list(custom_attributes = list(...)))
}

ol_user_set_state <- function(con, id, state) {
  stopifnot(as.integer(state) %in% 0L:3L)

  con$PUT(glue::glue("api/1/users/{id}/set_state"),
          body = list(state = state))
}

ol_user_log_out <- function(con, id) {
  con$PUT(glue::glue("api/1/users/{id}/logout"))
}

ol_user_lock_account <- function(con, id) {
 con$PUT(glue::glue("api/1/users/{id}/lock_user"))
}

ol_user_delete <- function(con, id) {
  con$DELETE(glue::glue("api/1/users/{id}"))
}
