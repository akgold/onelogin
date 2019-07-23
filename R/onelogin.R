#' Class representing a Onelogin API client
ONELOGIN <- R6::R6Class(
  'ONELOGIN',

  public = list(
    host = NULL,
    client_id = NULL,
    client_secret = NULL,
    access_token = NULL,
    refresh_token = NULL,
    token_expire = NULL,

    get_onelogin = function() {
      self
      },

    get_host = function(region) {
      glue::glue("https://api.{region}.onelogin.com/api")
    },

    initialize = function(region, client_id, client_secret) {
      region <- tolower(region)
      stopifnot(region %in% c("us", "eu"))

      self$host = self$get_host(region)
      glue::glue("Defining onelogin API connection {self$host}")
      self$client_id = safer::encrypt_string(client_id)
      self$client_secret = safer::encrypt_string(client_secret)
    }
  )
)


