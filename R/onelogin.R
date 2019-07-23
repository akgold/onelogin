#'
#' Class representing a Onelogin API client
#'
#' @name onelogin
#'
#' @section Usage:
#' \preformatted{
#' client <- onelogin$new(host = 'https://api.us.onelogin.com/',
#'   client_id = 'my_id',
#'   client_secret = 'my_secret')
#' }
#'
#' @section Details:
#'
#' This class allows a user to manipulate [onelogin](onelogin.com) via the
#' [onelogin API](https://developers.onelogin.com/).
#'
#' Authentication is done by providing a client ID and secret to get both an
#' auth and refresh token.
#'
#' @export
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

    initialize = function(host, client_id, client_secret) {
      message(glue::glue("Defining onelogin API connection {host}"))
      self$host = host
      self$client_id = safer::encrypt_string(client_id)
      self$client_secret = safer::encrypt_string(client_secret)
    },

    make_auth = function(type = "token") {
      switch(
        type,
        token = glue::glue("bearer:{self$access_token}"),
        generate_token = glue::glue("client_id:{safer::decrypt_string(self$client_id)}, client_secret:{safer::decrypt_string(self$client_secret)}"))
    },

    raise_error = function(res) {
      if (httr::http_error(res)) {
        err <- sprintf('\n\n%s request failed with %s\n',
                       res$request$url,
                       httr::http_status(res)$message)
        message(err)
        res
      }
    },

    generate_token = function() {
      res <- self$POST("/auth/oauth2/v2/token",
                       auth_type = "generate_token",
                       body = list(grant_type = "client_credentials"),
                       res_to_df = FALSE)

      self$access_token = res$access_token
      self$refresh_token = res$refresh_token
      self
    },

    get_refresh_token = function() {
      res <- self$POST("/auth/oauth2/v2/token",
                auth_type = "generate_token",
                body = list(grant_type = "refresh_token",
                            access_token = self$access_token,
                            refresh_token = self$refresh_token),
                res_to_df = FALSE)

      self$access_token = res$access_token
      self$refresh_token = res$refresh_token
      self
    },

    revoke_token = function() {
      con$POST("auth/oauth2/revoke",
               body = list(access_token = self$access_token),
               auth_type = "generate_token",
               res_to_df = FALSE)
    },

    GET = function(path, writer = httr::write_memory(), parser = 'parsed',
                   res_to_df = TRUE,
                   ...) {
      req <- file.path(self$host, path)
      res <- httr::GET(req,
                       httr::add_headers(Authorization = self$make_auth()),
                       writer,
                       query = list(...)) %>%
        self$parse_res(res_to_df = res_to_df)
    },

    PUT = function(path, body, encode = 'json', res_to_df = TRUE) {
      req <- paste0(self$host, path)
      res <- httr::PUT(
        req,
        httr::add_headers(Authorization = self$make_auth()),
        body = body,
        encode = encode
      )
      self$raise_error(res)
      res <- httr::content(res, as = 'parsed')

      if (res_to_df) {
        res <- self$res_to_df(res)
      }
      res
    },

    POST = function(path, body, encode = 'json', res_to_df = TRUE,
                    auth_type = "token") {
      req <- paste0(self$host, path)
      res <- httr::POST(
        req,
        httr::add_headers(Authorization = self$make_auth(auth_type)),
        body = body,
        encode = encode)
      self$raise_error(res)
      res <- httr::content(res, as = 'parsed')

      if (res_to_df) {
        res <- self$res_to_df(res)
      }
      res
    },

    DELETE = function(path, body = NULL, encode = 'json', res_to_df = TRUE) {
      req <- paste0(self$host, path)
      res <- httr::DELETE(req,
                          httr::add_headers(Authorization = self$make_auth()),
                          body = body,
                          encode = encode)
      self$raise_error(res)
      res <- httr::content(res, as = 'parsed')

      if (res_to_df) {
        res <- self$res_to_df(res)
      }
      res
    },

    parse_res = function(res, res_to_df) {
      self$raise_error(res)
      res <- httr::content(res, as = 'parsed')

      # Responses come down in pages of 50 -- recurse to get all pages
      dat <- NULL
      if (!is.null(res$pagination$next_link)) {
        # Need to remove host from next link or get twice is self$GET
        req <- sub(con$host, "", res$pagination$next_link)
        dat <- self$GET(req)
      }

      if (res_to_df) {
        dat <- bind_rows(dat, self$res_to_df(res))
      }

      dat
    },

    res_to_df = function(res) {
      data <- res$data

      if (all(purrr::map_int(data, length) == 1)) {
        return(dplyr::as_tibble(data))
      }

      data %>%
        purrr::map_df(~ purrr::map_if(., is.null, function(x) NA) %>%
                        dplyr::as_tibble())
    }
  )
)

onelogin <- function(host = 'https://api.us.onelogin.com/',
                     client_id = Sys.getenv("ONELOGIN_CLIENT_ID"),
                     client_secret = Sys.getenv("ONELOGIN_CLIENT_SECRET")) {
  con <- ONELOGIN$new(host = host,
                      client_id = client_id, client_secret = client_secret)
  # Try to get access token
  tryCatch({
    con <- con$generate_token()
  },
  error = function(e){
    message(glue::glue("Trouble getting access token."))
    stop(e)
  })
  con
}



