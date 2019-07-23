# Manage API Access OAuth Tokens

ol_token_generate <- function(con) {
  con$generate_token()
}

ol_token_refresh <- function(con) {
  con$get_refresh_token()
}

ol_token_revoke <- function(con) {
  con$revoke_token()
}

ol_token_get_rate_limit <- function(con) {
  con$GET("auth/rate_limit")
}