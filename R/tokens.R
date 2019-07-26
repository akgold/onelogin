# Manage API Access OAuth Tokens

#' Generate a OneLogin token
#'
#' @param con a onelogin object
#'
#' @return a onelogin connection
#' @export
#'
#' @examples
#' if(interactive()) ol_token_get
ol_token_get <- function(con) {
  con$generate_token()
}
