# Functions for invite endpoints

ol_gen_invite_link <- function(con, email) {
  con$POST("api/1/invites/get_invite_link", body = list(email = email))
}

ol_send_invite_link <- function(con, email) {
  con$POST("api/1/invites/send_invite_link", body = list(email = email))
}