% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users.R
\name{ol_user_delete}
\alias{ol_user_delete}
\title{Delete user}
\usage{
ol_user_delete(con, user_id)
}
\arguments{
\item{con}{a 'OneLogin' connection}

\item{user_id}{user id}
}
\description{
Delete user
}
\examples{
if (interactive()) ol_user_delete(onelogin(), 54963040)
}
