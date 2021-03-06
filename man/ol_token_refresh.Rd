% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokens.R
\name{ol_token_refresh}
\alias{ol_token_refresh}
\title{Refresh 'OneLogin' auth token}
\usage{
ol_token_refresh(con)
}
\arguments{
\item{con}{a 'OneLogin' connection}
}
\value{
A 'OneLogin' connection with refreshed auth token
}
\description{
Refresh 'OneLogin' auth token
}
\examples{
if(interactive()) ol_token_refresh(onelogin())
}
