% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokens.R
\name{ol_token_get_rate_limit}
\alias{ol_token_get_rate_limit}
\title{Get 'OneLogin' API rate limit}
\usage{
ol_token_get_rate_limit(con)
}
\arguments{
\item{con}{a 'OneLogin' connection}
}
\value{
A tibble of rate limit data
}
\description{
Get 'OneLogin' API rate limit
}
\examples{
if(interactive()) ol_token_get_rate_limit(onelogin())
}
