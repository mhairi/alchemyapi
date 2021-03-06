% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_keywords}
\alias{get_keywords}
\title{Get keywords from Alchemy API}
\usage{
get_keywords(url, api_key)
}
\arguments{
\item{url}{URL to extract the keywords from}

\item{api_key}{}
}
\value{
Dataframe with a row for each word, the corrisponding score
and the origional url
}
\description{
Get keywords from Alchemy API
}
\examples{
\dontrun{
get_keywords('google.com', '1234')
}
}

