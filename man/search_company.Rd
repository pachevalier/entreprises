% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sirene.R
\name{search_company}
\alias{search_company}
\title{Search company}
\usage{
search_company(
  string,
  endpoint = "https://entreprise.data.gouv.fr/api/sirene/v1"
)
}
\arguments{
\item{string}{a company name as a string}

\item{endpoint}{API endpoint}
}
\value{
a list
}
\description{
search company by name
}
\examples{

search_company(string = "dataiku")

}
