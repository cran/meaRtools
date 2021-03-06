% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment.R
\name{add_plateinfo}
\alias{add_plateinfo}
\title{Provide information about a new MEA plate format.}
\usage{
add_plateinfo(arrayname, info)
}
\arguments{
\item{arrayname}{Name of the plate}

\item{info}{List of information to store about this plate}
}
\value{
Nothing.
}
\description{
Provide information about a new MEA plate format
}
\details{
Store information about a new platename called arrayname.
If arrayname already has been used, overwrite it with this
new information.
}
\author{
Stephen Eglen
}
