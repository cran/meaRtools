\name{calculate_spike_features}
\alias{calculate_spike_features}

\title{
Filter spikes and bursts in recording objects
}
\description{
Apply user defined filters on the spikes that were recorded and calculate spike features.
}
\usage{
calculate_spike_features(r_object_files,  parameters)
}

\arguments{
  \item{r_object_files}{ A list of recording Robject files}
  \item{parameters}{ A list of parameters, see data("parameters") }
}

\value{
Returns an 's' object containing all the spikes and bursts of all the loaded recording Robjects.
}
\author{
Diana Hall
}

\keyword{ spikes }
\keyword{ filter }
