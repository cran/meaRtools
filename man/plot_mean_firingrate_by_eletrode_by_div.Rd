\name{plot_mean_firingrate_by_eletrode_by_div}
\alias{plot_mean_firingrate_by_eletrode_by_div}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.mean.firingrate.by.eletrode.by.div
}
\description{
Displays average firing rate by well and by electrode for each DIV available.   
}
\usage{
plot_mean_firingrate_by_eletrode_by_div(s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
's' object. must be a list, with each DIV a different entry.
}
}
\examples{

data("S") 
s<-list()
s[[1]]<-S
\donttest{
  plot_mean_firingrate_by_eletrode_by_div(s)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ firing }
\keyword{ rate }

