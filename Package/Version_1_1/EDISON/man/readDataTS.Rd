\name{readDataTS}
\alias{readDataTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read target data.
}
\description{
This function reads in the target data.
}
\usage{
readDataTS(data, posI, t0, tf, m, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Input data matrix to read.
}
  \item{posI}{
Position of interest.
}
  \item{t0}{
First timepoint.
}
  \item{tf}{
Last timepoint.
}
  \item{m}{
Number of repetitions.
}
  \item{n}{
Number of timepoints.
}
}
\value{
Returns the target data.
}
\author{
Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{buildXY}}
}
