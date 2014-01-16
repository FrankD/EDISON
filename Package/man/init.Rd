\name{init}
\alias{init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Initialise the MCMC simulation.
}
\description{
This function intialises the parameters and variables needed for the MCMC simulation.
}
\usage{
init(X, Y, sinit, GLOBvar, HYPERvar, options)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Input response data.
}
  \item{Y}{
Input target data.
}
  \item{sinit}{
Initial changepoints.
}
  \item{GLOBvar}{
Global variables used during the MCMC simulation.
}
  \item{HYPERvar}{
Hyperparameter variables.
}
  \item{options}{
MCMC simulation options as obtained e.g. by \code{\link{defaultOptions}}.
}
}
\value{
List with elements:
\item{counters}{Matrices for counting the number of moves made and accepted.}
\item{initState}{Initial state of the variables of the MCMC simulation.}
\item{listStock}{Variables for recording the network, changepoint and hyperparameter samples.}
}
\author{
Sophie Lebre

Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sampleParms}}
}
