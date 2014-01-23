\name{output}
\alias{output}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Collects and saves output.
}
\description{
This function collects the network, changepoint and hyperparameter samples taken from the MCMC simulation, and saves them to a file if appropriate.
}
\usage{
output(counters, listStock, GLOBvar, HYPERvar, OUTvar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{counters}{List of counters for the number of moves that have been proposed and
  accepted.}
  \item{listStock}{Network, changepoint and hyperparameter samples.
}
  \item{GLOBvar}{Global variables of the MCMC simulation.
}
  \item{HYPERvar}{Hyperparameter variables.
}
  \item{OUTvar}{Output variables, including the output file.
}
}
\value{
Returns a list with an element for each target node which is also a list. Each sublist containts the elements:
\item{cp_samples }{Changepoint samples, a NumSamples by MaxNumChangePoints matrix.}
\item{edge_samples}{Network samples (with regression parameters), a NumSamples by (NumSegs * NumNodes) matrix.}
\item{target}{The target node for this subnetwork.}
\item{hyper_samples}{Information sharing prior hyperparameter samples, a NumSamples by NumHyperParams matrix.}
\item{sampled}{Sampled iterations.}
\item{counters}{Counters for the number of proposed and accepted moves.}
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

