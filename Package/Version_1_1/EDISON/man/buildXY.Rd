\name{buildXY}
\alias{buildXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds response Y and predictor X.
}
\description{This function builds the response variables Y and predictor variables X from the input
data.
}
\usage{
buildXY(targetData, predData, GLOBvar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{targetData}{Target input data.
}
  \item{predData}{Predictor input data.
}
  \item{GLOBvar}{Global variables of the MCMC simulation.
}
}
\value{
A list with elements:
\item{X}{Predictor variables.}
\item{Y}{Response variables.}
}
\author{Sophie Lebre
}