\name{main}
\alias{main}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Main function of the MCMC simulation.
}
\description{This function executes the main loop of the MCMC simulation, making the different moves and recording samples.
}
\usage{
main(X, Y, initiation, GLOBvar, HYPERvar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Input response data.
}
  \item{Y}{Input target data.
}
  \item{initiation}{Initialisation of the MCMC simulation, as obtained by function
  \code{\link{init}}.}
  \item{GLOBvar}{Global variables of the MCMC simulation.
}
  \item{HYPERvar}{Hyperparameter variables.
}
}
\value{
Returns a list with the following elements:
\item{counters}{List containing the different move counters for the number of times moves have been proposed and accepted.}
\item{listStock}{List containing the recorded samples for the networks, changepoints and hyperparameters}
}
\references{For more information about the MCMC simulations, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre

Frank Dondelinger
}


\seealso{
\code{\link{runDBN}}
}
