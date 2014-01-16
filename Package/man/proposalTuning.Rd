\name{proposalTuning}
\alias{proposalTuning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tune the proposal width for betas.
}
\description{
This function adjusts the proposal width for the beta hyperparameter(s) of the exponential information sharing prior, so that the acceptance rate is close to 0.25.
}
\usage{
proposalTuning(acceptRate, hyper.proposals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acceptRate}{
Current acceptance rate.
}
  \item{hyper.proposals}{
Current proposal width.
}
}
\value{
Returns the new proposal width.
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

