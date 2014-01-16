\name{HyperParms}
\alias{HyperParms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sets up initial values of hyperparameters.
}
\description{
This function initialises the variable HYPERvar with values for the various hyperparameters in the model. 
}
\usage{
HyperParms(options)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{options}{
MCMC settings, possibly from \code{\link{defaultOptions}}.
}
}
\value{
Settings for the HYPERvar variable:
\item{cD}{Proportion of changepoint moves proposed.}
\item{alphaD}{Prior settings for the number of changepoints.}
\item{betaD}{Prior settings for the number of changepoints.}
\item{c}{Ratio of changepoint birth/death moves proposed.}
\item{v0}{Prior settings for the sigma squared parameters.}
\item{gamma0}{Prior settings for the sigma squared parameters.}
\item{alphad2}{Prior settings for the signal-to-noise ratio delta squared.}
\item{betad2}{Prior settings for the signal-to-noise ratio delta squared.}
\item{alphalbd}{Prior settings for the number of transcription factors.}
\item{betalbd}{Prior settings for the number of transcription factors.}
}
\author{
Sophie Lebre

Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
