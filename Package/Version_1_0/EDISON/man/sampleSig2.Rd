\name{sampleSig2}
\alias{sampleSig2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample initial sigma squared.
}
\description{
This function samples the initial values for the sigma squared variance from the inverse gamma prior.
}
\usage{
sampleSig2(y, Px, v0, gamma0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Input data.
}
  \item{Px}{
Projection matrix.
}
  \item{v0}{
Inverse gamma prior hyperparameter.
}
  \item{gamma0}{
Inverse gamma prior hyperparameter.
}
}
\value{
The sampled sigma squared values.
}
\references{
For more information about the model, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

