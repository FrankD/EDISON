\name{sampleDelta2}
\alias{sampleDelta2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample delta squared.
}
\description{
This function samples the signal-to-noise hyperparameter delta squared.
}
\usage{
sampleDelta2(pos, x, q, B, S, sig2, alphad2, betad2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{
The current segment.
}
  \item{x}{
Data,
}
  \item{q}{
Number of nodes.
}
  \item{B}{
Regression coefficients.
}
  \item{S}{
Network structure.
}
  \item{sig2}{
Sigma squared.
}
  \item{alphad2}{
Gamma prior hyperparameter.
}
  \item{betad2}{
Gamma prior hyperparameter.
}
}
\value{
New sample of delta squared.
}
\references{
For details of the sampling, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{
Sophie Lebre
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

