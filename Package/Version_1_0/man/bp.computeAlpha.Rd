\name{bp.computeAlpha}
\alias{bp.computeAlpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the acceptance ratio of two changepoint configurations.
}
\description{
This function computes the acceptance ratio of two changepoint configurations with 
networks in a changepoint birth or death move.
}
\usage{
bp.computeAlpha(birth, lNew, kminus, Ekl, Estar, Ekr, yL, 

PxL, yR, PxR, y2, Px2, D, delta2, q, smax, v0, gamma0, 

prior_ratio = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{birth}{\code{1} for a changepoint birth move, \code{-1} for a changepoint death move.
}
  \item{lNew}{Number of edges in the new segment.
}
  \item{kminus}{ Minimal number of changepoints between the two compared models (equal to \code{s}
for a birth move, \code{s-1} for a death move.
}
  \item{Ekl}{Changepoint on the left of proposed changepoint.
}
  \item{Estar}{Changepoint being inserted or deleted.
}
  \item{Ekr}{Changepoint on the right of proposed changepoint.
}
  \item{yL}{Response data (left).
}
  \item{PxL}{Projection matrix (left).
}
  \item{yR}{Response data (right).
}
  \item{PxR}{Projection matrix (right).
}
  \item{y2}{Response data (both).
}
  \item{Px2}{Projection matrix (both).
}
  \item{D}{Hyperparameters for the number of edges in each segment.
}
  \item{delta2}{Hyperparameters for the empirical covariance (signal-to-noise ratio).
}
  \item{q}{Total number of nodes in the network.
}
  \item{smax}{Maximum number of changepoints.
}
  \item{v0}{Hyperparameter.
}
  \item{gamma0}{Hyperparameter.
}
  \item{prior_ratio}{Ratio of network structure priors.
}
}
\references{
For more information about the model, see:

Dondelinger et al. (2012), "Non-homogeneous dynamic Bayesian networks with 
Bayesian regularization for inferring gene regulatory networks with gradually 
time-varying structure", Machine Learning.
}
\author{Sophie Lebre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cp.birth}}, \code{\link{cp.death}}
}
