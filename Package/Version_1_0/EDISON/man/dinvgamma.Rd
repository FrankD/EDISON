\name{dinvgamma}
\alias{dinvgamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate inverse gamma distribution.
}
\description{
This function calculates the density of the inverse gamma distribution.
}
\usage{
dinvgamma(x, shape, scale = 1, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Input.
}
  \item{shape}{
Shape parameter.
}
  \item{scale}{
Scale parameter (1/rate).
}
  \item{log}{
Whether to return the log density. 
}
}
\value{
Returns the density (or log density).
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# Draw samples from inverse gamma distribution with shape parameter 1 
# and scale parameter 1
samples = rinvgamma(100, shape=1, scale=1)

# Calculate density of samples
densities = dinvgamma(samples, shape=1, scale=1)
}
