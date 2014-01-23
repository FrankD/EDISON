\name{rinvgamma}
\alias{rinvgamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Samples from the inverse gamma distribution.
}
\description{
This function samples from the inverse gamma distribution.
}
\usage{
rinvgamma(n, shape, scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of values to sample.
}
  \item{shape}{
Shape parameter.
}
  \item{scale}{
Scale parameter (1/rate).
}
}
\value{
Random sample from the inverse gamma distribution.
}
\author{
Frank Dondelinger
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dinvgamma}}
}
\examples{
# Draw samples from inverse gamma distribution with shape parameter 1 
# and scale parameter 1
samples = rinvgamma(100, shape=1, scale=1)

# Calculate density of samples
densities = dinvgamma(samples, shape=1, scale=1)
}
