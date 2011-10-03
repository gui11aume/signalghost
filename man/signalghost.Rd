\name{signalghost}
\alias{signalghost}
\title{
  Resamples a signal with the GHOST protocol.
}
\description{
  ...
}
\usage{
signalghost(x, blocks, minlvl, maxlvl)
}
\arguments{
  \item{x}{A \code{vector} or \code{matrix} with the signal to resample.}
  \item{blocks}{A \code{factor} or \code{vector} or indices indicating
    to which block belong the obervations.}
  \item{minlvl}{An integer indicating the minimum hierarchic level at
    which transpositions occur.}
  \item{maxlvl}{An integer indicating the maximum hierarchic level at
    which transpositions occur.}
}
\details{
  The GHOST protocol is described in detail elsewhere.
}
\value{
  A \code{vector} or \code{matrix} of the same dimensions as \code{x}.
}
\author{
Guillaume JP Filion and Wouter Meuleman.
}
