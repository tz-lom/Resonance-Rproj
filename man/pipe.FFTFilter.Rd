% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipe.FFTFilter.R
\name{pipe.FFTFilter}
\alias{pipe.FFTFilter}
\title{Extracts powers of frequencies in specified channels}
\usage{
pipe.FFTFilter(input, choose, normalize = F)
}
\arguments{
\item{input}{Input pipe}

\item{choose}{DataFrame with columns 'channel' and 'frequency'}

\item{normalize}{If enabled returns percentage of power instead of absolute value}
}
\value{
Row where each element corresponds to row from choose and equals to power of channel
}
\description{
Extracts powers of frequencies in specified channels
}
