% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross.windowizeByEvents.R
\name{cross.windowizeByEvents}
\alias{cross.windowizeByEvents}
\title{Split data stream to windows using event stream}
\usage{
cross.windowizeByEvents(data, events, windowSize, shift = 0,
  dropLateEvents = TRUE, lateTime = 10)
}
\arguments{
\item{data}{Data stream}

\item{events}{Events stream}

\item{windowSize}{Size of resulting window}

\item{dropLateEvents}{Don't expand buffer infinitely, lateTime controls buffer size and any events that arrive with timestamp earlier than last data timestamp-lateTime potentially can be dropped}

\item{lateTime}{- allowed delay for events (in seconds)}

\item{backBuffer}{Size of buffer for data, may be increased in case of big delay in events arrival}
}
\value{
window
}
\description{
Split data stream to windows using event stream
}
