SI.channels <- function(channels, samplingRate){
  list(
    type='channels',
    channels=channels,
    samplingRate=samplingRate
  )
}

SI.window <- function(channels, samples, samplingRate){
  list(
    type='window',
    channels = channels,
    samples = samples,
    samplingRate = samplingRate
  )
}

SI.event <- function(){
  list(
    type='event'
  )
}

DB.event <- function(SI, timestamp, message){
  attr(message, 'TS') <- timestamp
  ret <- list(message)
  SI(ret) <- SI
  class(ret) <- 'DB.event'
  ret
}

DB.channels <- function(SI, timestamp, vector){
  data <- matrix(vector, ncol=SI$channels, byrow = T)
  attr(data, 'TS') <- seq(to=timestamp, by=1E6/SI$samplingRate, length.out=nrow(data))
  class(data) <- c('DB.channels','matrix')
  SI(data) <- SI
  data
}

DB.window <- function(SI, timestamp, vector, samples){
  window <- matrix(vector, nrow=samples, byrow = T)
  attr(window, 'TS') <- timestamp
  ret <- list(window)
  class(ret) <- c('DB.window', 'matrix')
  SI(ret) <- SI
  ret
}

merge.DB.channels <- function(x,y,...){
  ret <- rbind(x, y, ...)
  SI(ret) <- SI(x)
  ts <- c(attr(x, 'TS'), attr(y, 'TS'), sapply(list(...), attr, 'TS'))
  attr(ret, 'TS') <- ts
  class(ret) <- class(x)
  ret
}

merge.DB.event <- function(x,y,...){
  ret <- c(x,y,...)
  SI(ret) <- SI(x)
  class(ret) <- class(x)
  ret
}

merge.DB.window <- function(x,y,...){
  ret <- c(x,y,...)
  SI(ret) <- SI(x)
  class(ret) <- class(x)
  ret
}

SI.is.channels <- function(input){
  r <- SI(input)$type=='channels'
  all(r, length(r)>0)
}

SI.is.window <- function(input){
  r <- SI(input)$type=='window'
  all(r, length(r)>0)
}

SI.is.event <- function(input){
  r <- SI(input)$type == 'event'
  all(r, length(r)>0)
}