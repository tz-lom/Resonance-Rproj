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

SI.epoch <- function(channels, samplingRate){
  list(
    type='epoch',
    channels = channels,
    samplingRate = samplingRate
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
  data <- matrix(as.double(vector), ncol=SI$channels, byrow = T)
  if(length(timestamp)==1){
    attr(data, 'TS') <- seq(to=timestamp, by=1E6/SI$samplingRate, length.out=nrow(data))
  } else {
    attr(data, 'TS') <- timestamp
  }
  class(data) <- c('DB.channels','matrix')
  SI(data) <- SI
  data
}

DB.window <- function(SI, timestamp, vector){
  window <- matrix(vector, nrow=SI$samples, byrow = T)
  attr(window, 'TS') <- timestamp
  ret <- list(window)
  class(ret) <- c('DB.window', 'matrix')
  SI(ret) <- SI
  ret
}

DB.epoch <- function(SI, timestamp, vector){
  data <- matrix(vector, ncol=SI$channels, byrow=T)
  if(length(timestamp)==1){
    attr(data, 'TS') <- seq(to=timestamp, by=1E6/SI$samplingRate, length.out=nrow(data))
  } else {
    attr(data, 'TS') <- timestamp
  }
  ret <- list(data)
  class(ret) <- c('DB.epoch','matrix')
  SI(ret) <- SI
  data
}

DB.something <- function(SI, timestamp, data){
  do.call(paste0('DB.', SI$type), list(SI, timestamp, data))
}

makeEmpty.channels <- function(si){
  ret <- matrix(0.0, nrow=0, ncol=si$channels)
  SI(ret) <- si
  ret
}

makeEmpty.event <- function(si){
  ret <- list()
  SI(ret) <- si
  ret
}

makeEmpty.epoch <- function(si){
  ret <- list()
  SI(ret) <- si
  ret
}

makeEmpty <- function(si){
  do.call(paste("makeEmpty", si$type, sep="."), list(si))
}

merge.DB.channels <- function(x, ...){
  ret <- rbind(x, ...)
  SI(ret) <- SI(x)
  ts <- c(attr(x, 'TS'), do.call(c, lapply(list(...), attr, 'TS')))
  attr(ret, 'TS') <- ts
  class(ret) <- class(x)
  ret
}

merge.DB.event <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
  class(ret) <- class(x)
  ret
}

merge.DB.window <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
  class(ret) <- class(x)
  ret
}

merge.DB.epoch <- function(x, ...){
  ret <- c(x, ...)
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

SI.is.epoch <- function(input){
  r <- SI(input)$type == 'epoch'
  all(r, length(r)>0)
}