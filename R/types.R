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
  data <- if(is.matrix(vector)) vector else matrix(vector, ncol=SI$channels, byrow = T)
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
  window <- if(is.matrix(vector)) vector else matrix(vector, nrow=SI$samples, byrow = T)
  # assert_that(ncol(window)==SI$channels)
  # assert_that(nrow(window)==SI$samples)
  attr(window, 'TS') <- timestamp
  ret <- list(window)
  class(ret) <- c('DB.window', 'matrix')
  SI(ret) <- SI
  ret
}

DB.epoch <- function(SI, timestamp, vector){
  data <- if(is.matrix(vector)) vector else matrix(vector, ncol=SI$channels, byrow = T)
  if(length(timestamp)==1){
    attr(data, 'TS') <- seq(to=timestamp, by=1E6/SI$samplingRate, length.out=nrow(data))
  } else {
    attr(data, 'TS') <- timestamp
  }
  ret <- list(data)
  class(ret) <- c('DB.epoch','matrix')
  SI(ret) <- SI
  ret
}

DB.something <- function(SI, timestamp, data){
  do.call(paste0('DB.', SI$type), list(SI, timestamp, data))
}

makeEmpty.window <- function(si){
  ret <- list()
  SI(ret) <- si
  class(ret) <- c('DB.window','matrix')
  ret
}

makeEmpty.channels <- function(si){
  ret <- matrix(0.0, nrow=0, ncol=si$channels)
  SI(ret) <- si
  class(ret) <- c('DB.channels','matrix')
  ret
}

makeEmpty.event <- function(si){
  ret <- list()
  SI(ret) <- si
  class(ret) <- 'DB.event'
  ret
}

makeEmpty.epoch <- function(si){
  ret <- list()
  SI(ret) <- si
  class(ret) <- c('DB.epoch','matrix')
  ret
}

makeEmpty <- function(si){
  do.call(paste("makeEmpty", si$type, sep="."), list(si))
}

DBcombine <- function(...){
  do.call(paste("DBcombine", class(..1)[[1]], sep="."), list(...))
}

DBcombine.DB.channels <- function(x, ...){
  ret <- rbind(x, ...)
  SI(ret) <- SI(x)
  ts <- c(attr(x, 'TS'), do.call(c, lapply(list(...), attr, 'TS')))
  attr(ret, 'TS') <- ts
  class(ret) <- class(x)
  ret
}

DBcombine.DB.event <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
  class(ret) <- class(x)
  ret
}

DBcombine.DB.window <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
  class(ret) <- class(x)
  ret
}

DBcombine.DB.epoch <- function(x, ...){
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
