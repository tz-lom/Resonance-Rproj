SI.channels <- function(channels, samplingRate, id=-1){
  c(list(
    type='channels',
    channels=as.integer(channels),
    samplingRate=as.numeric(samplingRate)
  ), if(id==-1) list() else list(id=id))
}

SI.window <- function(channels, samples, samplingRate, id=-1){
  c(list(
    type='window',
    channels = as.integer(channels),
    samples = as.integer(samples),
    samplingRate = as.numeric(samplingRate)
  ), if(id==-1) list() else list(id=id))
}

SI.event <- function(id=-1){
  c(list(
    type='event'
  ), if(id==-1) list() else list(id=id))
}

SI.epoch <- function(channels, samplingRate, id=-1){
  c(list(
    type='epoch',
    channels = as.integer(channels),
    samplingRate = as.numeric(samplingRate)
  ), if(id==-1) list() else list(id=id))
}

SI.outputStream <- function(name, id){
  list(
    type='outputStream',
    name=name,
    id=id
  )
}

DB.event <- function(SI, timestamp, message){
  TS(message) <- nanotime(timestamp)
  ret <- list(message)
  SI(ret) <- SI
  ret
}

DB.channels <- function(SI, timestamp, vector){
  data <- if(is.matrix(vector)) vector else matrix(as.numeric(vector), ncol=SI$channels, byrow = T)
  if(length(timestamp)==1){
    TS(data) <- nanotime(seq(to=as.integer64(timestamp), by=1E9/SI$samplingRate, length.out=nrow(data)))
  } else {
    TS(data) <- nanotime(timestamp)
  }
  SI(data) <- SI
  data
}

DB.window <- function(SI, timestamp, vector){
  data <- if(is.matrix(vector)) vector else matrix(as.numeric(vector), nrow=SI$samples, byrow = T)
  # assert_that(ncol(data)==SI$channels)
  # assert_that(nrow(data)==SI$samples)
  if(length(timestamp)==1){
    TS(data) <- nanotime(seq(to=as.integer64(timestamp), by=1E9/SI$samplingRate, length.out=nrow(data)))
  } else {
    TS(data) <- nanotime(timestamp)
  }
  ret <- list(data)
  SI(ret) <- SI
  ret
}

DB.epoch <- function(SI, timestamp, vector){
  data <- if(is.matrix(vector)) vector else matrix(as.numeric(vector), ncol=SI$channels, byrow = T)
  if(length(timestamp)==1){
    TS(data) <- nanotime(seq(to=as.integer64(timestamp), by=1E9/SI$samplingRate, length.out=nrow(data)))
  } else {
    TS(data) <- nanotime(timestamp)
  }
  ret <- list(data)
  SI(ret) <- SI
  ret
}

DB.something <- function(SI, timestamp, data){
  do.call(paste0('DB.', SI$type), list(SI, timestamp, data))
}

makeEmpty.window <- function(si){
  ret <- list()
  SI(ret) <- si
  ret
}

makeEmpty.channels <- function(si){
  ret <- matrix(0.0, nrow=0, ncol=si$channels)
  TS(ret) <- nanotime(c())
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

makeEmpty.outputStream <- function(si){
  ret <- list()
  SI(ret) <- si
  ret
}

makeEmpty <- function(si){
  do.call(paste("makeEmpty", si$type, sep="."), list(si))
}

DBcombine <- function(...){
  stopifnot(length(unique(lapply(list(...), SI)))==1)
  do.call(paste("DBcombine", SI(..1)$type, sep="."), list(...))
}

DBcombine.channels <- function(x, ...){
  ret <- rbind(x, ...)
  SI(ret) <- SI(x)
  ts <- c(TS(x), do.call(c, lapply(list(...), TS)))
  TS(ret) <- ts
  ret
}

DBcombine.event <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
  ret
}

DBcombine.window <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
  ret
}

DBcombine.epoch <- function(x, ...){
  ret <- c(x, ...)
  SI(ret) <- SI(x)
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
