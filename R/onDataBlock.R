onDataBlock <- function(id, data) {

  X <- .globals$inputsData[[id]]
  .globals$inputsData[[id]] <- data

  tryCatch(
    .globals$env$process()
  , error=function(e) {
    print(e)
    print(.traceback())
  })

  .globals$inputsData[[id]] <- X
}

onDataBlock.message <- function(id, msg, timestamp){
  attr(msg, 'TS') <- timestamp
  onDataBlock(id, list(msg))
}

onDataBlock.channels <- function(id, vector, samples, timestamp){
  data <- matrix(vector, nrow=samples, byrow = T)
  attr(data, 'TS') <- seq(to=timestamp, by=1E6/.globals$inputs[[id]]$samplingRate, length.out=samples)
  SI(data) <- .globals$inputs[[id]]
  onDataBlock(id, data)
}

onDataBlock.epoch <- function(id, vector, samples, timestamp){
  data <- matrix(vector, nrow=samples, byrow = T)
  attr(data, 'TS') <- seq(to=timestamp, by=1E6/.globals$inputs[[id]]$samplingRate, length.out=samples)
  SI(data) <- .globals$inputs[[id]]
  onDataBlock(id, data)
}

onDataBlock.window <- function(id, vector, samples, timestamp){
  
}
