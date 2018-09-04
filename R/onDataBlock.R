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
  data <- DB.event(.globals$inputs[[id]], timestamp, msg)
  onDataBlock(id, list(msg))
}

onDataBlock.channels <- function(id, vector, timestamp){
  data <- DB.channels(.globals$inputs[[id]], timestamp, vector)
  onDataBlock(id, data)
}

onDataBlock.epoch <- function(id, vector, timestamp){
  data <- DB.epoch(.globals$inputs[[id]], timestamp, vector)
  onDataBlock(id, data)
}

onDataBlock.window <- function(id, vector, samples, timestamp){
  
}
