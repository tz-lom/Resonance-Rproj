
# queue to resonance

globals <- new.env()

globals$queue <- list()   
globals$compileCallbacks <- TRUE

compileCallbacks <- function(bool=TRUE){
  globals$compileCallbacks <- any(bool)
}

addToQueue = function(cmd, ...){
  globals$queue <- append(globals$queue, list(list(
    cmd=cmd,
    args = list(...)
    )))
}



popQueue <- function(){
  ret <- globals$queue
  globals$queue <- list()
  ret
}

globals$inputStreams <- list()
globals$outputStreams <- list()

clearStreams <- function(){
  globals$inputStreams <- list()
  globals$outputStreams <- list()
}

registerInput <- function(name, channels, samplingRate){
  bp <- block.processor("input", channels=channels, samplingRate=samplingRate, name=name)
  
  class(bp) <- c("block.processor","stream.input")
  globals$inputStreams <- append(globals$inputStreams, list(bp))
}

createOutput <- function(name, type, input){
  bp <- switch(input$type,
    'channels' = list(
      samplingRate = input$samplingRate,
      channels = input$channels
      ),
    'input' = list(
      samplingRate = input$samplingRate,
      channels = input$channels
    ),
    'window' = list(
      samplingRate = input$samplingRate,
      channels = input$channels,
      )
  )
  
  
  bp$name <- name;
  bp$typeName <- type;
  bp$type <- 'output';
  
  id = length(globals$outputStreams)+1
  
  input$connect(function(block){
    addToQueue("sendBlockToStream",
                  id = id,
                  data = block)
  })
  
  addToQueue("createOutputStream",
             id = id,
             name = name,
             typeName = type,
             samplingRate = bp$samplingRate,
             channels = bp$channels
  )
  
  class(bp) <- c("block.processor","stream.output")
  globals$outputStreams <- append(globals$outputStreams, list(bp))
  bp
}

input <- function(id){
  globals$inputStreams[[id]]
}

output <- function(id){
  globals$outputStreams[[id]]
}

DataBlock <- function(df, timestamp){
  if(!is.null(attr(timestamp, 'timestamp')))
  {
    attr(df, 'timestamp') <- as.integer64(attr(timestamp, 'timestamp'))
  }
  else
  {
    attr(df,'timestamp') <- as.integer64(timestamp)
  }
  df
}

onStart <- function(){
# called on start of recording
}

onStop <- function(){
  
}

onPrepare <- function(){
  clearStreams();
  popQueue();
}

blockReceived <- function(streamId, samples, timestamp, data){
  stream <- globals$inputStreams[[streamId]];
  df <- matrix(
    data, 
    byrow = T,
    nrow = samples
  )
  
  block <- DataBlock(df, timestamp)
  stream$emit(block)
}

block.processor <- function(.firstArg, ...){

  if(inherits(.firstArg,'block.processor')){
    bp <- .firstArg
    l <- list(...)
    for(name in names(l)){
      bp[[name]] <- l[[name]]
    }
  }else{
    bp <- list(...)
    bp$type <- .firstArg
  }
  
  callback <- NULL
  
  bp$emit <- function(data){
    callback(data);
  }
  
  bp$connect <- function(cb){
    if(is.null(callback)){
      callback <<- if(globals$compileCallbacks) cmpfun(cb,options = list(optimize=3)) else cb
    }else{
      snap <- callback
      callback <<- cmpfun(function(data){ cb(data); snap(data); }, options=list(optimize=3))
    }
  }
  
  class(bp) <- 'block.processor'
  bp
}




