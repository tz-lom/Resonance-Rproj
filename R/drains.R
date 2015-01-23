#' Terminating pipe that records all signal to one big buffer.
#' 
#' @param input Input stream.
#' @return function that returns recorded signal
drain.channelsRecorder <- function(input){
  (input$type=='channels') || stop('input type must be channels')
  
  pointer <- 0L
  buffer <- matrix(0.0, nrow = 2^5, ncol = input$channels)
  
  input$connect(function(db){
    if(nrow(db)+pointer >= nrow(buffer))
    {
      #perform realloc
      tmp <- matrix(0.0, ncol=input$channels, nrow=(nrow(db)+nrow(buffer))*1.5)
      rowsCopy(tmp,0, buffer,0, -1)
      buffer <<- tmp
    }
    rowsCopy(buffer, pointer, db, 0, -1)
    pointer <<- pointer+nrow(db)
  })
  
  # return accessor for recorded data
  function(){
    buffer[1:(pointer),]
  }
}

#' Terminating pipe that records windows to one big buffer.
#' 
#' @param input Input stream.
#' @return function that returns recorded signal
drain.windowRecorder <- function(input){
  (input$type=='window') || stop('input type must be window')
  
  pointer <- 0L
  buffer <- array(0.0, dim=c(input$size, input$channels, 2^5))
  
  input$connect(function(db){
    if(nrow(db)+pointer >= nrow(buffer))
    {
      #perform realloc
      tmp <- array(0.0, dim=c(input$size, input$channels, dim(buffer)[3]*1.5))
      tmp[,,1:dim(buffer)[3]] <- buffer
      buffer <<- tmp
    }
    buffer[,, pointer] <<- db
    pointer <<- pointer+1
  })
  
  # return accessor for recorded data
  function(){
    buffer[,, 1:(pointer)]
  }
}

#' Stores data as DataBlocks
#' 
#' Usefull for debugging and testing purposes.
#' Use functions `all_received` and `last_received` from returned object to access data.
#' 
#' @param input Any block.processor
drain.debug <- function(input){
  bp <- list()
  
  lr <- NULL
  ar <- NULL
  
  bp$last_received <- function() lr
  bp$all_received <- function() ar
  bp$reset <- function(){
    lr<<- NULL
    ar <<- NULL
  }
  
  input$connect(function(db){
    lr <<- db
    ar <<- c(ar, list(db)) # @todo: need to preallocate list
  })
  bp
}

drain.windowRecorder <- function(input){

  pointer <- 1
  buffer <- array(dim=c(input$size, input$channels, 128))
  
  input$connect(function(db){
    if(pointer+1 > dim(buffer)[[3]])
    {
      #perform realloc
      buffer <<- array(buffer, dim=c(input$size, input$channels, pointer*1.5))
    }
    buffer[,,pointer] <<- db
    pointer <<- pointer+1
  })
  
  # return accessor for recorded data
  function(){
    buffer[,,1:(pointer-1)]
  }
}

drain.terminator <- function(input){
  input$connect(function(db){ db; })
}