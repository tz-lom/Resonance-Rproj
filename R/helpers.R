SI <- function(input){
  attr(input, '.StreamInfo', TRUE)
}

`SI<-` <- function(x, name=NULL, value){
  if(is.null(name)) {
    attr(x, '.StreamInfo') <- value
  } else {
    attr(x, '.StreamInfo')[[name]] <- value
  }
  x
}

multipleStreams <- function(...){
  l <- list(...)
  class(l) <- 'multipleStreams'
  l
}

#' Wraps all necessary handlers to pipeline function
#'
#'
processor <- function(
  ...,
  prepare,
  online,
  offline=NULL
  ){

  inputs <- list(...)
  
  env <- new.env(parent=environment(online))
  si <- prepare(env)
  
  if(is.character(si)) stop(si)

  if(isTRUE(SI(inputs[[1]])$online)){
    # prepare online processing
    environment(online) <- env
    
    
    
    if( class(si)=='multipleStreams' ){
      si <- lapply(si, function(si){
        si$id <- .execution_plan$nextStreamId
        .execution_plan$nextStreamId <<- .execution_plan$nextStreamId+1
        si$online <- TRUE
        si
      })
      result <- lapply(si, makeEmpty)
    } else {
      si$id <- .execution_plan$nextStreamId
      .execution_plan$nextStreamId <- .execution_plan$nextStreamId+1
      si$online <- TRUE
      result <- makeEmpty(si)
      si <- list(si)
    }
    
    .execution_plan$plan <- c(
      .execution_plan$plan,
      list(list(
        online=online,
        outputs=si,
        inputs=lapply(inputs, SI),
        call=sys.call(-1)
      )))
    
    return(result)
  } else {
    result <- if(is.null(offline)){
      environment(online) <- env
      online(...)
    } else {
      environment(offline) <- env
      offline(...)
    }
    
    if(class(result)=='multipleStreams'){
      result <- mapply(function(stream, si){
        SI(stream) <- si
        stream
      }, result, si)
    } else {
      SI(result) <- si
    }
    return(result);
  }
  # else
  # {
  #   # perform online processing
  #   result <- do.call(.processor_cache[[hash]], inputs)
  #   
  #   if(class(result)=='multipleStreams'){
  #     result <- mapply(function(stream, si){
  #       SI(stream) <- si
  #       stream
  #     }, result, .processor_si_cache[[hash]])
  #   } else {
  #     SI(result) <- .processor_si_cache[[hash]]
  #   }
  #   
  #   return(result)
  # }
}
