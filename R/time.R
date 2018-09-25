TS <- function(x){
  attr(x, 'TS', TRUE)
}

`TS<-` <- function(x, value){
  attr(x, 'TS') <- nanotime(value)
  x
}

timeoption2samples <- function(si, x){
  if('seconds' %in% class(x)){
    as.numeric(x)*si$samplingRate
  } else {
    x
  }
}

timeoption2seconds <- function(si, x){
  if('seconds' %in% class(x)){
    x
  } else {
    x / si$samplingRate  
  }
}

timeoption2ts <- function(si, x){
  if('seconds' %in% class(x)){
    nanotime(as.numeric(x)*1E9)
  } else {
    nanotime(x*1E9/si$samplingRate)
  }
}

samples <- function(x){
  class(x) <- c(class(x), 'samples')
  x
}

seconds <- function(x){
  class(x) <- c(class(x), 'seconds')
  x
}

timeInterval <- function(x, time){
  time <- na.omit(time)
  if(time[[1]]>x) return(0)
  if(time[[length(time)]]<x) return(Inf)
  
  low <- 1
  high <- length(time)
  
  while(TRUE){
    if(high-low <= 1) return(low)
    
    boundary <- ceiling((high+low)/2)
    if(x > time[[boundary]]){
      low <- boundary
    } else {
      high <- boundary
    }
  }
}
