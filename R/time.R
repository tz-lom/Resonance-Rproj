TS <- function(x){
  attr(x, 'TS', TRUE)
}

`TS<-` <- function(x, value){
  attr(x, 'TS') <- value
  x
}

timeoption2samples <- function(si, x){
  if('seconds' %in% class(x)){
    x*si$samplingRate
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
    nanotime(x*1E9)
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
