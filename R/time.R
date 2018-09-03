TS <- function(x){
  attr(x, 'TS', TRUE)
}

`TS<-` <- function(x, value){
  attr(x, 'TS') <- value
  x
}
