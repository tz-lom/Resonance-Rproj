run.offline <- function(data, code) {
  
  env <- new.env()
  env$input <- function(index){
    if(index>0 && index<=length(data)){
      data[[index]]
    } else {
      stop('Unknown input required')
    }
  }
  
  results <- list()
  
  env$createOutput <- function(name, out){
    results[[name]] <<- out
  }
  
  eval(code, env)
  env$process()
  
  results
}