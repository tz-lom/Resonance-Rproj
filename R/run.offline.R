run.offline <- function(inputs, blocks, code) {
  
  data <- lapply(inputs, function(si){
    F <- Filter(function(x){
      identical(SI(x), si)
    }, blocks)
    
    if(length(F)){
      do.call(merge, F)
    }else{
      makeEmpty(si)
    }
  })
  
  
  env <- new.env()
  env$input <- function(index){
    if(index>0 && index<=length(data)){
      data[[index]]
    } else {
      stop('Unknown input required')
    }
  }
  
  results <- list()
  
  env$createOutput <- function(out, name){
    results[[name]] <<- out
  }
  
  if(!is.language(code)) code <- parse(text=code)
  
  eval(code, env)
  env$process()
  
  results
}
