run.offline <- function(inputs, blocks, code) {
  
  data <- lapply(inputs, function(si){
    do.call(
      merge,
      Filter(function(x){
        identical(SI(x), si)
      }, blocks)
    )
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
