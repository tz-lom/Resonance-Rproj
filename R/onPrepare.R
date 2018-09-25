onPrepare <- function(inputs, code){
  
  .reset_execution_plan()
  
  .execution_plan$inputsData <- lapply(inputs, makeEmpty)
  
  .execution_plan$nextStreamId <- length(inputs)+1
  
  if(is.language(code)){
    eval(code, envir = .execution_plan$env)
  }else{
    eval(parse(text=code), envir = .execution_plan$env)
  }

  if(is.function(.execution_plan$env$process)) {
    .execution_plan$env$process()
  } else {
    stop('Nothing to process')
  }

}
