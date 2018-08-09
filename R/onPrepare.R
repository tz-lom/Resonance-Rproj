onPrepare <- function(inputs, code){

    Resonance:::.reset_processor_cache()

  resetGlobals();

  if(is.language(code)){
    eval(code, envir = .globals$env)
  }else{
    eval(parse(text=code), envir = .globals$env)
  }

  .globals$inputsData <- lapply(inputs, makeEmpty)
  .globals$inputs <- inputs

  .globals$env$input <- function(id){
    .globals$inputsData[[id]]
  }


  if(is.function(.globals$env$process)) {
    .globals$env$process()
  } else {
    stop('Nothing to process')
  }

}
