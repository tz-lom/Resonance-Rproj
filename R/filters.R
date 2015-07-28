
pipe.decimate <- function(input, inc, dec, coef){  
  input$type=='channels' || stop('Must be channels in input')
  
  bp <- block.processor(input, samplingRate=input$samplingRate*inc/dec)
  
  decs <- list()
  
  for(i in 1:input$channels)
    decs[[i]] <- upFirDown(inc, dec, coef)
  
  input$connect(function(db){
    res <- decs[[1]](db[,1])
    
    result <- matrix(nrow = length(res), ncol=ncol(db))
    result[,1] <- res
    
    if(ncol(db)>1) {
      for(i in 2:ncol(db)){
        result[,i] <- decs[[i]](db[,i])
      }
    }
    
    bp$emit(DataBlock(result, db))
  })
  
  bp
}

to.channels <- function(input){
  if(input$type == 'channels') return(input)
  
  if(input$type == 'window'){
    bp <- block.processor(input, type="channels", channels=input$channels)
    input$connect(function(x) bp$emit(x))
    return(bp)
  }
  if(input$type == 'input'){
    bp <- block.processor(input, type="channels", channels=input$channels)
    input$connect(function(db) bp$emit(db))
    return(bp)
  }
}

pipe.rescale <- function(input, shift, mult){
  bp <- block.processor(input)
  
  input$connect(function(db){
    bp$emit((db-shift)*mult)
  })
  
  bp
}

encapsulateProcessors <- function(input, output){
  bp <- output
  bp$emit <- input$emit
  bp
}

#' Simple filter pipe that applies high-pass, low-pass and notch filters
#'
#' @param input Pipe connected to
#' @param lowFreq Frequency of high-pass filter
#' @param highFreq Frequency of low-pass filter
#' @param notch Frequency for notch filter
#' @return Constructed pipe
pipe.bandFilter <- function(input, lowFreq, highFreq, notch, order=2){
  out <- input
  if(lowFreq){
    out <- pipe.applyFilter(
      out,
      butter(order, lowFreq/SI(input)$samplingRate*2, type = 'high')
    )
  }
  if(highFreq){
    out <- pipe.applyFilter(
      out,
      butter(order, highFreq/SI(input)$samplingRate*2, type='low')
    )
  }
  if(notch){
    out <- pipe.applyFilter(
      out,
      butter(order, c(notch-1, notch+1)/SI(input)$samplingRate*2, type = 'stop')
    )
  }
  
  out
}

#' Combine pipes to chain, returns it's output
#'
#' Allows nice descriptive specification of pipeline.
#' Iterates over arguments and constructs pipeline where output from \emph{n-1} filter directed to input of \emph{n} filter.
#'
#' @param ... List of pipes, I recommend to skip first argument for pipe
#' @param .env Environment for evaluating pipes, for experts only
#' @return Output of last pipe
#' 
#' @examples
#' pipeline(
#'  pipe.applyFilter(input, f1),
#'  pipe.applyFilter(, f2)
#' )
pipeline <- function(... , .env = parent.frame()){
  arguments <- eval(substitute(alist(...)))
  
  piped <- NULL
  
  piped <- eval(arguments[[1]], envir = .env) 
  arguments <- arguments[-1]
  
  for(filter in arguments){
    is.call(filter) || stop('only filters must be in pipeline')
    
    filter[[2]] <- piped
    
    piped <- eval(filter, envir = .env)
  }
  
  piped
}
