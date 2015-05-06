#' 
#'
pipe.references <- function(input, refs){
  
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input)=='channels' || stop('Must be channels in input')
      
      cout <- 1:SI(input)$channels
      env$cout <- cout[-refs]
      
      SI.channels(samplingRate = SI(input)$samplingRate, channels = SI(input)$channels - length(refs))
    },
    online = function(input){
      rdata <- input[,refs, drop=F]
      out <- input[,cout, drop=F]
      
      rdata <- apply(rdata,1, mean)
      
      ret <- sweep(out,1,rdata)
      attr(ret, 'TS') <- attr(input, 'TS')
      ret
    })
}