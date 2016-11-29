pipe.transform.channels <- function(input, outputChannels, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop("Input must be channels")
      
      SI.channels(channels = outputChannels, samplingRate = SI(input)$samplingRate)
    },
    online = function(input){
      if(nrow(input)==0){
        ret <- matrix(0.0, ncol=outputChannels, nrow=0)
        attr(ret, 'TS') <- c()
        return(ret)
      }
      
      ret <- transform(input)
      attr(ret, 'TS') <- attr(input, 'TS')
      ret
    })
}