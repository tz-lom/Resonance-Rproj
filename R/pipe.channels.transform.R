pipe.channels.transform <- function(input, outputChannels, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop("Input must be channels")
      
      SI.channels(channels = outputChannels, samplingRate = SI(input)$samplingRate)
    },
    online = function(input){
      ret <- transform(input)
      attr(ret, 'TS') <- attr(input, 'TS')
      ret
    })
}