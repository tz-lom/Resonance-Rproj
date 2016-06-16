pipe.diff <- function(input, initial = 0){
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop("Input must be channels")
      
      env$init <- matrix(initial, ncol=SI(input)$channels, nrow=1)
      
      SI(input)
    },
    online = function(input){
      
      ret <- diff(rbind(init, input))
      attr(ret, 'TS') <- attr(input, 'TS')
      init <<- input[nrow(input),]
      
      ret
    }
  )
}