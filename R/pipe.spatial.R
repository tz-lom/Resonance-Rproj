
pipe.spatial <- function(input, matrix){
  
  processor(
    input,
    prepare = function(env){
      if(!SI.is.channels(input)) return("input must be 'channels' type");
      if(SI(input)$channels != ncol(matrix)) return("matrix must have exact number of columns as number of input channels");
      
      SI.channels(channels=nrow(matrix), samplingRate = SI(input)$samplingRate)
    },
    online = function(input){
      ret <- input %*% t(matrix)
      attr(ret, 'TS') <- attr(input, 'TS')
      ret
    }
  )
}
