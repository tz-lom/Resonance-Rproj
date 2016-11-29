pipe.linearApproximation <- function(input, filters){
  
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop("Input must be channels")
      SI.channels(samplingRate = SI(input)$samplingRate, channels = length(filters))
    },
    online = function(data){
      if(nrow(data)==0){
        ret <- matrix(0.0, ncol=length(filters), nrow=0)
        attr(ret, 'TS') <- c()
        return(ret)
      }
      ret <- sapply(filters, function(filter){
        channel <- data[,filter[[1]]]            
        
        pairs <- simplify2array(filter[2:length(filter)])
        
        
        pairs <- cbind(
          
          as.matrix(c(-Inf, round(pairs[2,1]))),
          
          pairs,
          
          as.matrix(c(Inf, round(pairs[2,ncol(pairs)])))
        )
        
        interp1(pairs[1,], pairs[2, ],channel)
      })
      if(!is.matrix(ret)){
        ret <- matrix(ret, nrow=1)
      }
      
      attr(ret, 'TS') <- attr(data, 'TS')
      ret
    })
}