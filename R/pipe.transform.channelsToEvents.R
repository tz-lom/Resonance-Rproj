pipe.transform.channelsToEvents <- function(input, transform){
  processor(
    input,
    prepare = function(env){
      SI.is.channels(input) || stop("Input must be channels")
      SI.event()
    },
    online = function(input){
      if(nrow(input)>0) {
        TS <- attr(input, 'TS')
        ret <- vector("list", length = nrow(input))
        for(i in 1:nrow(input)){
          ret[[i]] <- transform(input[i,])
          attr(ret[[i]], 'TS') <- TS[[i]]
        }
        ret
      } else {
        list()
      }
    }
  )
}